#' Stage weights of the sample units
#'
#' Step of stage-wise raising (see [raisingScheme()]). For every stage of the
#' scheme (VS, FT, FO) it builds the unit table ([getUnitTable()]), resolves
#' the total and value chosen by the user (RDBES column, function or
#' [externalAux()] table), and computes a factor per sampled unit within
#' parent x stratum ([computeStageFactors()]). The factors are multiplied down
#' the hierarchy, so every unit gets a cumulative weight, and every
#' lowest-level SA gets `w = f_VS * f_FT * f_FO` (SS is always 1).
#'
#' Selection methods are not used for the factors. They are logged per stage.
#'
#' **Zeros and non-response.** A sampled haul (`FOsamp` not `"N"`) can be:
#' * with an included sample of the species: its fish count,
#' * without any sample of the species: zero catch, logged as an assumption
#'   (the species is assumed to be on the species list),
#' * with a recorded zero (an SA with total weight 0, see
#'   [getSampleUnits()]): zero catch, logged as information,
#' * with samples of the species that are all excluded (for example lower
#'   hierarchy D, missing weights, no FM or BV): no usable information, so
#'   it is treated as not sampled (non-response) and left out of the number
#'   of sampled hauls.
#'
#' A trip without any sampled haul left, and a vessel without any sampled
#' trip left, are treated as not sampled in the same way. All of this is
#' logged.
#'
#' @param RDBESDataObj An `RDBESDataObject` of hierarchy 1.
#' @param sampleUnits Output of [getSampleUnits()].
#' @param scheme A scheme made with [raisingScheme()].
#' @param verbose Logical. Print log entries as messages. Default `FALSE`.
#'
#' @return `sampleUnits` with the columns `f_VS`, `f_FT`, `f_FO`, `w`, `DEid`
#'   and `DEstratumName` added. Attributes: `"unitWeights"`, a list with one
#'   table per stage (unit id, ancestor ids, `factor` and cumulative weight
#'   `wUnit` of every sampled unit), `"scheme"` and `"ratioLog"`.
#' @seealso [raisingScheme()], [raiseToDE()]
#' @export
getStageWeights <- function(RDBESDataObj, sampleUnits, scheme,
                            verbose = FALSE) {
  if (!inherits(scheme, "RDBESRaisingScheme")) {
    stop("scheme must be made with raisingScheme().", call. = FALSE)
  }
  step <- "getStageWeights"
  log <- newRatioLog()
  chain <- getTablesInRDBESHierarchy(scheme$hierarchy,
                                     includeOptTables = FALSE,
                                     includeLowHierTables = FALSE,
                                     includeTablesNotInSampHier = FALSE)
  stages <- names(scheme$stages)
  lowest <- stages[length(stages)]
  lowId <- paste0(lowest, "id")
  unitWeights <- list()
  prevW <- NULL

  # Unit tables of all stages
  unitTables <- list()
  for (tb in stages) {
    unitTables[[tb]] <- getUnitTable(
      RDBESDataObj, tb,
      fields = stageFieldsNeeded(RDBESDataObj, tb, scheme$stages[[tb]]),
      verbose = verbose)
    log <- rbind(log, getRatioLog(unitTables[[tb]]))
  }

  # Non-response at the lowest stage: a sampled haul whose samples of the
  # species are all excluded (and none is a recorded zero) gives no usable
  # information. It is treated as not sampled, not as zero catch. A haul
  # without any sample of the species stays sampled with zero catch.
  su0 <- sampleUnits
  withSA <- unique(su0[[lowId]])
  usable <- unique(su0[include == TRUE |
                         excludeReason == zeroSampleReason()][[lowId]])
  nonResp <- setdiff(withSA, usable)
  lowUnits <- unitTables[[lowest]]
  drop <- lowUnits$sampled & lowUnits[[lowId]] %in% nonResp
  if (any(drop)) {
    lowUnits[drop, sampled := FALSE]
    unitTables[[lowest]] <- lowUnits
    log <- addToRatioLog(log, step, "exclusion",
      paste0("Sampled ", lowest, " units whose samples of the species were ",
             "all excluded: treated as not sampled (non-response)"),
      sum(drop), verbose)
  }
  partly <- intersect(unique(su0[include == TRUE][[lowId]]),
                      unique(su0[include == FALSE &
                                   excludeReason != zeroSampleReason()][[lowId]]))
  if (length(partly) > 0) {
    log <- addToRatioLog(log, step, "warning",
      paste0(lowest, " units with both included and excluded samples of the ",
             "species: the excluded part counts as zero"),
      length(partly), verbose)
  }

  # A unit stays sampled only if at least one unit below it is sampled
  if (length(stages) > 1) {
    for (k in rev(seq_len(length(stages) - 1))) {
      tb <- stages[k]
      idCol <- paste0(tb, "id")
      below <- unitTables[[stages[k + 1]]]
      keepIds <- below[sampled == TRUE][[idCol]]
      u <- unitTables[[tb]]
      dropUp <- u$sampled & !u[[idCol]] %in% keepIds
      if (any(dropUp)) {
        u[dropUp, sampled := FALSE]
        unitTables[[tb]] <- u
        log <- addToRatioLog(log, step, "exclusion",
          paste0(tb, " units without any sampled ", stages[k + 1],
                 " unit left: treated as not sampled (non-response)"),
          sum(dropUp), verbose)
      }
    }
  }

  for (tb in stages) {
    s <- scheme$stages[[tb]]
    idCol <- paste0(tb, "id")
    parentId <- paste0(chain[match(tb, chain) - 1], "id")
    units <- unitTables[[tb]]
    # Only units under a sampled parent are raised
    k <- match(tb, stages)
    if (k > 1) {
      parentsKept <- unitTables[[stages[k - 1]]][sampled == TRUE][[parentId]]
      units <- units[units[[parentId]] %in% parentsKept]
    }
    stageStep <- paste0(step, " (", tb, ")")

    totalCol <- NULL
    valueCol <- NULL
    if (!s$method %in% c("none", "prob")) {
      r <- resolveRaisingArg(units, s$total, ".total", stageStep, verbose)
      units <- r$units
      log <- rbind(log, r$log)
      totalCol <- ".total"
    }
    if (s$method != "none" && !identical(s$value, 1)) {
      r <- resolveRaisingArg(units, s$value, ".value", stageStep, verbose)
      units <- r$units
      log <- rbind(log, r$log)
      valueCol <- ".value"
    }
    by <- parentId
    if (!is.null(s$strata)) {
      if (!s$strata %in% names(units)) {
        stop(stageStep, ": strata column '", s$strata, "' not found.",
             call. = FALSE)
      }
      by <- c(by, s$strata)
    }
    engine <- if (s$method %in% c("none", "prob")) s$method else "ratio"
    numSamp <- if (s$method == "count" &&
                   paste0(tb, "numSamp") %in% names(units)) {
      paste0(tb, "numSamp")
    } else {
      NULL
    }
    f <- computeStageFactors(units, id = idCol, by = by, total = totalCol,
                             value = valueCol, method = engine,
                             sampled = "sampled", numSamp = numSamp,
                             step = stageStep, verbose = verbose)
    log <- rbind(log, getRatioLog(f))

    # Selection methods are only logged
    smCol <- paste0(tb, "selectMeth")
    if (smCol %in% names(units)) {
      sm <- unique(stats::na.omit(units[sampled == TRUE][[smCol]]))
      log <- addToRatioLog(log, stageStep, "assumption",
        paste0("Factors from method '", s$method, "', selection method(s) ",
               paste(sm, collapse = ", "), " not used"),
        sum(units$sampled), verbose)
    }

    # Cumulative weight: own factor times the weight of the parent unit
    keep <- intersect(c(idCol, parentId, "VSid", "FTid", "SDid", "DEid",
                        "DEstratumName"), names(units))
    uw <- merge(units[sampled == TRUE, keep, with = FALSE],
                f[, c(idCol, "factor"), with = FALSE], by = idCol)
    if (is.null(prevW)) {
      uw[, wUnit := factor]
    } else {
      uw <- merge(uw, prevW[, c(parentId, "wUnit"), with = FALSE],
                  by = parentId)
      uw[, wUnit := factor * wUnit]
    }
    unitWeights[[tb]] <- uw
    prevW <- uw
  }

  # Weights of the lowest-level SA
  su <- data.table::copy(sampleUnits)
  for (tb in stages) {
    idCol <- paste0(tb, "id")
    uw <- unitWeights[[tb]]
    su[, (paste0("f_", tb)) := uw$factor[match(su[[idCol]], uw[[idCol]])]]
  }
  lw <- unitWeights[[lowest]]
  m <- match(su[[lowId]], lw[[lowId]])
  su[, `:=`(w = lw$wUnit[m], DEid = lw$DEid[m],
            DEstratumName = lw$DEstratumName[m])]
  noW <- su[include == TRUE & is.na(w), .N]
  if (noW > 0) {
    stop(step, ": ", noW, " included sample(s) sit under units that are not ",
         "sampled (XXsamp = \"N\" or no records below). Check the upper ",
         "hierarchy.", call. = FALSE)
  }

  # Sampled hauls with zero catch: a recorded zero, or no sample of the
  # species at all (then the species is assumed to be on the species list)
  zeroIds <- setdiff(lw[[lowId]], su[include == TRUE][[lowId]])
  recorded <- intersect(zeroIds,
                        su[excludeReason == zeroSampleReason()][[lowId]])
  notFound <- setdiff(zeroIds, su[[lowId]])
  if (length(recorded) > 0) {
    log <- addToRatioLog(log, step, "info",
      paste0("Sampled ", lowest, " units with a recorded zero of the ",
             "species: zero catch"), length(recorded), verbose)
  }
  if (length(notFound) > 0) {
    log <- addToRatioLog(log, step, "assumption",
      paste0("Sampled ", lowest, " units without a sample of the species are ",
             "counted as zero catch (species assumed on the species list)"),
      length(notFound), verbose)
  }

  attr(su, "unitWeights") <- unitWeights
  attr(su, "scheme") <- scheme
  attr(su, "ratioLog") <- log
  su[]
}

#' Extra columns a stage raising needs from the stage table (or VD)
#' @keywords internal
#' @noRd
stageFieldsNeeded <- function(RDBESDataObj, tb, s) {
  cols <- c(if (is.character(s$total)) s$total,
            if (is.character(s$value)) s$value,
            if (inherits(s$total, "RDBESExternalAux")) s$total$by,
            if (inherits(s$value, "RDBESExternalAux")) s$value$by,
            s$strata)
  available <- names(RDBESDataObj[[tb]])
  if (tb == "VS" && !is.null(RDBESDataObj$VD)) {
    available <- c(available, grep("^VD", names(RDBESDataObj$VD),
                                   value = TRUE))
  }
  unique(intersect(cols, available))
}

#' Put a total or value (column, function or external table) in a column
#' @keywords internal
#' @noRd
resolveRaisingArg <- function(units, arg, newCol, step, verbose) {
  log <- newRatioLog()
  u <- data.table::copy(units)
  if (inherits(arg, "RDBESExternalAux")) {
    missingBy <- setdiff(arg$by, names(u))
    if (length(missingBy) > 0) {
      stop(step, ": external key column(s) not in the unit table: ",
           paste(missingBy, collapse = ", "), call. = FALSE)
    }
    ext <- data.table::copy(arg$data)
    # Compare keys as text so that integer and character ids match
    for (b in arg$by) {
      ext[, (b) := as.character(get(b))]
      u[, (paste0(".key_", b)) := as.character(get(b))]
    }
    data.table::setnames(ext, c(arg$by, arg$col),
                         c(paste0(".key_", arg$by), newCol))
    keys <- paste0(".key_", arg$by)
    unmatched <- nrow(ext[!u, on = keys])
    if (unmatched > 0) {
      log <- addToRatioLog(log, step, "info",
        paste0("External rows of '", arg$col, "' that match no unit"),
        unmatched, verbose)
    }
    u <- merge(u, ext, by = keys, all.x = TRUE)
    u[, (keys) := NULL]
  } else if (is.function(arg)) {
    vals <- arg(u)
    if (!is.numeric(vals) || length(vals) != nrow(u)) {
      stop(step, ": the value function must return one number per unit.",
           call. = FALSE)
    }
    u[, (newCol) := vals]
  } else {
    if (!arg %in% names(u)) {
      stop(step, ": column '", arg, "' not found.", call. = FALSE)
    }
    vals <- as.numeric(u[[arg]])
    u[, (newCol) := vals]
  }
  list(units = u, log = log)
}
