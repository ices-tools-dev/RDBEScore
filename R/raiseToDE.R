#' Raise the weighted samples to the DE stratum
#'
#' Last raising step of stage-wise raising (see [raisingScheme()]). Each
#' expanded sample from [expandWithinSample()] is multiplied by its stage
#' weight `wStages` from [getStageWeights()], and summed per DE stratum,
#' domain and class:
#' \deqn{\hat N_{d,a} = r_h \sum_{i \in d} w_i y_{i,a}}
#' With DE `"none"`, \eqn{r_h = 1}. With DE `"ratio"`,
#' \eqn{r_h = T_h / \sum_{j \in h} w_j x_j} per DE stratum \eqn{h}, where
#' \eqn{T_h} is `DEauxVarTot` (or an external total) and \eqn{x_j} the
#' auxiliary value of the sampled units of the stage given by `at`, weighted
#' by their cumulative stage weight. When `at` is not `"SA"`, sampled units
#' without the species stay in the sum.
#'
#' The ratio is formed per DE stratum and shared by all domains (quarter,
#' area, métier, ...) of that stratum.
#'
#' **Totals.** When the scheme has `totals` ([totalsRaising()]), a second
#' ratio follows per totals stratum \eqn{g}:
#' \eqn{r_g = T_g / \sum_{j \in g} w_j r_{h(j)} x_j}, with \eqn{T_g} from
#' your table. Each sample is multiplied by \eqn{r_h r_g}. A totals stratum
#' with sampled units but no total stops the run (usually codes that do not
#' match). A total without sampled units is logged as a warning and not
#' raised. The rows are then per domain and class, without the DE stratum,
#' because totals strata can cut across DE strata, and `rApplied` is the
#' combined factor `liveWeightEst_kg / liveWeightSampled_kg` of the domain.
#'
#' `liveWeightEst_kg` is the **estimated** live weight of the species per
#' domain, \eqn{r_h \sum w x} with x the sampled live weight (`xSample`).
#' The SOP check in [summariseRatioDomains()] then compares the raised fish
#' weights with the raised sample weights.
#'
#' @param expanded Output of [expandWithinSample()].
#' @param stageWeights Output of [getStageWeights()].
#' @param RDBESDataObj The `RDBESDataObject` (for DE totals and the unit
#'   tables of `at`).
#' @param scheme The scheme from [raisingScheme()]. Default: the one stored in
#'   `stageWeights`.
#' @param domainVars Domain variables. Default
#'   `c("year", "quarter", "area", "metier6")`.
#' @param verbose Logical. Print log entries as messages. Default `FALSE`.
#'
#' @return A `data.table` with one row per domain, DE stratum and class: the
#'   domain variables, `DEstratumName` (not when there are totals), `class`,
#'   the raised `N`, `NW` (numbers with a weight), `W_g`, `NL` (numbers with
#'   a length), `L_mm`, `nFish`, and `liveWeightSampled_kg` (sum of
#'   `wStages * xSample`, before the ratios), `liveWeightEst_kg` (after the
#'   ratios) and `rApplied` (the DE ratio, 1 for `"none"`, or the combined
#'   factor with totals). Attributes:
#'   * `"deRatios"`: one row per DE stratum, with `DEstratumName`, `rDE`,
#'     `total` (the DE total) and `xWeightedStratum` (sum of w x over the
#'     stratum). Only `DEstratumName` and `rDE` for DE `"none"`.
#'   * `"totalsRatios"`: one row per totals stratum, with the strata columns,
#'     `rTotals`, `total`, `xWeightedStratum` and `nUnits`. `NULL` without
#'     totals.
#'   * `"domainSummary"`, `"target"`, `"domainVars"` and `"ratioLog"`.
#' @seealso [getStageWeights()], [summariseRatioEstimates()]
#' @export
raiseToDE <- function(expanded, stageWeights, RDBESDataObj,
                      scheme = attr(stageWeights, "scheme"),
                      domainVars = c("year", "quarter", "area", "metier6"),
                      verbose = FALSE) {
  if (!inherits(scheme, "RDBESRaisingScheme")) {
    stop("scheme must be made with raisingScheme().", call. = FALSE)
  }
  checkDomainVars(domainVars)
  step <- "raiseToDE"
  log <- newRatioLog()
  deStep <- scheme$DE
  stratCol <- deStep$strata

  su <- data.table::copy(stageWeights[include == TRUE &
                                        SAid %in% unique(expanded$SAid)])
  if (nrow(su) == 0) stop("No included samples with expanded data.")
  for (v in domainVars) {
    if (v %in% c("year", "quarter", "month")) su[, (v) := as.integer(get(v))]
    else su[, (v) := as.character(get(v))]
  }
  de <- data.table::copy(RDBESDataObj$DE)
  if (!stratCol %in% names(de)) {
    stop("DE strata column '", stratCol, "' not in DE.", call. = FALSE)
  }
  # One stratum per DE record. The label is the strata column, with the
  # DEid added when a label is used by more than one DE record (for example
  # several years or sampling schemes in one object).
  labs <- as.character(de[[stratCol]])
  dupLab <- labs %in% labs[duplicated(labs)]
  labs[dupLab] <- paste0(labs[dupLab], " [DEid ", de$DEid[dupLab], "]")
  deLab <- data.table::data.table(DEid = de$DEid, .stratum = labs)
  su <- merge(su, deLab, by = "DEid", all.x = TRUE)

  # ---------------------------------------------------------------------------
  # DE ratio per DE stratum
  # ---------------------------------------------------------------------------
  if (deStep$method == "none") {
    ratios <- data.table::data.table(.stratum = unique(su$.stratum),
                                     ratio = 1)
    log <- addToRatioLog(log, step, "info",
      "DE 'none': estimates are the weighted sums, no DE ratio", NA,
      verbose)
  } else {
    if (deStep$at == "SA") {
      xs <- sampleX(su, expanded, deStep$value, step, verbose)
      log <- rbind(log, xs$log)
      xu <- su[, .(.id = SAid, DEid, wCumulative = wStages)]
      xu[, x := xs$x[match(xu$.id, xs$SAid)]]
    } else {
      uw <- attr(stageWeights, "unitWeights")[[deStep$at]]
      if (is.null(uw)) {
        stop("No unit weights for stage ", deStep$at, ".", call. = FALSE)
      }
      idCol <- paste0(deStep$at, "id")
      units <- getUnitTable(RDBESDataObj, deStep$at,
                            fields = stageFieldsNeeded(
                              RDBESDataObj, deStep$at,
                              list(value = deStep$value, total = NULL,
                                   strata = NULL)),
                            verbose = verbose)
      r <- resolveRaisingArg(units, deStep$value, ".x", step, verbose)
      log <- rbind(log, r$log)
      xu <- merge(uw[, c(idCol, "DEid", "wCumulative"), with = FALSE],
                  r$units[, c(idCol, ".x"), with = FALSE], by = idCol)
      data.table::setnames(xu, c(idCol, ".x"), c(".id", "x"))
    }
    xu[, .v := wCumulative * x]

    r <- resolveRaisingArg(de, deStep$total, ".total", step, verbose)
    log <- rbind(log, r$log)
    deT <- merge(deLab, r$units[, .(DEid, .total)], by = "DEid")
    noSample <- deT[!is.na(.total) & .total > 0 & !DEid %in% xu$DEid]
    if (nrow(noSample) > 0) {
      stop("DE strata with a total but no sampled units: ",
           paste(noSample$.stratum, collapse = ", "), call. = FALSE)
    }
    xu <- merge(xu, deT, by = "DEid", all.x = TRUE)
    f <- computeStageFactors(xu, id = ".id", by = ".stratum",
                             total = ".total", value = ".v",
                             step = paste0(step, " (DE)"), verbose = verbose)
    log <- rbind(log, getRatioLog(f))
    ratios <- unique(f[, .(.stratum, ratio = fStage, T = total,
                           sumWX = sumValue)])
    log <- addToRatioLog(log, step, "info",
      paste0("DE ratio per ", stratCol, " with x measured on ", deStep$at),
      nrow(ratios), verbose)
  }

  # Multiplier of each sample: the DE ratio, then the totals ratio
  su[, mult := ratios$ratio[match(.stratum, ratios$.stratum)]]
  tot <- scheme$totals
  totalsRatios <- NULL
  if (!is.null(tot)) {
    tr <- raiseToTotals(su, expanded, stageWeights, RDBESDataObj, tot,
                        deLab, ratios, deStep$method, step, verbose)
    su[, mult := mult * tr$factor[match(SAid, tr$SAid)]]
    log <- rbind(log, tr$log)
    totalsRatios <- tr$ratios
  }

  # ---------------------------------------------------------------------------
  # Weighted sums per domain x class, and per DE stratum when there are no
  # totals (totals strata can cut across DE strata)
  # ---------------------------------------------------------------------------
  byS <- if (is.null(tot)) c(domainVars, ".stratum") else domainVars
  ys <- merge(expanded, su[, c("SAid", "wStages", "mult", byS), with = FALSE],
              by = "SAid")[
    , .(N = sum(wStages * mult * yN), NW = sum(wStages * mult * yNW),
        W_g = sum(wStages * mult * yW_g), NL = sum(wStages * mult * yNL),
        L_mm = sum(wStages * mult * yL_mm), nFish = sum(nFish)),
    by = c(byS, "class")]
  # Live weight of the species in the samples, before and after the ratios
  xs <- su[, .(liveWeightSampled_kg = sum(wStages * xSample),
               liveWeightEst_kg = sum(wStages * mult * xSample)), by = byS]
  out <- merge(ys, xs, by = byS)
  if (is.null(tot)) {
    out[, rApplied := ratios$ratio[match(.stratum, ratios$.stratum)]]
    data.table::setnames(out, ".stratum", "DEstratumName")
  } else {
    # the combined factor of the DE and totals ratios in the domain
    out[, rApplied := liveWeightEst_kg / liveWeightSampled_kg]
  }
  lead <- c(domainVars, if (is.null(tot)) "DEstratumName", "class")
  out <- out[, c(lead, "N", "NW", "W_g", "NL", "L_mm", "nFish",
                 "liveWeightSampled_kg", "liveWeightEst_kg", "rApplied"),
             with = FALSE]
  data.table::setorderv(out, lead)

  # Domain summary
  # Number of sampled units per stage, e.g. nVessels or nLandingEvents
  sc <- ratioStageChain(RDBESDataObj)
  labels <- sc$spec$unitLabels[sc$stages]
  idCols <- paste0(sc$stages, "id")
  keepIds <- idCols %in% names(su)
  idCols <- idCols[keepIds]
  labels <- labels[keepIds]
  domSum <- su[, c(list(nSamples = .N,
                        liveWeightSampled_kg = sum(wStages * xSample),
                        liveWeightEst_kg = sum(wStages * xSample * mult)),
                   lapply(.SD, data.table::uniqueN)),
               by = domainVars, .SDcols = idCols]
  data.table::setnames(domSum, idCols, unname(labels))

  log <- addToRatioLog(log, step, "info", "Domains estimated",
                       nrow(domSum), verbose)
  deRatios <- data.table::copy(ratios)
  data.table::setnames(deRatios, c(".stratum", "ratio", "T", "sumWX"),
                       c("DEstratumName", "rDE", "total", "xWeightedStratum"),
                       skip_absent = TRUE)
  data.table::setkey(deRatios, NULL)
  attr(out, "deRatios") <- deRatios[]
  attr(out, "totalsRatios") <- totalsRatios
  attr(out, "domainSummary") <- domSum
  attr(out, "target") <- attr(expanded, "target")
  attr(out, "domainVars") <- domainVars
  attr(out, "ratioLog") <- log
  out[]
}

#' The totals step: a ratio to user totals per totals stratum
#'
#' Units (samples for at = "SA", stage units otherwise) carry their weight
#' after the stages and the DE step, their x and their totals stratum.
#' Returns the factor per sample, the ratios per stratum and the log.
#' @keywords internal
#' @noRd
raiseToTotals <- function(su, expanded, stageWeights, RDBESDataObj, tot,
                          deLab, ratios, deMethod, step, verbose) {
  step <- paste0(step, " (totals)")
  log <- newRatioLog()
  cols <- tot$strata
  if (tot$at == "SA") {
    xs <- sampleX(su, expanded, tot$value, step, verbose)
    log <- rbind(log, xs$log)
    xu <- data.table::data.table(.id = su$SAid,
                                 wCumulative = su$wStages * su$mult)
    xu[, x := xs$x[match(.id, xs$SAid)]]
    st <- sampleStrata(su, RDBESDataObj, cols, step)
    xu <- cbind(xu, st[match(xu$.id, st$SAid), cols, with = FALSE])
    unitOfSA <- su$SAid
  } else {
    at <- tot$at
    idCol <- paste0(at, "id")
    uw <- attr(stageWeights, "unitWeights")[[at]]
    if (is.null(uw)) {
      stop(step, ": no unit weights for stage ", at, ".", call. = FALSE)
    }
    units <- getUnitTable(RDBESDataObj, at,
                          fields = stageFieldsNeeded(
                            RDBESDataObj, at,
                            list(value = tot$value, total = NULL,
                                 strata = NULL)),
                          verbose = verbose)
    r <- resolveRaisingArg(units, tot$value, ".x", step, verbose)
    log <- rbind(log, r$log)
    xu <- merge(uw[, c(idCol, "DEid", "wCumulative"), with = FALSE],
                r$units[, c(idCol, ".x"), with = FALSE], by = idCol)
    # weight after the DE step: the DE ratio of the unit's DE stratum
    rDE <- ratios$ratio[match(deLab$.stratum[match(xu$DEid, deLab$DEid)],
                              ratios$.stratum)]
    if (deMethod == "none") rDE[is.na(rDE)] <- 1
    if (anyNA(rDE)) {
      stop(step, ": ", at, " units in DE strata without a DE ratio.",
           call. = FALSE)
    }
    xu[, wCumulative := wCumulative * rDE]
    st <- unitStrata(RDBESDataObj, at, xu[[idCol]], cols,
                     attr(stageWeights, "areaLookup"), step)
    xu <- merge(xu, st, by = idCol)
    xu[, DEid := NULL]
    data.table::setnames(xu, c(idCol, ".x"), c(".id", "x"))
    unitOfSA <- su[[idCol]]
  }

  # Match the units to the user totals on the strata columns, as text
  keyOf <- function(d) {
    do.call(paste, c(lapply(cols, function(cl) as.character(d[[cl]])),
                     sep = " | "))
  }
  td <- data.table::copy(tot$total$data)
  td[, .g := keyOf(td)]
  td[, .total := as.numeric(get(tot$total$col))]
  xu[, .g := keyOf(xu)]
  noTotal <- setdiff(unique(xu$.g), td$.g)
  if (length(noTotal) > 0) {
    stop(step, ": totals strata (", paste(cols, collapse = " | "), ") with ",
         "sampled units but no row in the totals table: ",
         paste(utils::head(noTotal, 10), collapse = ", "),
         if (length(noTotal) > 10) " ...", ". Check that the codes match.",
         call. = FALSE)
  }
  noSample <- td[!is.na(.total) & .total > 0 & !.g %in% xu$.g]
  if (nrow(noSample) > 0) {
    log <- addToRatioLog(log, step, "warning",
      paste0("Totals strata with a total but no sampled units, not raised ",
             "(sum of their totals ", format(sum(noSample$.total)), "): ",
             paste(utils::head(noSample$.g, 10), collapse = ", "),
             if (nrow(noSample) > 10) " ..."),
      nrow(noSample), verbose)
  }
  xu[, .total := td$.total[match(.g, td$.g)]]
  xu[, .v := wCumulative * x]
  f <- computeStageFactors(xu, id = ".id", by = ".g", total = ".total",
                           value = ".v", step = step, verbose = verbose)
  log <- rbind(log, getRatioLog(f))
  ratiosT <- unique(f[, .(.g, rTotals = fStage, total,
                          xWeightedStratum = sumValue, nUnits = n)])
  ratiosT <- merge(unique(xu[, c(".g", cols), with = FALSE]), ratiosT,
                   by = ".g")
  ratiosT[, .g := NULL]
  data.table::setorderv(ratiosT, cols)
  log <- addToRatioLog(log, step, "info",
    paste0("Totals ratio per ", paste(cols, collapse = " x "),
           " with x measured on ", tot$at),
    nrow(ratiosT), verbose)
  fac <- f$fStage[match(unitOfSA, f$.id)]
  list(SAid = su$SAid, factor = fac, ratios = ratiosT[], log = log)
}

#' Totals strata per sample (at = "SA")
#'
#' Domain variables come from the sample units. An SA column is taken from
#' the lowest-level SA, else from its top-level SA. Other columns come from
#' the stage table (or DE) through its id.
#' @keywords internal
#' @noRd
sampleStrata <- function(su, RDBESDataObj, cols, step) {
  out <- data.table::data.table(SAid = su$SAid)
  sa <- RDBESDataObj$SA
  for (cl in cols) {
    if (cl %in% names(su)) {
      v <- su[[cl]]
    } else if (startsWith(cl, "SA") && cl %in% names(sa)) {
      v <- sa[[cl]][match(su$SAid, sa$SAid)]
      if ("topSAid" %in% names(su)) {
        top <- sa[[cl]][match(su$topSAid, sa$SAid)]
        v <- ifelse(isBlank(v), as.character(top), as.character(v))
      }
    } else {
      tb <- substr(cl, 1, 2)
      tbl <- RDBESDataObj[[tb]]
      idc <- paste0(tb, "id")
      if (is.null(tbl) || !cl %in% names(tbl) || !idc %in% names(su)) {
        stop(step, ": strata column '", cl, "' is not a domain variable, ",
             "an SA column or a column of a table on the path.",
             call. = FALSE)
      }
      v <- tbl[[cl]][match(su[[idc]], tbl[[idc]])]
    }
    out[, (cl) := as.character(v)]
  }
  out
}

#' Totals strata per stage unit (at a stage)
#'
#' Each unit is joined with its ancestors (and DE) and its descendants down
#' to the lowest stage. Domain variables are derived per row as in
#' getSampleUnits(). A unit with more than one value of a stratum stops.
#' @keywords internal
#' @noRd
unitStrata <- function(RDBESDataObj, at, ids, cols, areaLookup, step) {
  sc <- ratioStageChain(RDBESDataObj)
  stages <- sc$stages
  spec <- sc$spec
  k <- match(at, stages)
  idc <- paste0(at, "id")
  w <- data.table::copy(RDBESDataObj[[at]][get(idc) %in% ids])
  for (j in rev(seq_len(k - 1))) {
    tb <- stages[j]
    w <- merge(w, RDBESDataObj[[tb]], by = paste0(tb, "id"), all.x = TRUE,
               suffixes = c("", paste0(".", tb)))
  }
  if ("SDid" %in% names(w)) {
    w <- merge(w, RDBESDataObj$SD[, .(SDid, DEid)], by = "SDid", all.x = TRUE,
               suffixes = c("", ".SD"))
    w <- merge(w, RDBESDataObj$DE, by = "DEid", all.x = TRUE,
               suffixes = c("", ".DE"))
  }
  for (j in seq_along(stages)[seq_along(stages) > k]) {
    tb <- stages[j]
    w <- merge(w, RDBESDataObj[[tb]], by = paste0(stages[j - 1], "id"),
               all.x = TRUE, allow.cartesian = TRUE,
               suffixes = c("", paste0(".", tb)))
  }
  firstCol <- function(cl) {
    if (cl %in% names(w)) as.character(w[[cl]]) else rep(NA_character_, nrow(w))
  }
  dom <- list()
  date <- firstDate(w, spec$dateCols)
  dom$year <- as.integer(format(date, "%Y"))
  if ("DEyear" %in% names(w)) {
    dom$year[is.na(dom$year)] <- as.integer(w$DEyear[is.na(dom$year)])
  }
  dom$month <- as.integer(format(date, "%m"))
  dom$quarter <- (dom$month - 1L) %/% 3L + 1L
  dom$area <- firstCol(spec$areaCol)
  if (!is.null(areaLookup)) {
    al <- data.table::as.data.table(areaLookup)
    hit <- match(dom$area, al$from)
    dom$area[!is.na(hit)] <- al$to[hit[!is.na(hit)]]
  }
  dom$statRect <- firstCol(spec$statRectCol)
  dom$metier6 <- firstCol(spec$metierCol)

  out <- data.table::data.table(id = ids)
  for (cl in cols) {
    v <- if (cl %in% names(dom)) {
      dom[[cl]]
    } else if (cl %in% names(w)) {
      w[[cl]]
    } else {
      stop(step, ": strata column '", cl, "' is not a domain variable or a ",
           "column of a table on the path of ", at, ".", call. = FALSE)
    }
    d <- data.table::data.table(id = w[[idc]], v = as.character(v))
    u <- d[!isBlank(v), .(nVal = data.table::uniqueN(v), v = v[1]), by = id]
    multi <- u[nVal > 1, id]
    if (length(multi) > 0) {
      stop(step, ": ", length(multi), " ", at, " unit(s) have more than one ",
           "value of '", cl, "' (", idc, " ",
           paste(utils::head(multi, 10), collapse = ", "), "). Use strata ",
           "that are fixed per ", at, " unit, or at = \"SA\".", call. = FALSE)
    }
    out[, (cl) := u$v[match(id, u$id)]]
  }
  data.table::setnames(out, "id", idc)
  out
}

#' x per sample for a ratio measured on the samples (at = "SA")
#'
#' `"speciesLiveWeight"` is `xSample` from [getSampleUnits()]. `"fishWeight"` and
#' `"fishCount"` come from the expanded fish, so they carry the same
#' within-sample factor as the numbers at age. Any other value is an SA
#' column or a function of the sample table.
#' @return A list with `SAid`, `x` and `log`.
#' @keywords internal
#' @noRd
sampleX <- function(su, expanded, value, step, verbose) {
  log <- newRatioLog()
  if (identical(value, "speciesLiveWeight")) {
    out <- su[, .(SAid, x = xSample)]
    noX <- out[is.na(x), SAid]
    if (length(noX) > 0) {
      stop(step, ": the live weight of the species is missing for ",
           length(noX), " sample(s) (SAid ", paste(utils::head(noX, 10),
                                                   collapse = ", "),
           "). With sampleRaising(\"count\") or (\"none\") it needs ",
           "SAsampWtMes. Fill it or use value = \"fishWeight\".",
           call. = FALSE)
    }
    return(list(SAid = out$SAid, x = out$x, log = log))
  }
  if (identical(value, "fishWeight") || identical(value, "fishCount")) {
    e <- expanded[SAid %in% su$SAid,
                  .(n = sum(yN), nW = sum(yNW), sW = sum(yW_g)), by = SAid]
    if (identical(value, "fishCount")) {
      return(list(SAid = e$SAid, x = e$n, log = log))
    }
    noW <- e[nW <= 0, SAid]
    if (length(noW) > 0) {
      stop(step, ": value = \"fishWeight\" but ", length(noW),
           " sample(s) have no weighed fish (SAid ",
           paste(utils::head(noW, 10), collapse = ", "), ").",
           call. = FALSE)
    }
    part <- e[nW < n - 1e-9, .N]
    if (part > 0) {
      log <- addToRatioLog(log, step, "assumption",
        paste0("fishWeight: fish without a weight get the mean weight of the ",
               "weighed fish of their sample"), part, verbose)
    }
    # g to kg, scaled up for the fish without a weight
    return(list(SAid = e$SAid, x = e$sW * e$n / e$nW / 1000, log = log))
  }
  r <- resolveRaisingArg(su, value, ".x", step, verbose)
  list(SAid = r$units$SAid, x = r$units$.x, log = r$log)
}
