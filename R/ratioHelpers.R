# Internal helpers for the ratio estimation function family:
# getSampleUnits(), expandWithinSample(), getStageWeights(), raiseToDE(),
# summariseRatioEstimates() and doRatioEstimation().

# Session cache for ICES code lists downloaded by getRDBESCodeList()
.rdbesCodeListCache <- new.env(parent = emptyenv())

#' RDBES fields that are checked against ICES code lists
#'
#' @return a data.table with the RDBES R field name, its table and the ICES
#'   vocabulary code type that holds its allowed values
#' @keywords internal
#' @noRd
ratioFieldCodeTypes <- function() {
  # Design fields of the upper hierarchy tables of all supported hierarchies
  upper <- c("VS", "FT", "FO", "OS", "LE", "SS")
  byPrefix <- function(prefixes, suffix, codeType) {
    data.table::data.table(field = paste0(prefixes, suffix),
                           codeType = codeType)
  }
  ct <- data.table::rbindlist(list(
    data.table::data.table(field = "DEhierarchy",
                           codeType = "RDBESUpperHierarchy"),
    byPrefix(c(upper, "SA", "BV"), "selectMeth", "SelectionMethod"),
    byPrefix(c(upper, "SA"), "samp", "YesNoFields"),
    byPrefix(c(upper, "SA", "BV"), "stratification", "YesNoFields"),
    byPrefix(c(upper, "SA"), "noSampReason", "ReasonForNotSampling"),
    data.table::data.table(
      field = c("SSuseCalcZero",
                "SAlowHierarchy", "SAcatchCat", "SScatchFra",
                "SApres", "FMpres", "BVpres",
                "SAstateOfProc", "FMstateOfProc", "BVstateOfProc",
                "SAspecState", "SAunitType",
                "FMaccuracy", "BVaccuracy",
                "FMtypeMeas", "BVtypeMeas", "BVvalUnitScale"),
      codeType = c("YesNoFields",
                   "LowerHierarchy", "CatchCategory", "CatchFraction",
                   rep("ProductPresentation", 3),
                   rep("StateOfProcessing", 3),
                   "SpecimensState", "SamplingUnit",
                   rep("AccuracyCode", 2),
                   rep("BiologicalMeasurementType", 2), "ValueUnitOrScale"))))
  ct[, table := substr(field, 1, 2)]
  ct[]
}

#' Upper hierarchies supported by stage-wise raising
#' @keywords internal
#' @noRd
supportedHierarchies <- function() c(1L, 5L)

#' What stage-wise raising needs to know about one upper hierarchy
#'
#' Adding a hierarchy means adding an entry here, plus a simulator and tests.
#'
#' @param hierarchy the upper hierarchy number
#' @return a list with
#'   * `hierarchy`
#'   * `stages`: the sampling stages between SD and SS, from the package
#'     table of hierarchies
#'   * `optional`: optional stages that can be used for raising, as a named
#'     character vector stage = parent stage (FT under LE in H5)
#'   * `dateCols`: date columns, in order of preference
#'   * `areaCol`, `statRectCol`, `metierCol`: used when the SA value is blank
#'   * `unitLabels`: names of the sample counts per stage
#' @keywords internal
#' @noRd
hierarchySpec <- function(hierarchy) {
  h <- suppressWarnings(as.integer(hierarchy))
  if (length(h) != 1 || is.na(h) || !h %in% supportedHierarchies()) {
    stop("Hierarchy ", paste(hierarchy, collapse = ", "),
         " is not implemented yet for stage-wise raising. Implemented: ",
         paste(supportedHierarchies(), collapse = ", "), ".", call. = FALSE)
  }
  chain <- getTablesInRDBESHierarchy(h, includeOptTables = FALSE,
                                     includeLowHierTables = FALSE,
                                     includeTablesNotInSampHier = FALSE)
  spec <- switch(as.character(h),
    "1" = list(optional = character(0),
               dateCols = c("FOendDate", "FOstartDate", "FTarvDate"),
               areaCol = "FOarea", statRectCol = "FOstatRect",
               metierCol = "FOmetier6",
               unitLabels = c(VS = "nVessels", FT = "nTrips",
                              FO = "nHauls")),
    "5" = list(optional = c(FT = "LE"),
               dateCols = c("LEdate", "FTarvDate", "OSsamDate"),
               areaCol = "LEarea", statRectCol = "LEstatRect",
               metierCol = "LEmetier6",
               unitLabels = c(OS = "nOnshoreEvents", LE = "nLandingEvents",
                              FT = "nTrips")))
  c(list(hierarchy = h, stages = setdiff(chain, c("DE", "SD", "SS", "SA"))),
    spec)
}

#' Hierarchy of an RDBESDataObject, which must hold exactly one
#' @keywords internal
#' @noRd
dataHierarchy <- function(RDBESDataObj) {
  hier <- unique(RDBESDataObj$DE$DEhierarchy)
  if (length(hier) != 1 || is.na(hier)) {
    stop("The object must hold one upper hierarchy. DEhierarchy in the ",
         "data: ", paste(hier, collapse = ", "), call. = FALSE)
  }
  as.integer(hier)
}

#' The chain of tables used for raising in this data set
#'
#' The sampling stages of the hierarchy, plus an optional stage when the
#' data use it. FT is a stage in H5 when the SS records link to FT, and FT
#' links to LE. A mix (some SS on LE, some on FT) stops.
#'
#' @param RDBESDataObj an RDBESDataObject
#' @param hierarchy the hierarchy, default taken from DE
#' @return a list with `chain` (DE, SD, the stages, SS, SA), `stages` and
#'   `spec` (from hierarchySpec())
#' @keywords internal
#' @noRd
ratioStageChain <- function(RDBESDataObj,
                            hierarchy = dataHierarchy(RDBESDataObj)) {
  spec <- hierarchySpec(hierarchy)
  stages <- spec$stages
  ss <- RDBESDataObj$SS
  for (opt in names(spec$optional)) {
    parent <- spec$optional[[opt]]
    optId <- paste0(opt, "id")
    parId <- paste0(parent, "id")
    if (is.null(ss) || !optId %in% names(ss)) next
    onOpt <- !is.na(ss[[optId]])
    if (!any(onOpt)) next
    onParent <- !onOpt & parId %in% names(ss) & !is.na(ss[[parId]])
    if (any(onParent)) {
      stop("SS records link to both ", parent, " (", sum(onParent), ") and ",
           opt, " (", sum(onOpt), "). Use one level for all records, or ",
           "split the object.", call. = FALSE)
    }
    optTbl <- RDBESDataObj[[opt]]
    if (is.null(optTbl) || !parId %in% names(optTbl) ||
        all(is.na(optTbl[[parId]]))) {
      stop("SS records link to ", opt, " but ", opt, " has no ", parId,
           ".", call. = FALSE)
    }
    pos <- match(parent, stages)
    stages <- append(stages, opt, after = pos)
  }
  list(chain = c("DE", "SD", stages, "SS", "SA"), stages = stages,
       spec = spec)
}

#' Was the species looked for in sampled lowest-stage units without an SA?
#'
#' A unit is a zero when one of its sampled SS records for the catch category
#' has `SSuseCalcZero = "Y"` and the species is on its species list. Without
#' SL/IS in the object, `"Y"` alone counts (an assumption). Otherwise the
#' species was not looked for.
#'
#' @param RDBESDataObj an RDBESDataObject
#' @param lowest the lowest stage table name (e.g. "FO" or "LE")
#' @param ids ids of the units to classify
#' @param species AphiaID(s)
#' @param catchCat catch category, e.g. "Lan"
#' @return data.table with the unit id column and `reason`, one of
#'   "on list", "no list", "not on list", "SSuseCalcZero not Y"
#' @keywords internal
#' @noRd
speciesLookedFor <- function(RDBESDataObj, lowest, ids, species, catchCat) {
  idCol <- paste0(lowest, "id")
  out <- data.table::data.table(id = ids, reason = "SSuseCalcZero not Y")
  if (length(ids) == 0) {
    data.table::setnames(out, "id", idCol)
    return(out)
  }
  ss <- data.table::copy(RDBESDataObj$SS)
  ss <- ss[ss[[idCol]] %in% ids]
  if ("SSsamp" %in% names(ss)) ss <- ss[isBlank(SSsamp) | SSsamp != "N"]
  # An SS of the catch category, or of the whole catch
  if ("SScatchFra" %in% names(ss)) {
    ss <- ss[isBlank(SScatchFra) | SScatchFra %in% c(catchCat, "Catch")]
  }
  useZero <- if ("SSuseCalcZero" %in% names(ss)) {
    !is.na(ss$SSuseCalcZero) & ss$SSuseCalcZero == "Y"
  } else {
    rep(FALSE, nrow(ss))
  }
  ss <- ss[useZero]
  sl <- RDBESDataObj$SL
  is <- RDBESDataObj$IS
  hasList <- !is.null(sl) && nrow(sl) > 0 && !is.null(is) && nrow(is) > 0 &&
    "SLid" %in% names(ss)
  if (nrow(ss) > 0) {
    if (hasList) {
      onList <- data.table::as.data.table(merge(sl, is, by = "SLid"))
      onList <- onList[as.character(IScommTaxon) %in% as.character(species) &
                         SLcatchFrac %in% c(catchCat, "Catch"), unique(SLid)]
      ss[, .reason := data.table::fifelse(SLid %in% onList, "on list",
                                          "not on list")]
    } else {
      ss[, .reason := "no list"]
    }
    # One unit can have several SS: a zero if any of them says so
    rank <- c("on list" = 1, "no list" = 2, "not on list" = 3)
    best <- ss[, .(.reason = .reason[which.min(rank[.reason])]),
               by = idCol]
    m <- match(out$id, best[[idCol]])
    out[!is.na(m), reason := best$.reason[m[!is.na(m)]]]
  }
  data.table::setnames(out, "id", idCol)
  out[]
}

#' Design-quality class of each RDBES selection method code
#'
#' The classes follow "Use of selection methods during estimation in the
#' RDBES" (RDBES data model documentation). They are an RDBEScore
#' classification, not part of the ICES vocabulary.
#'
#' @return a data.table with columns selectMeth and selectionClass
#' @keywords internal
#' @noRd
selectionMethodClasses <- function() {
  data.table::data.table(
    selectMeth = c("SRSWR", "SRSWOR", "UPSWR", "UPSWOR", "SYSS",
                   "CENSUS",
                   "NPQSRSWR", "NPQSRSWOR", "NPQSYSS",
                   "NPJS", "NPCS", "NPCLQS-T", "NPCLQS-O", "Unknown",
                   "FIXED", "NotApplicable", "D", "R"),
    selectionClass = c(rep("Probabilistic", 5),
                       "Census",
                       rep("Quasi-probabilistic", 3),
                       rep("Non-probabilistic", 5),
                       rep("Other", 4)))
}

#' Class width in millimetres for RDBES length accuracy codes
#'
#' @return named numeric vector, names are ICES AccuracyCode keys
#' @keywords internal
#' @noRd
accuracyCodeToMM <- function() {
  c(mm = 1, smm = 0.5, scm = 5, cm = 10, "25mm" = 25, "5cm" = 50)
}

#' Domain variables available on the sample units
#'
#' @return character vector of the domain variables made by getSampleUnits()
#' @keywords internal
#' @noRd
ratioDomainVars <- function() {
  c("year", "quarter", "month", "area", "statRect", "metier6")
}

#' Check that species are given as AphiaIDs
#'
#' Stops when a value is not a whole number. For FAO codes found in the SA
#' table, the message suggests the matching AphiaID.
#' @param species the species argument
#' @param sa the SA table, used to suggest AphiaIDs
#' @keywords internal
#' @noRd
checkAphiaIds <- function(species, sa = NULL) {
  num <- suppressWarnings(as.numeric(as.character(species)))
  bad <- species[is.na(num) | num != round(num)]
  if (length(bad) == 0) return(invisible(TRUE))
  hint <- ""
  if (!is.null(sa) && all(c("SAspeCodeFAO", "SAspeCode") %in% names(sa))) {
    m <- unique(sa[as.character(sa$SAspeCodeFAO) %in% as.character(bad),
                   c("SAspeCodeFAO", "SAspeCode"), with = FALSE])
    if (nrow(m) > 0) {
      hint <- paste0(" In this SA table: ",
                     paste0(m$SAspeCodeFAO, " = ", m$SAspeCode,
                            collapse = ", "), ".")
    }
  }
  stop("species must be AphiaID(s), not: ", paste(bad, collapse = ", "),
       ".", hint, call. = FALSE)
}

#' Exclusion reason of a recorded zero (an SA with total weight 0)
#' @keywords internal
#' @noRd
zeroSampleReason <- function() "recorded zero (total weight 0)"

#' Check that the requested domain variables are supported
#' @keywords internal
#' @noRd
checkDomainVars <- function(domainVars) {
  ok <- ratioDomainVars()
  bad <- setdiff(domainVars, ok)
  if (length(bad) > 0) {
    stop("Unsupported domainVars: ", paste(bad, collapse = ", "),
         ". Allowed: ", paste(ok, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

#' Empty assumptions and exclusions log
#' @keywords internal
#' @noRd
newRatioLog <- function() {
  data.table::data.table(step = character(), level = character(),
                         message = character(), n = integer())
}

#' Add an entry to a ratio estimation log
#'
#' @param log the log data.table
#' @param step the function that wrote the entry
#' @param level one of "info", "assumption", "exclusion", "warning"
#' @param message text of the entry
#' @param n number of records concerned (optional)
#' @param verbose print the entry as a message as well
#' @keywords internal
#' @noRd
addToRatioLog <- function(log, step, level, message, n = NA_integer_,
                          verbose = FALSE) {
  if (verbose) message("[", step, "] ", level, ": ", message,
                       if (!is.na(n)) paste0(" (n = ", n, ")") else "")
  rbind(log, data.table::data.table(step = step, level = level,
                                    message = message, n = as.integer(n)))
}

#' Get the log of a ratio estimation step
#'
#' Every step of [doRatioEstimation()] attaches a log of its assumptions,
#' exclusions, warnings and information to its output. This returns it.
#'
#' @param x The output of [getSampleUnits()], [expandWithinSample()],
#'   [getStageWeights()] or [raiseToDE()].
#'
#' @return A `data.table` with columns `step`, `level` (`"info"`,
#'   `"assumption"`, `"exclusion"` or `"warning"`), `message` and `n`
#'   (number of records concerned). Empty if `x` has no log.
#' @seealso [doRatioEstimation()], whose result holds all logs in `$log`.
#' @export
#'
#' @examples
#' \dontrun{
#' su <- getSampleUnits(myH1Object, species = 127143)
#' getRatioLog(su)
#' }
getRatioLog <- function(x) {
  lg <- attr(x, "ratioLog")
  if (is.null(lg)) newRatioLog() else lg
}

#' First non-missing date per row, over several date columns
#' @param d a data.frame
#' @param cols date columns in order of preference (missing ones are skipped)
#' @keywords internal
#' @noRd
firstDate <- function(d, cols) {
  out <- as.Date(rep(NA_character_, nrow(d)))
  for (cl in intersect(cols, names(d))) {
    miss <- is.na(out)
    if (!any(miss)) break
    out[miss] <- parseRDBESDate(d[[cl]])[miss]
  }
  out
}

#' Parse RDBES date strings (ISO yyyy-mm-dd, or dd/mm/yyyy)
#' @keywords internal
#' @noRd
parseRDBESDate <- function(x) {
  x <- as.character(x)
  x[x == ""] <- NA_character_
  d <- as.Date(substr(x, 1, 10), format = "%Y-%m-%d")
  retry <- is.na(d) & !is.na(x)
  if (any(retry)) d[retry] <- as.Date(x[retry], format = "%d/%m/%Y")
  d
}

#' TRUE where a character value is missing or empty
#' @keywords internal
#' @noRd
isBlank <- function(x) {
  is.na(x) | as.character(x) == ""
}
