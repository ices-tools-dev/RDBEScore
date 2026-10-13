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
  ct <- data.table::data.table(
    field = c("DEhierarchy",
              "VSselectMeth", "FTselectMeth", "FOselectMeth", "SSselectMeth",
              "SAselectMeth", "BVselectMeth",
              "VSsamp", "FTsamp", "FOsamp", "SSsamp", "SAsamp",
              "VSstratification", "FTstratification", "FOstratification",
              "SSstratification", "SAstratification", "BVstratification",
              "VSnoSampReason", "FTnoSampReason", "FOnoSampReason",
              "SSnoSampReason", "SAnoSampReason",
              "SAlowHierarchy", "SAcatchCat", "SScatchFra",
              "SApres", "FMpres", "BVpres",
              "SAstateOfProc", "FMstateOfProc", "BVstateOfProc",
              "SAspecState", "SAunitType",
              "FMaccuracy", "BVaccuracy",
              "FMtypeMeas", "BVtypeMeas", "BVvalUnitScale"),
    codeType = c("RDBESUpperHierarchy",
                 rep("SelectionMethod", 6),
                 rep("YesNoFields", 5),
                 rep("YesNoFields", 6),
                 rep("ReasonForNotSampling", 5),
                 "LowerHierarchy", "CatchCategory", "CatchFraction",
                 rep("ProductPresentation", 3),
                 rep("StateOfProcessing", 3),
                 "SpecimensState", "SamplingUnit",
                 rep("AccuracyCode", 2),
                 rep("BiologicalMeasurementType", 2), "ValueUnitOrScale"))
  ct[, table := substr(field, 1, 2)]
  ct[]
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
#' su <- getSampleUnits(myH1Object, species = "PLE")
#' getRatioLog(su)
#' }
getRatioLog <- function(x) {
  lg <- attr(x, "ratioLog")
  if (is.null(lg)) newRatioLog() else lg
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
