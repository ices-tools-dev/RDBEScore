#' Get an ICES code list used by the RDBES
#'
#' Downloads an ICES vocabulary code list (for example `"SelectionMethod"`,
#' `"LowerHierarchy"` or `"ProductPresentation"`) through
#' [icesVocab::getCodeList()]. Each list is downloaded once per R session and
#' then served from a cache.
#'
#' Code types relevant to the RDBES include `SelectionMethod`,
#' `LowerHierarchy`, `CatchCategory`, `CatchFraction`, `ProductPresentation`,
#' `StateOfProcessing`, `SpecimensState`, `SamplingUnit`, `AccuracyCode`,
#' `BiologicalMeasurementType`, `ValueUnitOrScale`, `ReasonForNotSampling`,
#' `RDBESUpperHierarchy` and `YesNoFields`. See
#' [icesVocab::getCodeTypeList()] for all code types.
#'
#' An internet connection to vocab.ices.dk is needed the first time a list is
#' requested in a session.
#'
#' @param codeType Character, a single ICES code type.
#' @param refresh Logical. If `TRUE` the list is downloaded again even if it
#'   is in the session cache. Default `FALSE`.
#'
#' @return A `data.table` with columns `Key`, `Description` and `Deprecated`.
#' @seealso [checkRDBESCodes()]
#' @importFrom icesVocab getCodeList
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
#' @export
#'
#' @examples
#' \dontrun{
#' getRDBESCodeList("SelectionMethod")
#' }
getRDBESCodeList <- function(codeType, refresh = FALSE) {
  if (!is.character(codeType) || length(codeType) != 1 || is.na(codeType)) {
    stop("codeType must be a single character value", call. = FALSE)
  }
  if (!refresh && exists(codeType, envir = .rdbesCodeListCache,
                         inherits = FALSE)) {
    return(data.table::copy(get(codeType, envir = .rdbesCodeListCache)))
  }
  x <- tryCatch(
    suppressMessages(icesVocab::getCodeList(codeType)),
    error = function(e) {
      stop("Could not download the ICES code list '", codeType,
           "' from vocab.ices.dk: ", conditionMessage(e), call. = FALSE)
    })
  if (!is.data.frame(x) || nrow(x) == 0) {
    stop("ICES code type '", codeType, "' was not found or is empty. ",
         "See icesVocab::getCodeTypeList() for valid code types.",
         call. = FALSE)
  }
  deprecated <- if ("Deprecated" %in% names(x)) {
    as.logical(x$Deprecated)
  } else {
    rep(FALSE, nrow(x))
  }
  deprecated[is.na(deprecated)] <- FALSE
  out <- data.table::data.table(Key = as.character(x$Key),
                                Description = as.character(x$Description),
                                Deprecated = deprecated)
  assign(codeType, out, envir = .rdbesCodeListCache)
  data.table::copy(out)
}
