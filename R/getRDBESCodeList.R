#' Get an ICES code list used by the RDBES
#'
#' Returns an ICES vocabulary code list (for example `"SelectionMethod"`,
#' `"LowerHierarchy"` or `"ProductPresentation"`). By default the list is read
#' from a snapshot of the ICES vocabulary distributed with the package, so no
#' internet connection is needed. With `refresh = TRUE` the current list is
#' downloaded from vocab.ices.dk through [icesVocab::getCodeList()]. If the
#' download fails, the snapshot is used with a warning.
#'
#' The snapshot holds every code type checked by [checkRDBESCodes()]:
#' `AccuracyCode`, `BiologicalMeasurementType`, `CatchCategory`,
#' `CatchFraction`, `LowerHierarchy`, `ProductPresentation`,
#' `RDBESUpperHierarchy`, `ReasonForNotSampling`, `SamplingUnit`,
#' `SelectionMethod`, `SpecimensState`, `StateOfProcessing`,
#' `ValueUnitOrScale` and `YesNoFields`. It is made with
#' `data-raw/refreshRDBESCodeSnapshot.R`. The ICES lists can change after the
#' snapshot was made, and after the data were uploaded.
#'
#' Each list is read once per R session and then served from a cache.
#'
#' @param codeType Character, a single ICES code type.
#' @param refresh Logical. If `TRUE` the list is downloaded from vocab.ices.dk
#'   (needs the suggested package icesVocab and internet access), replacing
#'   the cached list. Default `FALSE`.
#'
#' @return A `data.table` with columns `Key`, `Description` and `Deprecated`.
#'   The attribute `source` is `"package snapshot"` or `"ICES vocabulary"`,
#'   and `retrieved` is the date the list was downloaded.
#' @seealso [checkRDBESCodes()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: rix133
#' - Notes/scope:
#' @export
#'
#' @examples
#' selectionMethods <- getRDBESCodeList("SelectionMethod")
#' selectionMethods
#' # Date the packaged ICES list was downloaded
#' attr(selectionMethods, "retrieved")
getRDBESCodeList <- function(codeType, refresh = FALSE) {
  if (!is.character(codeType) || length(codeType) != 1 || is.na(codeType)) {
    stop("codeType must be a single character value", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1 || is.na(refresh)) {
    stop("refresh must be TRUE or FALSE", call. = FALSE)
  }
  if (!refresh && exists(codeType, envir = .rdbesCodeListCache,
                         inherits = FALSE)) {
    return(data.table::copy(get(codeType, envir = .rdbesCodeListCache)))
  }
  out <- NULL
  if (refresh) {
    out <- tryCatch(downloadRDBESCodeList(codeType), error = function(e) {
      warning("Could not download the ICES code list '", codeType, "': ",
              conditionMessage(e), " Using the package snapshot.",
              call. = FALSE)
      NULL
    })
  }
  if (is.null(out)) out <- snapshotRDBESCodeList(codeType)
  assign(codeType, out, envir = .rdbesCodeListCache)
  data.table::copy(out)
}

#' Read one code type from the packaged ICES code list snapshot
#'
#' @param codeType a single ICES code type
#' @return a data.table as returned by [getRDBESCodeList()]
#' @keywords internal
#' @noRd
snapshotRDBESCodeList <- function(codeType) {
  snapshotFile <- system.file("extdata", "rdbes-code-snapshot.csv",
                              package = "RDBEScore")
  if (!nzchar(snapshotFile)) {
    stop("The ICES code list snapshot is missing from the installed package.",
         call. = FALSE)
  }
  snapshot <- data.table::fread(snapshotFile, colClasses = "character",
                                na.strings = "")
  # Find the rows outside [] so that codeType is the argument, not the column
  rows <- which(snapshot$codeType == codeType)
  x <- snapshot[rows]
  if (nrow(x) == 0) {
    stop("ICES code type '", codeType, "' was not found in the package ",
         "snapshot of ICES code lists. Use refresh = TRUE to download it.",
         call. = FALSE)
  }
  out <- data.table::data.table(Key = x$Key, Description = x$Description,
                                Deprecated = x$Deprecated == "TRUE")
  data.table::setattr(out, "source", "package snapshot")
  data.table::setattr(out, "retrieved", as.Date(x$Retrieved[1]))
  out
}

#' Download one ICES code list from vocab.ices.dk
#'
#' @param codeType a single ICES code type
#' @return a data.table as returned by [getRDBESCodeList()]
#' @keywords internal
#' @noRd
downloadRDBESCodeList <- function(codeType) {
  if (!requireNamespace("icesVocab", quietly = TRUE)) {
    stop("the package icesVocab is needed for refresh = TRUE.", call. = FALSE)
  }
  x <- suppressMessages(icesVocab::getCodeList(codeType))
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
  data.table::setattr(out, "source", "ICES vocabulary")
  data.table::setattr(out, "retrieved", Sys.Date())
  out
}
