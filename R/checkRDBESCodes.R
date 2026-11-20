#' Check coded RDBES fields against the ICES code lists
#'
#' Compares the values of the coded fields used in estimation (selection
#' methods, lower hierarchy, catch category, presentation, state of
#' processing, sampling unit, measurement type, accuracy, unit or scale and
#' yes/no fields) with the ICES code lists from [getRDBESCodeList()]. By
#' default these are the lists packaged with RDBEScore, so no internet
#' connection is needed.
#'
#' The ICES code lists can change between data upload and estimation. A value
#' that is not in the lists is reported, but the data are not changed and
#' [doRatioEstimation()] only gives a warning.
#'
#' @param RDBESDataObj A valid `RDBESDataObject`.
#' @param tables Optional character vector of table names (e.g. `c("SA",
#'   "BV")`) to restrict the check to. Default `NULL` checks all.
#' @param fields Optional character vector of R field names to restrict the
#'   check to. Default `NULL` checks all supported fields.
#' @param strict Passed to [validateRDBESDataObject()]. Default `TRUE`.
#' @param verbose Logical. If `TRUE` a message lists the invalid values.
#' @param refresh Logical. Passed to [getRDBESCodeList()]: if `TRUE` the code
#'   lists are downloaded from vocab.ices.dk instead of using the packaged
#'   snapshot. Default `FALSE`.
#'
#' @return A `data.table` with one row per table, field and distinct value:
#'   `table`, `field`, `codeType`, `value`, `n` (number of records), `valid`
#'   (value is in the ICES code list), `deprecated`, and `source` and
#'   `retrieved` of the code list used (see [getRDBESCodeList()]).
#' @seealso [getRDBESCodeList()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: rix133
#' - Notes/scope:
#' @export
#'
#' @examples
#' H1DataObj <- H1Example
#' codeCheck <- checkRDBESCodes(H1DataObj)
#' head(codeCheck)
#' # Values that are not in the packaged ICES code lists (none here)
#' codeCheck[valid == FALSE]
checkRDBESCodes <- function(RDBESDataObj,
                            tables = NULL,
                            fields = NULL,
                            strict = TRUE,
                            verbose = FALSE,
                            refresh = FALSE) {
  validateRDBESDataObject(RDBESDataObj, verbose = FALSE, strict = strict)

  map <- ratioFieldCodeTypes()
  if (!is.null(tables)) map <- map[table %in% tables]
  if (!is.null(fields)) map <- map[field %in% fields]

  res <- list()
  refreshed <- character()
  for (i in seq_len(nrow(map))) {
    tb <- RDBESDataObj[[map$table[i]]]
    f <- map$field[i]
    if (is.null(tb) || nrow(tb) == 0 || !f %in% names(tb)) next
    v <- as.character(tb[[f]])
    v <- v[!isBlank(v)]
    if (length(v) == 0) next
    cnt <- data.table::data.table(value = v)[, .(n = .N), by = value]
    # A refresh downloads each code type only once per call
    cl <- getRDBESCodeList(map$codeType[i],
                           refresh = refresh &&
                             !map$codeType[i] %in% refreshed)
    refreshed <- c(refreshed, map$codeType[i])
    cnt[, `:=`(table = map$table[i], field = f, codeType = map$codeType[i],
               valid = value %in% cl$Key,
               deprecated = value %in% cl$Key[cl$Deprecated],
               source = attr(cl, "source"),
               retrieved = attr(cl, "retrieved"))]
    res[[length(res) + 1L]] <- cnt
  }

  if (length(res) == 0) {
    out <- data.table::data.table(table = character(), field = character(),
                                  codeType = character(), value = character(),
                                  n = integer(), valid = logical(),
                                  deprecated = logical(),
                                  source = character(),
                                  retrieved = as.Date(character()))
  } else {
    out <- data.table::rbindlist(res, use.names = TRUE)
    data.table::setcolorder(out, c("table", "field", "codeType", "value", "n",
                                   "valid", "deprecated", "source",
                                   "retrieved"))
    data.table::setorder(out, table, field, value)
  }

  if (verbose && any(!out$valid)) {
    bad <- out[valid == FALSE]
    message("Values not in the ICES code lists:\n",
            paste0("  ", bad$field, " = '", bad$value, "' (n = ", bad$n, ")",
                   collapse = "\n"))
  }
  out[]
}
