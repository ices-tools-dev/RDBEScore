#' Check coded RDBES fields against the ICES code lists
#'
#' Compares the values of the coded fields used in estimation (selection
#' methods, lower hierarchy, catch category, presentation, state of
#' processing, sampling unit, measurement type, accuracy, unit or scale and
#' yes/no fields) with the live ICES code lists from [getRDBESCodeList()].
#'
#' @param RDBESDataObj A valid `RDBESDataObject`.
#' @param tables Optional character vector of table names (e.g. `c("SA",
#'   "BV")`) to restrict the check to. Default `NULL` checks all.
#' @param fields Optional character vector of R field names to restrict the
#'   check to. Default `NULL` checks all supported fields.
#' @param strict Passed to [validateRDBESDataObject()]. Default `TRUE`.
#' @param verbose Logical. If `TRUE` a message lists the invalid values.
#'
#' @return A `data.table` with one row per table, field and distinct value:
#'   `table`, `field`, `codeType`, `value`, `n` (number of records), `valid`
#'   (value is in the ICES code list) and `deprecated`.
#' @seealso [getRDBESCodeList()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
#' @export
#'
#' @examples
#' \dontrun{
#' codeCheck <- checkRDBESCodes(H1Example)
#' codeCheck[valid == FALSE]
#' }
checkRDBESCodes <- function(RDBESDataObj,
                            tables = NULL,
                            fields = NULL,
                            strict = TRUE,
                            verbose = FALSE) {
  validateRDBESDataObject(RDBESDataObj, verbose = FALSE, strict = strict)

  map <- ratioFieldCodeTypes()
  if (!is.null(tables)) map <- map[table %in% tables]
  if (!is.null(fields)) map <- map[field %in% fields]

  res <- list()
  for (i in seq_len(nrow(map))) {
    tb <- RDBESDataObj[[map$table[i]]]
    f <- map$field[i]
    if (is.null(tb) || nrow(tb) == 0 || !f %in% names(tb)) next
    v <- as.character(tb[[f]])
    v <- v[!isBlank(v)]
    if (length(v) == 0) next
    cnt <- data.table::data.table(value = v)[, .(n = .N), by = value]
    cl <- getRDBESCodeList(map$codeType[i])
    cnt[, `:=`(table = map$table[i], field = f, codeType = map$codeType[i],
               valid = value %in% cl$Key,
               deprecated = value %in% cl$Key[cl$Deprecated])]
    res[[length(res) + 1L]] <- cnt
  }

  if (length(res) == 0) {
    out <- data.table::data.table(table = character(), field = character(),
                                  codeType = character(), value = character(),
                                  n = integer(), valid = logical(),
                                  deprecated = logical())
  } else {
    out <- data.table::rbindlist(res, use.names = TRUE)
    data.table::setcolorder(out, c("table", "field", "codeType", "value", "n",
                                   "valid", "deprecated"))
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
