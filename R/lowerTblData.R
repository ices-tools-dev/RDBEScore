#' Get lower table data from upper table id
#'
#' This function takes an upper table field name, values for that field, a list of tables,
#' and a target lower level table name. It returns the rows of the target lower level table
#' that are associated with the given values of the upper table field.
#' NB! running on the RDBESDataObject will work properly if it is sorted by default the tables are not
#' in the correct order for a specific hierarchy.
#'
#' @param field A character string specifying the name of the upper table field.
#' @param values A vector of values for the upper table field.
#' @param tbls A named list of data frames representing the tables.
#' @param level A character string specifying the name of the target lower level table.
#' @param verbose A logical value indicating whether to print intermediate levels during recursion.
#' @param requestedField Internal: the originally requested field, carried
#'   through the recursion (default `field`).
#' @param requestedValues Internal: the originally requested value belonging to
#'   each element of `values`, carried through the recursion (default `values`).
#'
#' @return A data.table with the originally requested field and all columns of
#'   the `level` table. No intermediate hierarchy IDs are added. It has one row
#'   per unique requested value and linked `level` record.
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: rix133
#'
#' @examples
#' # it is important to run the function only on the sorted RDBESDataObject
#' RDBEScore:::lowerTblData("TEid", c(4), sort(H8ExampleEE1), "SA", TRUE)
#'
#' DE <- data.table::data.table(DEid = c(1, 2, 3, 4), SDid = c(1, 2, 3, 4))
#' SD <- data.table::data.table(SDid = c(1, 2, 3, 4), TEid = c(1, 2, 3, 4))
#' TE <- data.table::data.table(SDid = c(1, 2, 3, 4), TEid = c(1, 2, 3, 4))
#' VS <- data.table::data.table(TEid = c(1, 2, 3, 4), VSid = c(1, 2, 3, 4))
#' LE <- data.table::data.table(VSid = 1:5, LEid = 1:5, value = c(10, 20, 3, 4, 6))
#' tblsSprat <- list( DE = DE ,SD = SD, TE = TE, VS = VS, LE = LE )
#'
#' RDBEScore:::lowerTblData("TEid", c(4), tblsSprat, "LE", TRUE)
#' @keywords internal
lowerTblData <- function(field, values, tbls, level, verbose = FALSE,
                         requestedField = field, requestedValues = values) {
  if (!is.list(tbls)) stop("tbls must be a list")
  if (!level %in% names(tbls)) stop("Missing table in data: ", level)

  current <- substr(field, 1, 2)
  i <- match(current, names(tbls))
  if (is.na(i)) stop("Missing table: ", current)

  # Keep track of original requested values
  mapping <- unique(data.table::data.table(
    .currentValue = values,
    .requestedValue = requestedValues
  ))

  result <- merge(mapping, tbls[[current]],
                  by.x = ".currentValue", by.y = field,
                  sort = FALSE, allow.cartesian = TRUE)
  data.table::setnames(result, ".currentValue", field)

  if (current != level) {
    if (verbose) cat(field, ": ", paste(values, collapse = ", "), "\n", sep = "")

    # Find the nearest following non-empty table in hierarchy order
    candidates <- seq_along(tbls)[-seq_len(i)]
    candidates <- candidates[vapply(tbls[candidates], NROW, integer(1)) > 0L]
    if (!length(candidates)) stop("No more lower tables found")

    nextName <- names(tbls)[min(candidates)]
    idField <- paste0(current, "id")
    nextField <- paste0(nextName, "id")
    if (!all(c(idField, nextField) %in% names(tbls[[nextName]]))) {
      stop("Error in linking tables ", current, " and ", nextName)
    }

    result <- result[, c(idField, ".requestedValue"), with = FALSE]
    result <- merge(result, tbls[[nextName]][, c(idField, nextField), with = FALSE],
                    by = idField, sort = FALSE, allow.cartesian = TRUE)
    return(lowerTblData(
      nextField, result[[nextField]], tbls, level,
      verbose, requestedField, result[[".requestedValue"]]
    ))
  }

  if (requestedField %in% names(result)) result[, (requestedField) := NULL]
  data.table::setnames(result, ".requestedValue", requestedField)
  data.table::setcolorder(result, c(requestedField, setdiff(names(result), requestedField)))

  result
}
