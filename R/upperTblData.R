#' Get Field from Upper Table
#'
#' This function gets a field from an upper table using a list of tables for any
#' row. It takes four arguments: `field`, `values`, `tbls`, and `level`. The
#' function checks if the start of the provided field matches the provided level.
#' If it does, the function returns the rows from the corresponding table in the
#' provided list of tables that have the provided values for the specified field.
#' Otherwise, the function finds the previous table in the list of tables and
#' gets the values for its `id` column that match the provided values for the
#' specified field. The function then calls itself recursively with these new
#' values and returns the result.
#' NB! running on the RDBESDataObject will work properly if it is sorted by default the tables are not
#' in the correct order for a spesific hierarchy.
#'
#' @param field A character string specifying the field to get.
#' @param values A vector of values to match for the specified field.
#' @param tbls A named list of data frames containing tables at different levels.
#' @param level A character string specifying the level to get data from.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#'
#' @return A data frame containing rows from an upper table that match the
#'  provided values for the specified field.
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Human review: rix133
#'
#' @examples
#' # it is important to run these functions only on sorted RDBESDataObject
#' RDBEScore:::upperTblData("SAid", c(1), sort(H8ExampleEE1), "DE", verbose = TRUE)
#' DE <- data.table::data.table(DEid = c(1, 2))
#' SD <- data.table::data.table(SDid = c(1, 2), DEid = c(1, 2))
#' VS <- data.table::data.table(VSid = c(1, 2),SDid =c(1,2),  VSvalue = c(10, 20))
#' tbls <- list(DE = DE, SD = SD, VS = VS)
#' RDBEScore:::upperTblData("VSid", c(1), tbls, "DE")
#' @keywords internal
upperTblData <- function(field, values, tbls, level, verbose = FALSE,
                         requestedField = field, requestedValues = values) {

  if (!is.list(tbls)) stop("tbls must be a list")
  if (!level %in% names(tbls)) stop("Missing table in data: ", level)

  current <- substr(field, 1, 2)
  i <- match(current, names(tbls))
  if (is.na(i)) stop("Missing table: ", current)

  # Keep track of original requested IDs
  mapping <- unique(data.table::data.table(
    .currentValue = values,
    .requestedValue = requestedValues
  ))

  # Match rows at the current level
  result <- merge(mapping, tbls[[current]],
                  by.x = ".currentValue", by.y = field,
                  sort = FALSE, allow.cartesian = TRUE)

  # Continue recursively until the target level is reached
  if (current != level) {
    if (verbose) message(current, ": ", paste(values, collapse = ", "))

    # Find the nearest preceding non-empty table
    candidates <- seq_len(i - 1L)
    candidates <- candidates[vapply(tbls[candidates], NROW, integer(1)) > 0L]
    if (!length(candidates)) stop("No upper table found for ", current)

    parentField <- paste0(names(tbls)[max(candidates)], "id")

    return(upperTblData(
      parentField, result[[parentField]], tbls, level,
      verbose, requestedField, result[[".requestedValue"]]
    ))
  }

  # Restore column names at the target level
  data.table::setnames(result, ".currentValue", field)

  if (requestedField %in% names(result)) {
    result[, .requestedValue := NULL]
  } else {
    data.table::setnames(result, ".requestedValue", requestedField)
    data.table::setcolorder(
      result, c(requestedField, setdiff(names(result), requestedField))
    )
  }

  result
}
