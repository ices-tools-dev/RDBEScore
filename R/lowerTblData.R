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
#'
#' @return A data frame containing the rows of the target lower level table that are associated with
#'   the given values of the upper table field.
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
lowerTblData <- function(field, values, tbls, level, verbose = FALSE, path_order = NULL) {
  if(!is.list(tbls)) stop("tbls must be a list")

  start <- substr(field, start = 1, stop = 2)
  if (start == level) {
    res <- tbls[[level]]
    return(res[get(field) %in% values])
  }
  currTbl <- which(start == names(tbls))

  # assumes tables are in correct order and no empty tables
  tc <- 1
  nextTbl <- tbls[[currTbl + tc]]
  while (is.null(nextTbl)) {
    tc <- tc + 1
    if(currTbl + tc > length(tbls)) stop("No more lower tables found")
    nextTbl <- tbls[[currTbl + tc]]
  }

  nextTblField <- paste0(names(tbls)[currTbl + tc], "id")
  if (verbose) {
    cat(paste0(field, ": ", paste0(values, collapse = ", "), "\n"))
  }
  if (!field %in% colnames(nextTbl)) {
    tbl <- tbls[[currTbl]]
    curTblId <- paste0(names(tbls)[currTbl], "id")
    values <- tbl[get(field) %in% values, get(curTblId)]
    field <- curTblId
  }

  # Initialize and update path order to ensure the original search field stays first
  if (is.null(path_order)) path_order <- field
  path_order <- unique(c(path_order, nextTblField))

  # values in next table for recursion
  nextTblValues <- nextTbl[get(field) %in% values, get(nextTblField)]

  # NEW: build link (current id -> next id) so we can keep intermediate IDs
  linkDT <- unique(nextTbl[get(field) %in% values,
                           .(from = get(field), to = get(nextTblField))])
  data.table::setnames(linkDT, c("from","to"), c(field, nextTblField))

  # recurse
  res <- lowerTblData(nextTblField, nextTblValues, tbls, level, verbose, path_order)

  # NEW: merge link back so the current level's ID is preserved
  # Always attach the authoritative parent id from the link.
  # If a column with the same name already exists (from another variant), drop it first,
  # then merge in the value derived from the current hierarchy.
  if (field %in% names(res)) {
    res[, (field) := NULL]
  }
  res <- merge(
    linkDT[, c(nextTblField, field), with = FALSE],
    res,
    by = nextTblField,
    all.y = TRUE,
    allow.cartesian = TRUE
  )

  #keep path columns toward the front with stable overall path order
  path_cols <- intersect(path_order, names(res))
  other_cols <- setdiff(names(res), path_cols)
  if (length(path_cols) > 0) data.table::setcolorder(res, c(path_cols, other_cols))

  # Ensure no data.table key is set on the result
  data.table::setkeyv(res, NULL)

  res
}
