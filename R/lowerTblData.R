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
#' @param path_order Internal: character vector tracking the traversal path of IDs during
#'   recursion to preserve a stable column order in the returned result. Users should not
#'   normally set this; it is maintained by recursive calls (default NULL).
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

  # assumes tables are in correct order; skip NULL and empty tables if bypassing is possible
  tc <- 1
  nextTbl <- tbls[[currTbl + tc]]
  nextTblField <- paste0(names(tbls)[currTbl + tc], "id")

  while (is.null(nextTbl) || (is.data.frame(nextTbl) && nrow(nextTbl) == 0)) {
    # Look ahead to the next non-empty table to check if it has the parent ID field
    tc_lookahead <- tc + 1
    found_bypass <- FALSE

    while(tc_lookahead + currTbl <= length(tbls)){
      lookahead_tbl <- tbls[[currTbl + tc_lookahead]]
      if(!is.null(lookahead_tbl) && is.data.frame(lookahead_tbl) && nrow(lookahead_tbl) > 0){
        # Found next non-empty table, check if it has parent field
        if(field %in% colnames(lookahead_tbl)){
          found_bypass <- TRUE
        }
        break
      }
      tc_lookahead <- tc_lookahead + 1
    }

    if(found_bypass){
      if(verbose) {
        cat(paste0("Skipping empty or NULL table: ", names(tbls)[currTbl + tc], "\n"))
      }
      tc <- tc + 1
      if(currTbl + tc > length(tbls)) stop("No more lower tables found")
      nextTbl <- tbls[[currTbl + tc]]
      nextTblField <- paste0(names(tbls)[currTbl + tc], "id")
    } else {
      # No bypass possible - this is a true empty result
      if(verbose) {
        cat(paste0("Empty table ", names(tbls)[currTbl + tc], " with no bypass - returning empty result\n"))
      }
      return(tbls[[level]][0,])
    }
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

  # values in next table for recursion
  nextTblValues <- nextTbl[get(field) %in% values, get(nextTblField)]

  # Check if intermediate table has no matching data and if bypass is possible
  if (length(nextTblValues) == 0 && length(values) > 0) {
    # Look ahead to see if we can bypass this non-empty but non-matching table
    # Only do this if values is non-empty (empty values can't match anything anyway)
    tc_lookahead <- tc + 1
    found_bypass <- FALSE

    while(tc_lookahead + currTbl <= length(tbls)){
      lookahead_tbl <- tbls[[currTbl + tc_lookahead]]
      if(!is.null(lookahead_tbl) && is.data.frame(lookahead_tbl) && nrow(lookahead_tbl) > 0){
        # Found next non-empty table, check if it has parent field
        if(field %in% colnames(lookahead_tbl)){
          found_bypass <- TRUE
        }
        break
      }
      tc_lookahead <- tc_lookahead + 1
    }

    if(found_bypass){
      # Bypass this table with no matches and continue with the next table
      if(verbose) {
        cat(paste0("No matches in table ", names(tbls)[currTbl + tc], ", bypassing\n"))
      }
      tc <- tc + 1
      nextTbl <- tbls[[currTbl + tc]]
      nextTblField <- paste0(names(tbls)[currTbl + tc], "id")
      # Recalculate nextTblValues with the new table
      nextTblValues <- nextTbl[get(field) %in% values, get(nextTblField)]
    }
    # If no bypass found, continue with empty nextTblValues
    # The recursion and merge logic will handle building the correct empty result
  }

  # Update path order after potential bypass
  path_order <- unique(c(path_order, nextTblField))

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
