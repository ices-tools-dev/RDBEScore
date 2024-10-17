#' Get lower table data from upper table id
#'
#' This function takes an upper table field name, values for that field, a list of tables,
#' and a target lower level table name. It returns the rows of the target lower level table
#' that are associated with the given values of the upper table field.
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
#'DE <- data.table::data.table(DEid = c(1, 2, 3, 4), SDid = c(1, 2, 3, 4))
#'SD <- data.table::data.table(SDid = c(1, 2, 3, 4), TEid = c(1, 2, 3, 4))
#'TE <- data.table::data.table(SDid = c(1, 2, 3, 4), TEid = c(1, 2, 3, 4))
#'VS <- data.table::data.table(TEid = c(1, 2, 3, 4), VSid = c(1, 2, 3, 4))
#'LE <- data.table::data.table(VSid = 1:5, LEid = 1:5, value = c(10, 20, 3, 4, 6))
#' tblsSprat <- list( DE = DE ,SD = SD, TE = TE, VS = VS, LE = LE )
#'
#' lowerTblData("TEid", c(4), tblsSprat, "LE", T)
#'
lowerTblData <- function(field, values, tbls, level, verbose = FALSE) {
  #check if tables are of correct type
  if(!is.list(tbls)) stop("tbls must be a list")
  #no null values are allowed
  if(!all(sapply(tbls, data.table::is.data.table))) stop("tbls must be a list of data tables")

  start <- substr(field, start = 1, stop = 2)
  if (start == level) {
    res <- tbls[[level]]
    return(res[get(field) %in% values])
  }
  currTbl <- which(start == names(tbls))
  #this assumes that the tables are in the correct order and no empty tables
  nextTbl <- tbls[[currTbl + 1]]
  nextTblField <- paste0(names(tbls)[currTbl + 1], "id")
  if (verbose) {
    cat(paste0(field, ": ", paste0(values, collapse = ", "), "\n"))
  }
  if (!field %in% colnames(nextTbl)) {
    tbl <- tbls[[currTbl]]
    curTblId <- paste0(names(tbls)[currTbl], "id")
    values <- tbl[get(field) %in% values, get(curTblId)]
    field <- curTblId
  }
  nextTblValues <- nextTbl[get(field) %in% values, get(nextTblField)]
  lowerTblData(nextTblField, nextTblValues, tbls, level, verbose)
}
