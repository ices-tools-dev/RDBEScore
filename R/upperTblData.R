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
#' NB! running on the RDBESDataObject will not always work properly as the tables are not
#' in the correct order for all Hierarchies.
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
#' @examples
#' DE <- data.table(DEid = c(1, 2))
#' SD <- data.table(SDid = c(1, 2), DEid = c(1, 2))
#' VS <- data.table(VSid = c(1, 2),SDid =c(1,2),  value = c(10, 20))
#' tbls <- list(DE = DE, SD = SD, VS = VS)
#' upperTblData("VSid", c(1), tbls, "DE")
#'
upperTblData <- function(field, values, tbls, level, verbose = FALSE){
  #check if tables are of correct type
  if(!is.list(tbls)) stop("tbls must be a list")
  if(!level %in% names(tbls)) stop(level, " must be a character string in the names of the tables")

  start <- substr(field, start=1, stop=2)
  if(start == level){
    res <- tbls[[level]]
    return(res[get(field) %in% values,])
  }
  tbl <- tbls[[start]]
  currTbl <- which(start == names(tbls))
  if(verbose){
    print(paste0(start,": ", paste0(values, collapse = ", ")))
  }
  #skip NULL tables
  tc <- -1
  prevTbl <- names(tbls)[currTbl+tc]
  while(is.null(tbls[[prevTbl]])){
    if(verbose){
      print(paste0("Skipping: ", prevTbl))
    }
    tc <- tc - 1
    if((currTbl+tc) < 1) stop("No table found")
    prevTbl <- names(tbls)[currTbl+tc]

  }
  prevTblfield <-  paste0(names(tbls)[currTbl+tc], "id")
  prevTblvalues <- tbl[get(field) %in% values, get(prevTblfield)]
  upperTblData(prevTblfield,prevTblvalues, tbls[1:currTbl], level, verbose)
}
