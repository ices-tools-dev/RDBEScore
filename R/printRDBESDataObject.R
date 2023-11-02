#' Print method for RDBESDataObject
#'
#' This method prints the hierarchy of the DE data.table (if it exists),
#' and the number of rows for each data.table in the RDBESDataObject that is not NULL.
#' It also provides the sampling method and number sampled and number total for tables
#' where it is applicable.
#' If the RDBESDataObject has a mixed hierarchy, a warning message is printed.
#'
#' @param x An object of class RDBESDataObject.
#' @param ... parameters to underling functions (not used currently)
#' @return None.
#' @examples
#' # Print the package data object
#' print(H1Example)
#' @rdname RDBESDataObject-methods
#' @method print RDBESDataObject
#' @export
print.RDBESDataObject <- function(x, ...) {
  summary_values <- summary(x)
  #years <- sapply(summary_values$data, function(x){getInfo(x$design, "year")})
  #years <- unique(years[!is.na(years)])
  if(!is.null(summary_values$hierarchy)){
    if(length(summary_values$hierarchy)>1){
      warning("Mixed hierarchy RDBESDataObject!", call.=FALSE)
    }
    cat(paste0("Hierarchy ",summary_values$hierarchy,
               " RDBESdataObject:\n "))
  } else {
    cat(paste0("No hierarchy, RDBESdataObject:\n "))
  }
  tblInfo <- sapply(summary_values$data, getPrintInfoForTable)
  names(tblInfo) <- names(summary_values$data)
  cat(paste0(names(tblInfo), ": ", paste(tblInfo), "\n"))
}

getInfo <- function(df, colName){
  if(is.numeric(df[[colName]]) && length(unique(df[[colName]])) > 1){
    paste0(min(df[[colName]]), "-", max(df[[colName]]))
  } else {
    ifelse(!is.null(df[[colName]]),
           paste0(unique(df[[colName]]), collapse = ","),
           NA)
  }
}

getPrintInfoForTable <- function(items){
  #if the table is empty just return the row count
  if(items$rows == 0){return(items$rows)}
  df <- items$design

  selMeth <- getInfo(df, "selectMeth")
  samp <- getInfo(df, "numSamp")
  tot <- getInfo(df, "numTotal")

  res <- paste0(items$rows,
                ifelse(is.na(selMeth), "", paste0(" (",selMeth,": ")),
                ifelse(is.na(samp), "",samp),
                ifelse(is.na(samp) & is.na(tot), "","/"),
                ifelse(is.na(tot), "", tot),
                ifelse(is.na(selMeth), "", ")")
                )
  return(res)
}
