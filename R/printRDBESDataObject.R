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
  if(!is.null(summary_values$hierarchy)){
    if(length(summary_values$hierarchy)>1){
      warning("Mixed hierarchy RDBESDataObject!", call.=FALSE)
    }
    cat(paste0("Hierarchy ",summary_values$hierarchy,
               " RDBESdataObject:\n "))
  } else {
    cat(paste0("No hierarchy, RDBESdataObject:\n "))
  }
  tblInfo <- mapply(getPrintInfoForTable, summary_values$data,
                    names(summary_values$data), SIMPLIFY = FALSE)
  names(tblInfo) <- names(summary_values$data)
  cat(paste0(names(tblInfo), ": ", paste(tblInfo), "\n"))
}

getPrintInfoForTable <- function(items, dfName){
  if(items$rows == 0){return(items$rows)}
  df <- items$design

  cols2get <- c("selectMeth", "numSamp", "numTotal")
  results <- lapply(cols2get, function(x) {getInfo(df, x)})
  names(results) <- cols2get
  selMeth <- results[["selectMeth"]][["text"]]
  samp <- results[["numSamp"]][["text"]]
  tot <- results[["numTotal"]][["text"]]

  warnings <- na.omit(sapply(results, function(x) x[["warningCol"]]))
  if(length(warnings) > 0){
    warning(paste0(dfName, ": ", paste0(warnings, collapse = ", "),
                   ifelse(length(warnings) > 1, " have", " has"),
                   " NAs!"), call.=FALSE)
  }

  res <- paste0(items$rows,
                ifelse(is.na(selMeth), "", paste0(" (",selMeth,": ")),
                ifelse(is.na(samp), "",samp),
                ifelse(is.na(samp) & is.na(tot), "","/"),
                ifelse(is.na(tot), "", tot),
                ifelse(is.na(selMeth), "", ")")
  )
  return(res)
}
#function returns a list of strings
getInfo <- function(df, colName){
  colData <- na.omit(df[[colName]])
  if(length(colData) != length(df[[colName]])){
    warningCol <- colName
  } else {
    warningCol <- NA
  }
  if(is.numeric(colData) && length(unique(colData)) > 1){
    text <- paste0(min(colData), "-", max(colData))
  } else {
    text <- ifelse(!is.null(colData),
           paste0(unique(colData), collapse = ","),
           NA)
  }
  return(list(text = text, warningCol = warningCol))
}
