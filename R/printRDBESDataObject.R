#' Print method for RDBESDataObject
#'
#' This method prints the hierarchy of the DE data.table (if it exists),
#' and the number of rows for each data.table in the RDBESDataObject that is not NULL.
#' If the RDBESDataObject has a mixed hierarchy, a warning message is printed.
#'
#' @param object An object of class RDBESDataObject.
#' @return None.
#' @examples
#' # Print the package data object
#' print(H1Example)
#' @rdname RDBESDataObject-methods
#' @method print RDBESDataObject
#' @export
print.RDBESDataObject <- function(object) {
  summary_values <- summary(object)
  if(summary_values$CS){
    if(length(summary_values$hierarchy)>1){
      warning("Mixed hierarchy RDBESDataObject!", call.=FALSE)
    }
    cat("Hierarchy",summary_values$hierarchy,"RDBESDataObject:\n ")
  } else {
    cat("No hierarchy RDBESDataObject:\n ")
  }
  cat(paste0(names(summary_values$rows), ": ", summary_values$rows, "\n"))
}
