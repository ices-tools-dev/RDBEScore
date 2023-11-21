#' Sort method for RDBESDataObject
#'
#' This method sorts the RDBESDataObject based on the hierarchy.
#'
#' @param x An object of class RDBESDataObject.
#' @param decreasing should hierarchy tables be the first ones
#' @param ... parameters to underling functions (not used currently)
#' @return The sorted RDBESDataObject by hierarchy.
#' @examples
#' # Sort the package data
#' sort(H8ExampleEE1)
#' @rdname RDBESDataObject-methods
#' @method sort RDBESDataObject
#' @export
sort.RDBESDataObject <- function(x, decreasing = TRUE, ...) {
  h <- unique(x$DE$DEhierarchy)

  # Order items by Hierarchy
  if(!is.null(h)){
    if(length(h)==1){
      if(is.na(h)){return(x)}
      # See if the user has specified a table to stop at
      targetTables <- RDBEScore::getTablesInRDBESHierarchy(h)
      restOfTables <- setdiff(names(x), targetTables)
      #put rest of tables to the end
      if(decreasing){ x <- x[c(targetTables,restOfTables)]}
      if(!decreasing){ x <- x[c(restOfTables, targetTables)]}

      # Reassign class
      class(x) <- c("RDBESDataObject", "list")
    } else{
      warning("No sort order for multiple hierarchies can be defined!", call.=F)
    }

  }

  return(x)
}
