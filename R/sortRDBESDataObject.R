#' Sort method for RDBESDataObject
#'
#' This method sorts the RDBESDataObject based on the hierarchy.
#'
#' @param object An object of class RDBESDataObject.
#' @return The sorted RDBESDataObject.
#' @examples
#' # Sort the package data
#' sort(H8Example)
#' @rdname RDBESDataObject-methods
#' @method sort RDBESDataObject
#' @export
sort.RDBESDataObject <- function(object) {
  h <- unique(object$DE$DEhierarchy)

  # Order items by Hierarchy
  if(!is.null(h)){
    if(length(h)==1){
      if(is.na(h)){return(object)}
      # See if the user has specified a table to stop at
      targetTables <- RDBEScore::getTablesInRDBESHierarchy(h)
      restOfTables <- setdiff(names(object), targetTables)
      #put rest of tables to the end
      object <- object[c(targetTables,restOfTables)]
      # Reassign class
      class(object) <- c("RDBESDataObject", "list")
    } else{
      warning("No sort order for multiple hierarchies can be defined!", call.=F)
    }

  }

  return(object)
}
