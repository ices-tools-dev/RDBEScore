#' Summary method for RDBESDataObject
#'
#' This method returns a list containing the hierarchy of the DE data.table,
#' the number of rows for each data.table in the RDBESDataObject that is not NULL,
#' and a logical value indicating if the hierarchy is not NULL.
#'
#' @param object An object of class RDBESDataObject.
#' @return A list with three elements:
#' \itemize{
#'   \item{hierarchy: The hierarchy of the DE data.table in the RDBESDataObject.}
#'   \item{rows: A named list where the names are the names of the data.tables in the RDBESDataObject and
#'                the values are the number of rows in each data.table. NULL values are excluded.}
#'   \item{CS: A logical value indicating if the hierarchy is not NULL.}
#' }
#' @examples
#' # Get summary of the package data
#' summary(H1Example)
#' @rdname RDBESDataObject-methods
#' @method summary RDBESDataObject
#' @export
summary.RDBESDataObject <- function(object) {
  h <- unique(object$DE$DEhierarchy)
  #order items by Hierarchy
  if(!is.null(h)){object <- sort(object)}

  items <- sapply(object, function(x) {
    if (!is.null(x) && "data.table" %in% class(x)) {
      nrow(x)
    } else {
      NULL
    }
  })
  # Remove NULL values from items
  items <- Filter(Negate(is.null), items)

  return(list(hierarchy=h, rows=items, CS=!is.null(h)))
}


