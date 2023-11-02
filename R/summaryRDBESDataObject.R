#' Summary method for RDBESDataObject
#'
#' This method returns a list containing the hierarchy of the DE data.table,
#' the number of rows for each data.table in the RDBESDataObject that is not NULL,
#' and a logical value indicating if the hierarchy is not NULL.
#'
#' @param object An object of class RDBESDataObject.
#' @param ... parameters to underling functions (not used currently)
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
summary.RDBESDataObject <- function(object, ...) {
  h <- unique(object$DE$DEhierarchy)
  #order items by Hierarchy
  if(!is.null(h)){object <- sort(object)}

  items <- sapply(object, function(x) {
    if (!is.null(x) && "data.table" %in% class(x)) {
      list(design = getDesignSummary(x),
           rows = nrow(x))
    } else {
      NULL
    }
  })
  # Remove NULL values from items
  items <- Filter(Negate(is.null), items)

  return(list(hierarchy=h, data=items))
}

getDesignSummary <- function(dt){
  #remove the table prefix
  colnames(dt) <- substr(colnames(dt), 3, nchar(colnames(dt)))

  # List of column names to add in the summary data if present
  cols <- c("selectMeth", "numSamp", "numTotal", "year")
  presentCols <- intersect(colnames(dt), cols)
  missingCols <- setdiff(cols, colnames(dt))
  # Create a new data frame with the same structure as dt
  res <- unique(dt[, presentCols, with=FALSE])

  # Add the missing columns and set them to NA
  #for (col in missingCols) {
  #  res[, (col) := NA]
  #}

  return(as.data.frame(res))

}


