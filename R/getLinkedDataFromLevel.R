#' Retrieve Linked Data Between RDBES Tables at a Specified Level
#'
#' The `getLinkedDataFromLevel` function facilitates the retrieval of linked data between different levels of RDBES tables. Depending on the relative positions of the source and target tables within the `RDBESDataObject`, the function determines whether to traverse "up" or "down" the data hierarchy to obtain the desired linked data.
#'
#' @param field A character string specifying the field name from which to retrieve linked data. The first two characters of this field indicate the source table.
#' @param values A vector of values corresponding to the specified `field` for which linked data is to be retrieved.
#' @param rdbesTables An `RDBESDataObject` containing the relevant RDBES tables. This object should include all tables that may be linked based on the provided `field` and `level`.
#' @param level A character string specifying the target table level from which to retrieve linked data. This must be one of the names within the `rdbesTables` object.
#' @param verbose Logical flag indicating whether to print detailed information about the data retrieval process. Default is `FALSE`.
#'
#' @return  The subset of the table at the specified `level`.
#'
#' @examples
#' \dontrun{
#' # Example 1: Going up in the table hierarchy to retrieve data from the DE table
#' # Retrieve data from the DE level based on BVid from the BV table
#' # This returns 1 row from the DE table
#' getLinkedDataFromLevel("BVid", c(1), H8ExampleEE1, "DE", TRUE)
#'
#' # Example 2: Going down in the table hierarchy to retrieve data from the SA table
#' # Retrieve data from the SA level based on DEid from the DE table
#' # This returns 15 rows from the SA table
#' getLinkedDataFromLevel("DEid", c(1), H8ExampleEE1, "SA", TRUE)
#'
#' # Example 3: Going up in the table hierarchy to see the Vessel that caught a specific fish
#' # Retrieve data from the VS level based on BVfishId from the BV table
#' getLinkedDataFromLevel("BVfishId", c("410472143", "410472144"), H8ExampleEE1, "VS", TRUE)
#' }
#' @export
getLinkedDataFromLevel <- function(field, values, rdbesTables, level, verbose = FALSE){

  # Check if rdbesTables is of class "RDBESDataObject"
  if(inherits(rdbesTables, "RDBESDataObject")) {
    rdbesTables <- sort(rdbesTables)
  } else {
    stop("rdbesTables must be of class RDBESDataObject")
  }

  # Check if the specified level exists within rdbesTables
  if(!level %in% names(rdbesTables)){
    stop("Table ", level, " not found in the RDBESDataObject")
  }

  # Extract the source table abbreviation from the field name
  sourceTbl <- substr(field, 1, 2)

  # Get the list of table names
  tblNames <- names(rdbesTables)

  # Determine the positions of sourceTbl and target level in the table list
  sourcePos <- which(tblNames == sourceTbl)
  targetPos <- which(tblNames == level)

  # Determine traversal direction based on table positions
  res <- NULL
  if(sourcePos > targetPos){
    if(verbose){
      cat("Traversing upwards in the table hierarchy from", sourceTbl, "to", level, "\n")
    }
    res <- upperTblData(field, values, rdbesTables, level, verbose)
  } else {
    if(verbose){
      cat("Traversing downwards in the table hierarchy from", sourceTbl, "to", level, "\n")
    }
    res <- lowerTblData(field, values, rdbesTables, level, verbose)
  }

  return(res)
}
