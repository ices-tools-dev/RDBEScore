#' Check whether an object is a valid RDBESEstObject
#'
#' @param objectToCheck The object to check
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#'
#' @return Whoever revises this function please specify what it returns here
#'
#' @export
#'
#' @examples
#' \dontrun{
#' myH1RawObject <-
#' importRDBESDataCSV(rdbesExtractPath = "tests/testthat/h1_v_1_19")
#' myEStObj <- createRDBESEstObject(myH1RawObject,1)
#' validateRDBESEstObject(myEStObj)}
validateRDBESEstObject <- function(objectToCheck, verbose = FALSE){

  validRDBESEstObject <- TRUE
  warningText <- NA

  # CHECK 1 Have we just been passed NA?
  if (length(is.na(objectToCheck)) == 1) {
    if (is.na(objectToCheck)) {
      validRDBESEstObject <- FALSE
      warningText <- "objectToCheck is NA"
    }
    # CHECK 2 Is this an object of class RDBESDataObject?  It should be!
  } else if (! 'RDBESEstObject' %in% class(objectToCheck)) {
    validRDBESEstObject <- FALSE
    warningText <- "objectToCheck is not of the class RDBESEstObject"
    # CHECK 3 Is this a data table?  It should be!
  } else if (!inherits(objectToCheck, "data.table")) {
    validRDBESEstObject <- FALSE
    warningText <- "objectToCheck does not inherit from data.table"
  }

  # stop and give an error message if there's a problem
  if(!validRDBESEstObject) {
    stop(warningText)
  }

  # Return the validation result
  return(invisible(objectToCheck));

}
