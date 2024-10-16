#' Filter an RDBESEstObject
#'
#' The returned object will include all rows which include the field names
#' and have one of the allowed values in `valuesToFilter`.
#'
#'
#' @param RDBESEstObjectToFilter The `RDBESEstObject` to filter
#' @param fieldsToFilter A vector of the field names you wish to check
#' @param valuesToFilter A vector of the field values you wish to filter for
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#'
#' @return the filtered input object of the same class as
#'   `RDBESEstObjectToFilter`
#'
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#'
#'  myRawObject <- createRDBESDataObject(input = "tests\\testthat\\h1_v_1_19_26")
#'  myEstObject <- createRDBESEstObject(myRawObject,1)
#'  myFilteredEst <- filterRDBESEstObject(myEst,c("BVid"),c(7349207))
#'
#' }
filterRDBESEstObject <- function(RDBESEstObjectToFilter,
                                 fieldsToFilter,
                                 valuesToFilter,
                                 verbose = FALSE) {

  # Check we have a valid RDBESEstObject before doing anything else
  validateRDBESEstObject(RDBESEstObjectToFilter,
                          verbose = verbose)

  # Check if the requested columns actually exist in the object
  allColNames <- names(RDBESEstObjectToFilter)
  missingFields <- fieldsToFilter[!fieldsToFilter %in% allColNames]
  if (length(missingFields) > 0) {
    warning(paste0(
      "The following fields were not found in the ",
      "RDBESEstObject: ",
      missingFields
    ))
  }

  foundNames <- fieldsToFilter[fieldsToFilter %in% allColNames]
  if (length(foundNames) > 0) {
    RDBESEstObjectToFilter <-
            dplyr::filter(RDBESEstObjectToFilter,
                  dplyr::if_all(all_of(foundNames), ~ .x %in% valuesToFilter)
                  )
  }

  return(RDBESEstObjectToFilter)
}
