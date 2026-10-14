#' Filter an RDBESEstObject
#'
#' The returned object will include all rows which include the field names
#' and have one of the allowed values in `valuesToFilter`.
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
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: rix133
#' - Notes/scope: examples only
#'
#' @export
#' @md
#'
#' @examples
#' H1DataObj <- Pckg_survey_apistrat_H1
#' H1EstObj <- createRDBESEstObject(H1DataObj, 1, stopTable = "SA")
#' # Select the elementary-school stratum
#' H1EstObjFiltered <- filterRDBESEstObject(H1EstObj, "VSstratumName", "E")
#' dim(H1EstObjFiltered)
#'
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
    # Keep the rows where all of the found fields have an allowed value
    rowsToKeep <- Reduce(`&`, lapply(foundNames, function(fieldName) {
      RDBESEstObjectToFilter[[fieldName]] %in% valuesToFilter
    }))
    RDBESEstObjectToFilter <- RDBESEstObjectToFilter[rowsToKeep, ]
  }

  return(RDBESEstObjectToFilter)
}
