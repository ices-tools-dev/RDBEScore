#' Filter and remove orphan records in an RDBESDataObject
#'
#' This function filters an RDBESDataObject based on specified fields and values, and can optionally remove any orphan records.
#' The returned object will include all rows which either: a) do not include any of the field names in `fieldsToFilter`, or b) do include the field names and have one of the allowed values in `valuesToFilter`.
#' If `killOrphans` is set to `TRUE`, the function will remove orphaned rows. The default is `FALSE`.
#'
#' @param RDBESDataObjectToFilterAndTidy The RDBESDataObject to filter.
#' @param fieldsToFilter A vector of the field names you wish to check.
#' @param valuesToFilter A vector of the field values you wish to filter for.
#' @param killOrphans Controls if orphan rows are removed. Default is `FALSE`.
#' @param verboseOrphans Controls if verbose output for orphan rows is printed. Default is `FALSE`.
#' @param verboseBrokenVesselLinks Controls if verbose output for broken vessel links is printed. Default is `FALSE`.
#'
#' @return The filtered input object of the same class as `RDBESDataObjectToFilterAndTidy`.
#'
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <- createRDBESDataObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19_13")
#'
#' # To check how removeBrokenVesselLinks() works
#' myH1RawObject$VD$VDlenCat[which(myH1RawObject$VD$VDencrVessCode=="VDcode_10")] <- "VL40XX"
#'
#' myFields <- c("VSencrVessCode", "VDlenCat")
#' myValues <- c("VDcode_1","VDcode_2", "VDcode_10","VL1518","VL2440")
#'
#' myFilteredObject <- filterAndTidyRDBESDataObject(myH1RawObject,
#'   fieldsToFilter = myFields,
#'   valuesToFilter = myValues,
#'   killOrphans = TRUE, verboseBrokenVesselLinks = TRUE
#' )
#' }
filterAndTidyRDBESDataObject <- function(RDBESDataObjectToFilterAndTidy,
                                 fieldsToFilter,
                                 valuesToFilter,
                                 killOrphans = FALSE,
                                 verboseOrphans = FALSE,
                                 verboseBrokenVesselLinks = FALSE)
  {

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(RDBESDataObjectToFilterAndTidy, verbose = FALSE)

  # 1 - filter
  # If now fields/values to filter, then the same object is returned; else filtering
  if (!(missing(fieldsToFilter) | missing(valuesToFilter)))
    RDBESDataObjectToFilterAndTidy <- filterRDBESDataObject(RDBESDataObjectToFilterAndTidy,
                                                      fieldsToFilter,
                                                      valuesToFilter)
  # 2 - remove broken vessels links, broken species links not included yet
  # note to myself
  if (any(grepl("VD", fieldsToFilter)))
    print(paste0("VD filtered by: ", fieldsToFilter[which(grepl("VD", fieldsToFilter))]))
  RDBESDataObjectToFilterAndTidy <- removeBrokenVesselLinks(RDBESDataObjectToFilterAndTidy, verbose = verboseBrokenVesselLinks)


  # 3 - remove orphans
  # Remove orphans after filtering and removing data missed in VD
  if (killOrphans == TRUE)
      RDBESDataObjectToFilterAndTidy <- findAndKillOrphans(RDBESDataObjectToFilterAndTidy, verbose = verboseOrphans)


  return(RDBESDataObjectToFilterAndTidy)

}
