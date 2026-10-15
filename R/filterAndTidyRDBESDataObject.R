#' Filter and remove orphan records in an RDBESDataObject
#'
#' This function filters an RDBESDataObject based on specified fields and values, and can optionally remove any orphan records.
#' The returned object will include all rows which either: a) do not include any of the field names in `fieldsToFilter`, or b) do include the field names and have one of the allowed values in `valuesToFilter`.
#' If `killOrphans` is set to `TRUE`, the function will remove orphaned rows. The default is `TRUE`.
#'
#' @param RDBESDataObjectToFilterAndTidy The RDBESDataObject to filter.
#' @param fieldsToFilter A vector of the field names you wish to check.
#' @param valuesToFilter A vector of the field values you wish to filter for.
#' @param killOrphans Controls if orphan rows are removed. Default is TRUE.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return The filtered input object of the same class as `RDBESDataObjectToFilterAndTidy`.
#'
#' @export
#' @md
#'
#' @examples
#' H1DataObj <- H1Example
#' # Keep a named design and remove the records no longer linked to it
#' H1DataObjFiltered <- filterAndTidyRDBESDataObject(
#'   H1DataObj, fieldsToFilter = "DEstratumName",
#'   valuesToFilter = "Pckg_SDAResources_agstrat_H1"
#' )
#' summary(H1DataObjFiltered)
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: rix133
#' - Notes/scope: examples only
filterAndTidyRDBESDataObject <- function(RDBESDataObjectToFilterAndTidy,
                                 fieldsToFilter,
                                 valuesToFilter,
                                 killOrphans = TRUE,
                                 verbose = FALSE,
                                 strict = TRUE)
  {

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(RDBESDataObjectToFilterAndTidy,
                          verbose = verbose,
                          strict = strict)

  # 1 - filter
  # If now fields/values to filter, then the same object is returned; else filtering
  if (!(missing(fieldsToFilter) | missing(valuesToFilter)))
    RDBESDataObjectToFilterAndTidy <-
      filterRDBESDataObject(RDBESDataObjectToFilterAndTidy,
                                                      fieldsToFilter,
                                                      valuesToFilter,
                                                      verbose = verbose,
                                                      strict = strict)
  # 2 - remove broken vessels links
    RDBESDataObjectToFilterAndTidy <-
      removeBrokenVesselLinks(RDBESDataObjectToFilterAndTidy,
                              verbose = verbose,
                              strict = strict)

  # 2a - remove any broken species list links
    RDBESDataObjectToFilterAndTidy <-
      removeBrokenSpeciesListLinks(RDBESDataObjectToFilterAndTidy,
                              verbose = verbose,
                              strict = strict)



  # 3 - remove orphans
  # Remove orphans after filtering
  if (killOrphans == TRUE)
      RDBESDataObjectToFilterAndTidy <-
      findAndKillOrphans(RDBESDataObjectToFilterAndTidy,
                         verbose = verbose,
                         strict = strict)


  return(RDBESDataObjectToFilterAndTidy)

}
