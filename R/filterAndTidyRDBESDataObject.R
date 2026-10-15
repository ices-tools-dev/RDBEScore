#' Filter and tidy an RDBESDataObject (deprecated)
#'
#' This deprecated function filters an RDBESDataObject, removes rows with broken
#' vessel and species-list links, and optionally removes downstream orphans.
#' It is retained for compatibility and will be removed in a future release.
#'
#' @section Migration:
#' Use [filterRDBESDataObject()] for filtering with downstream orphan removal.
#' To reproduce the full cleanup performed here, filter with
#' `killOrphans = FALSE`, call [removeBrokenVesselLinks()], then
#' [removeBrokenSpeciesListLinks()], and finally [findAndKillOrphans()] if
#' orphan removal is wanted. This explicit sequence preserves the distinction
#' between orphan removal and cleanup of vessel and species-list links.
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
#' # Migration: filter, remove broken links, then remove downstream orphans
#' H1DataObj <- H1Example
#' H1DataObjFiltered <- filterRDBESDataObject(
#'   H1DataObj, fieldsToFilter = "DEstratumName",
#'   valuesToFilter = "Pckg_SDAResources_agstrat_H1", killOrphans = FALSE
#' )
#' H1DataObjFiltered <- removeBrokenVesselLinks(H1DataObjFiltered)
#' H1DataObjFiltered <- removeBrokenSpeciesListLinks(H1DataObjFiltered)
#' H1DataObjFiltered <- findAndKillOrphans(H1DataObjFiltered)
#' summary(H1DataObjFiltered)
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Previous review: rix133 (examples only)
#' - Notes/scope:
filterAndTidyRDBESDataObject <- function(RDBESDataObjectToFilterAndTidy,
                                 fieldsToFilter,
                                 valuesToFilter,
                                 killOrphans = TRUE,
                                 verbose = FALSE,
                                 strict = TRUE)
  {

  .Deprecated(
    msg = paste0(
      "filterAndTidyRDBESDataObject() is deprecated and will be removed in a ",
      "future release. Use filterRDBESDataObject() for filtering and orphan ",
      "removal. For the full cleanup sequence, see ",
      "help('filterAndTidyRDBESDataObject')."
    )
  )

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
                                                      killOrphans = FALSE,
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
