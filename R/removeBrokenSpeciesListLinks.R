#' Remove SS rows with broken species-list links
#'
#' A species-list link matches the list name, catch fraction, design year
#' and sampling country against the SL table.
#'
#' @param objectToCheck an RDBESDataObject.
#' @param verbose (Optional) If set to TRUE more detailed text will be printed
#' out by the function.  Default is FALSE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return an RDBESDataObject with SS rows without a matching SL record removed.
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: rix133
#' - Notes/scope:
#'
#' @export
#'
#' @examples
#' H1DataObj <- H1Example
#' # Retain one species list, leaving selections linked to other lists
#' H1DataObjFiltered <- filterRDBESDataObject(
#'   H1DataObj, "SLspeclistName", "ZW_1965_SpeciesList2"
#' )
#' H1DataObjCleaned <- removeBrokenSpeciesListLinks(H1DataObjFiltered)
#' c(before = nrow(H1DataObjFiltered$SS), after = nrow(H1DataObjCleaned$SS))
#'
removeBrokenSpeciesListLinks <- function(objectToCheck,
                                        verbose = FALSE,
                                        strict = TRUE) {
  validateRDBESDataObject(objectToCheck, verbose = verbose, strict = strict)

  if (is.null(objectToCheck[["SL"]])) {
    stop(paste0("The SL entry in in objectToCheck is null - ",
                "cannot check for broken species list links"))
  }

  mySS <- objectToCheck[["SS"]]
  if (is.null(mySS) || nrow(mySS) == 0L) return(objectToCheck)

  myLinks <- data.table::data.table(
    SLspeclistName = mySS[["SSspecListName"]],
    SLcatchFrac = mySS[["SScatchFra"]],
    SLyear = extractHigherFields(objectToCheck, "SS", "DEyear"),
    SLcou = extractHigherFields(objectToCheck, "SS", "SDctry")
  )
  # One match per SS row is sufficient, even when several SL records match.
  myMatches <- objectToCheck[["SL"]][myLinks,
    on = c("SLspeclistName", "SLcatchFrac", "SLyear", "SLcou"),
    which = TRUE, mult = "first"]
  hasMatch <- !is.na(myMatches)

  if (verbose) {
    print(paste0("SS rows before: ", nrow(mySS),
                 "; removed: ", sum(!hasMatch),
                 "; remaining: ", sum(hasMatch)))
  }

  objectToCheck[["SS"]] <- mySS[hasMatch]
  objectToCheck
}

