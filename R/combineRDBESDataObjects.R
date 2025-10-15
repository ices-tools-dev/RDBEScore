#' Combine Two RDBES Raw Objects
#' combines 2 RDBESDataObjects into a single RDBESDataObject by merging individual
#' tables one by one
#'
#' @param RDBESDataObject1 The first object to combine
#' @param RDBESDataObject2 The second object to combine
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @details
#' When combining RDBESDataObjects from different hierarchies (e.g., H1 and H5),
#' a warning is issued. The resulting combined object will have a mixed hierarchy,
#' which may be structurally and statistically invalid for some analyses. However,
#' such combinations can be useful for fisheries overviews, annual reports, or
#' countries performing broader estimations.
#'
#' @return the combination of \code{RDBESDataObject1} and  \code{RDBESDataObject2}
#' @seealso  \link[data.table]{rbindlist}
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'     importRDBESDataCSV(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#' myH5RawObject <-
#'     importRDBESDataCSV(rdbesExtractPath = "tests\\testthat\\h5_v_1_19")
#'
#' myCombinedRawObject <- combineRDBESDataObjects(RDBESDataObject1=myH1RawObject,
#'                                              RDBESDataObject2=myH5RawObject)
#' }
combineRDBESDataObjects <- function(RDBESDataObject1,
                                    RDBESDataObject2,
                                    verbose = FALSE,
                                    strict = TRUE) {

  validateRDBESDataObject(RDBESDataObject1, verbose = verbose, strict = strict)
  validateRDBESDataObject(RDBESDataObject2, verbose = verbose, strict = strict)

  # Check for multiple hierarchies
  hierarchy1 <- NULL
  hierarchy2 <- NULL

  if (!is.null(RDBESDataObject1$DE) && nrow(RDBESDataObject1$DE) > 0) {
    hierarchy1 <- unique(RDBESDataObject1$DE$DEhierarchy)
  }

  if (!is.null(RDBESDataObject2$DE) && nrow(RDBESDataObject2$DE) > 0) {
    hierarchy2 <- unique(RDBESDataObject2$DE$DEhierarchy)
  }

  # Warn if combining different hierarchies
  if (!is.null(hierarchy1) && !is.null(hierarchy2) &&
      length(hierarchy1) > 0 && length(hierarchy2) > 0) {
    if (!all(hierarchy1 %in% hierarchy2) || !all(hierarchy2 %in% hierarchy1)) {
      warnMsg <- paste("Combining RDBESDataObjects from different hierarchies (",
              paste(hierarchy1, collapse = ", "), " and ",
              paste(hierarchy2, collapse = ", "),
              "). This creates a mixed hierarchy object that may be structurally ",
              "and statistically invalid for some analyses.")
      if(strict){
        stop(warnMsg, call. = F)
      } else{
        warning(warnMsg, call. = F)
      }
    }
  }

  # Create an empty RDBESDataObject as the basis of what we will return
  myRDBESDataObject <- createRDBESDataObject()

  # For each entry, bind the data tables together
  for (myTable in names(myRDBESDataObject)) {
    # Only bind the data tables if one of them is not null
    if (!(is.null(RDBESDataObject1[[myTable]]) &
      is.null(RDBESDataObject2[[myTable]]))) {
      myRDBESDataObject[[myTable]] <-
        data.table::rbindlist(list(
          RDBESDataObject1[[myTable]],
          RDBESDataObject2[[myTable]]
        ),
        use.names = T, fill = T
        )
      # Need to re-set the key because rbindlist loses it...
      data.table::setkeyv(myRDBESDataObject[[myTable]],paste0(myTable,"id"))


      # De-duplicate the resulting SL,IS,VD,CL,and CE tables
      if (myTable %in% c('VD','SL','IS','CL','CE')){
        # Note - uniqueness will be based only on the data table key
        # (data.table now needs this behaviour set explicitly using by=...)
        myRDBESDataObject[[myTable]] <- unique(myRDBESDataObject[[myTable]]
                                      , by = key(myRDBESDataObject[[myTable]]))
        #myRDBESDataObject[[myTable]] <-
        #  dplyr::distinct(myRDBESDataObject[[myTable]], .keep_all = TRUE)
      }

    }
  }

  myRDBESDataObject
}
