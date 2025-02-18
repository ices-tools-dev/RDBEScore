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
