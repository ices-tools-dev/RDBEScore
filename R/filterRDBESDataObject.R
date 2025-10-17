#' Filter an RDBESDataObject
#'
#' The returned object will include all rows which either: a) do not included
#' any of the field names in `fieldsToFilter`, or b) do include the field names
#' and have one of the allowed values in `valuesToFilter`.
#' If you want to filter for a id field like `DEid`, `FTid` etc, the filtering
#' works only on the table where the id field is its key. For example, if you
#' try to filter on `FOid` it does not look  `FOid` in other tables like `FT`,
#' although the field `FOid` exists in `FT` table.
#'
#' `killOrphans` allows you to remove orphaned rows if set to `TRUE`. The
#' default is `FALSE`.
#'
#' @param RDBESDataObjectToFilter The `RDBESDataObject` to filter
#' @param fieldsToFilter A vector of the field names you wish to check
#' @param valuesToFilter A vector of the field values you wish to filter for
#' @param killOrphans Controls if orphan rows are removed. Default is `FALSE`.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return the filtered input object of the same class as
#'   `RDBESDataObjectToFilter`
#'
#' @export
#' @md
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'   importRDBESDataCSV(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#'
#' myFields <- c("SDctry", "VDctry", "VDflgCtry", "FTarvLoc")
#' myValues <- c("ZW", "ZWBZH", "ZWVFA")
#'
#' myFilteredObject <- filterRDBESDataObject(myH1RawObject,
#'   fieldsToFilter = myFields,
#'   valuesToFilter = myValues
#' )
#'
#' # Inverse filtering (exclude certain values)
#' # Example: keep all DE rows except those with DEid in `excludedValues`
#' # Compute the complement of the excluded set using setdiff
#' allValues <- unique(myH1RawObject$DE$DEid)
#' excludedValues <- c(5351)
#' myInverseFiltered <- filterRDBESDataObject(
#'   myH1RawObject,
#'   fieldsToFilter = "DEid",
#'   valuesToFilter = setdiff(allValues, excludedValues)
#' )
#' }
filterRDBESDataObject <- function(RDBESDataObjectToFilter,
                                 fieldsToFilter,
                                 valuesToFilter,
                                 killOrphans = FALSE,
                                 verbose = FALSE,
                                 strict = TRUE) {

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(RDBESDataObjectToFilter,
                          verbose = verbose,
                          strict = strict)

  # Check if the requested columns actually exist in the object
  allColNames <- unlist(lapply(RDBESDataObjectToFilter, names))
  missingFields <- fieldsToFilter[!fieldsToFilter %in% allColNames]
  if (length(missingFields) > 0) {
    warning(paste0(
      "The following fields were not found in the ",
      "RDBESDataObject: ",
      missingFields
    ))
  }

  tblNames <- names(RDBESDataObjectToFilter)
  alteredObject <- mapply(function(x, name) {
    # do not search id columns that are not ids of this table
    # see issue #183
    idCols <- names(x)[grepl("id$", names(x))]
    searchNames <- c(setdiff(names(x), idCols), paste0(name, "id"))
    foundNames <- searchNames[which(searchNames %in% fieldsToFilter)]
    if (length(foundNames) > 0) {
      keep <- x[, Reduce(`&`, lapply(.SD, function(col) col %in% valuesToFilter)), .SDcols = foundNames]
      x <- x[keep]
    }
    x
  }, RDBESDataObjectToFilter, tblNames, SIMPLIFY = FALSE)
  names(alteredObject) <- tblNames


  # Update the original object so we don't lose its class type
  for (myTable in names(RDBESDataObjectToFilter)) {
    if (!is.null(alteredObject[[myTable]])) {
      RDBESDataObjectToFilter[[myTable]] <- alteredObject[[myTable]]
    }
  }

  # remove orphans
  if (killOrphans == TRUE) RDBESDataObjectToFilter <- findAndKillOrphans(RDBESDataObjectToFilter, verbose = verbose, strict = strict)

  #
  return(RDBESDataObjectToFilter)
}
