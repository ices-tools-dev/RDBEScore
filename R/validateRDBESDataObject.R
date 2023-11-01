#' Check Whether an `RDBESDataObject` is in a Valid Format
#'
#' Perform basic checks on a object.
#'
#' Checks if 'objectToCheck' parameter is valid. Returns the parameter if it is
#' valid and otherwise stops on error.
#' It checks the `RDBESDataObject` if:
#' * Is this an object of class RDBESDataObject
#' * Tables don't have column names that aren't allowed
#' * Tables have all the required  column names
#'
#' It does not check if the data is valid.
#' The RDBES upload system performs an extensive set of checks on the
#' uploaded data.
#'
#' @param objectToCheck RDBESDataObject i.e. a list of data.tables
#' @param checkDataTypes (Optional) Set to TRUE if you want to check that
#' the data types of the required columns are correct, or FALSE if you don't
#' care. Default value is FALSE.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) Set to TRUE if you want to be sure all columns
#' are present in the data, set to FALSE if you only want to check that
#' essential columns are present.  The default is TRUE.
#'
#' @return Returns objectToCheck
#' @md
#' @export
#' @aliases checkRDBESDataObject
#' @examples
#' \dontrun{
#' myH1RawObject <-
#' importRDBESDataCSV(rdbesExtractPath = "tests/testthat/h1_v_1_19")
#' validateRDBESDataObject(myH1RawObject)}
#'
validateRDBESDataObject <- function(objectToCheck,
                                checkDataTypes = FALSE,
                                verbose = FALSE,
                                strict = TRUE) {

  allowedNamesInList <- unique(RDBEScore::mapColNamesFieldR$Table.Prefix)

  # STEP 1 OF CHECKS

  # CHECK 1 Have we just been passed NA or NULL?
  if (length(is.na(objectToCheck)) == 1) {
    if (is.na(objectToCheck)){
      stop("objectToCheck is NA")
    }
    if (is.null(objectToCheck)){
      stop("objectToCheck is NULL")
    }
  }

  # CHECK 2 Is this an object of class RDBESDataObject?  It should be!
  if (! 'RDBESDataObject' %in% class(objectToCheck)) {
    stop("objectToCheck is not of the class RDBESDataObject")
  }

  # CHECK 3 Is this a list?  It should be!
  if (!(is.list(objectToCheck) & inherits(objectToCheck, "list"))) {
    stop("objectToCheck does not inherit from list")
  }

  # CHECK 4 Does this list have any names that aren't required?
  # (this is now only an error if we being strict)
  if (!all(names(objectToCheck) %in% allowedNamesInList)) {
    if (strict){
      stop(paste("objectToCheck is a list but has extra names ",
                 paste(names(objectToCheck), collapse = ","),
                 sep = ""
      ))
    }
  }

  # CHECK 5 Does the list have an entry for all the required names?
  if (!all(allowedNamesInList %in% names(objectToCheck))) {
      print(paste(names(objectToCheck), collapse = ","))
      stop(paste("objectToCheck is a list but does not contain ",
                 "all the required names ",
                 paste(names(objectToCheck), collapse = ","),
                 sep = ""
      ))
  }

  # STEP 2 OF CHECKS

  # Get any objectToCheck entries which aren't null or data tables
  badEntries <- objectToCheck[!
    sapply(
      objectToCheck,
      function(x) {
        returnValue <- FALSE
        if (length(x) == 0 & is.null(x)) {
          returnValue <- TRUE
        } else if ("data.table" %in% class(x)) {
          returnValue <- TRUE
        }
        returnValue
      }
    )]

  if (length(badEntries) > 0) {
    stop(paste("objectToCheck is a list but contains some entries which are ",
        "not NULL or data tables ",
        paste(names(badEntries), collapse = ","),
        sep = ""
    ))
  }


  # Print out null and empty entries for information
  if (verbose){
    nullEntries <- objectToCheck[sapply(objectToCheck, is.null)]
    if (length(nullEntries)>0){
      print(paste("Note that ",names(nullEntries)
                  ," is NULL but this is allowed in an RDBESDataObject"
                  , sep = ""))
    }

    emptyTables <- unlist(sapply(objectToCheck, function(x){nrow(x) == 0}))
    if (any(emptyTables)){
      print(paste("Note that ",names(emptyTables)
                  ," has 0 rows but this is allowed in an RDBESDataObject"
                  , sep = ""))
    }
  }


  # From now on we will just check the non-NULL entries
  nonNullEntries <- objectToCheck[sapply(objectToCheck, Negate(is.null))]

  # The next checks are only relevant if we don't have an empty object
  if (length(nonNullEntries) > 0) {

        warningText <- ""
        validRDBESDataObject <- TRUE

        # CHECK 6 Check that keys are set on the data tables
        for(aTable in names(nonNullEntries)){
            if (is.null(key(nonNullEntries[[aTable]]))){
              validRDBESDataObject <- FALSE
              warningText <- paste0(warningText, aTable, " does not have a key set. ")
            }
        }

        # CHECK 7 Check the field names
        myReturnValue <- validateRDBESDataObjectFieldNames(nonNullEntries,
                                          verbose = verbose,
                                          strict = strict)
        if (!is.na(myReturnValue[["warningText"]])){
          warningText <- paste0(warningText,myReturnValue[["warningText"]],". ")
        }
        if (!myReturnValue[["validRDBESDataObject"]]) {
          validRDBESDataObject <- FALSE
        }

        # CHECK 8 check whether there are any duplicates
        myReturnValue <- validateRDBESDataObjectDuplicates(nonNullEntries,
                                                        verbose = verbose,
                                                        strict = strict)
        if (!is.na(myReturnValue[["warningText"]])){
          warningText <- paste0(warningText,myReturnValue[["warningText"]],". ")
        }
        if (!myReturnValue[["validRDBESDataObject"]]) {
          validRDBESDataObject <- FALSE
        }

        # CHECK 9 (Optional)  check the data types of the columns
        if (checkDataTypes){
          myDiffs <- validateRDBESDataObjectDataTypes(nonNullEntries)
          numberOfDifferences <- nrow(myDiffs)
          if (numberOfDifferences >0 ){
            validRDBESDataObject <- FALSE
            warningText <- paste0(warningText,
              "objectToCheck has the following fields ",
              "with incorrect data types: ",
              paste(myDiffs[,"R.Name"], collapse = ","),
              sep = ""
              )
          }
        }
        if(!validRDBESDataObject & strict) {
          stop(warningText)
        }
        if(!validRDBESDataObject) {
          warning(warningText)
        }
  }

  return(invisible(objectToCheck));
}

#' @rdname validateRDBESDataObject
#' @export
checkRDBESDataObject <- validateRDBESDataObject
