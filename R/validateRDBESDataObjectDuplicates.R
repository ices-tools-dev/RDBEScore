#' check RDBES Raw Object Content
#'  Private function to do some basic checks on
#' the content of the RDBESDataObject (e.g. all required field names are
#' present).  Function is only used by checkRDBESDataObject and should only
#' be passed a list of non-null objects
#'
#' @param objectToCheck - RDBESDataObject i.e. a list of data.tables
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) Set to TRUE if you want to be sure all columns
#' are present in the data, set to FALSE if you only want to check that
#' essential columns are present.  The default is TRUE.
#'
#' @return list with  first element as the object and the second the warnings
#'
validateRDBESDataObjectDuplicates <- function(objectToCheck,
                                           verbose = FALSE,
                                           strict = TRUE) {
  validRDBESDataObject <- TRUE
  warningText <- NA


    # CHECK do any any tables have duplicate rows
  tablesWithDupes <- objectToCheck[
      sapply(objectToCheck, function(x) {
        any(duplicated(x))
      })
    ]

  if (length(tablesWithDupes) > 0) {
      validRDBESDataObject <- FALSE
      warningText <-
        paste("objectToCheck contains the following tables which have ",
          "duplicate rows: ",
          paste(names(tablesWithDupes), collapse = ","),
          sep = ""
        )
  } else {

      # CHECK See if the XXid fields contain duplicates
      # (only check this if we have already passed the first duplicate check)
      badEntries <- objectToCheck[
        sapply(objectToCheck, function(x) {
          returnValue <- FALSE
          # Assume the first field name accurately gives us the table name
          tableName <- substring(names(x)[1], 1, 2)
          idFieldName <- paste0(tableName, "id")
          returnValue <- any(duplicated(x[, idFieldName, with=FALSE]))
          returnValue
        })
      ]

      if (length(badEntries) > 0) {
        validRDBESDataObject <- FALSE
        warningText <-
          paste("objectToCheck contains the following tables which have ",
            "duplicated values in their XXid field: ",
            paste(names(badEntries), collapse = ","),
            sep = ""
          )
      }
  }


  # Return our outcome as a list
  list("validRDBESDataObject" = validRDBESDataObject,
       "warningText" = warningText)
}
