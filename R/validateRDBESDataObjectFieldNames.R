#' check RDBES Data Object field names
#' Private function to do some checks on the columns of an RDBESDataObject -
#' 1) are all required fields present? 2) are there any extra fields present?
#' It is used by validateRDBESDataObject() and should only be passed a list
#' of non-null objects
#'
#' @param objectToCheck - RDBESDataObject i.e. a list of data.tables
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) Set to TRUE if you want to be sure all columns
#' are present in the data, set to FALSE if you only want to check that
#' essential columns are present.  The default is TRUE.
#'
#' @return list with  first element as a boolean indicating validity
#' and the second element contains any warnings
#'
validateRDBESDataObjectFieldNames <- function(objectToCheck,
                                           verbose = FALSE,
                                           strict = TRUE) {
  validRDBESDataObject <- TRUE
  warningText <- NA

  # For each entry see if they have the required, or extra, field names
  badEntries <- objectToCheck[!
  sapply(objectToCheck, function(x) {
    # Assume the first field name accurately gives us the table name
    tableName <- substring(names(x)[1], 1, 2)
    requiredColumnNames <-
      RDBEScore::mapColNamesFieldR[
        RDBEScore::mapColNamesFieldR$Table.Prefix == tableName,
        ]
    requiredColumnNames[is.na(requiredColumnNames$R.Name), "R.Name"] <-
      requiredColumnNames[is.na(requiredColumnNames$R.Name), "Field.Name"]
    # Only a subset of the columns are "essential" for estimation
    essentialColumnNames <-
      requiredColumnNames[requiredColumnNames$EssentialForEst == TRUE,"R.Name"]
    requiredColumnNames <- requiredColumnNames$R.Name

    allNeededColumns <- FALSE
    extraColumns <- FALSE

    if (strict){
      # If we're being strict - check all columns
      # Are all the required names present?
      if (all(requiredColumnNames %in% names(x))) {
        allNeededColumns <- TRUE
      } else {
        # Missing columns
        missingColumnNames <-
          requiredColumnNames[!requiredColumnNames %in% names(x)]
        print(paste("The following required columns are missing from table",
                    tableName,":",
                    paste(missingColumnNames,collapse = ",")))
      }
    } else {
      # If we're not being strict - just check the essential columns
      # Are all the essential names present?
      if (all(essentialColumnNames %in% names(x))) {
        allNeededColumns <- TRUE
      } else {
        # Missing columns
        missingColumnNames <-
          essentialColumnNames[!essentialColumnNames %in% names(x)]
        print(paste("The following essential columns are missing from table",
                    tableName,":",
                    paste(missingColumnNames,collapse = ",")))
      }
    }

    # Are there any extra columns in the data?
    extraColumnNames <-
      names(x)[!names(x) %in% requiredColumnNames]
    if (length(extraColumnNames) >0){
      extraColumns <- TRUE
      if (verbose){
        print(paste("The following unnecessary columns were found in table",
                    tableName,":",
                    paste(extraColumnNames,collapse = ",")))
      }
    }
    # Decide whether this was valid or not (default is invalid)
    valueToReturn <- FALSE
    if (allNeededColumns){
      if(!strict){
        # if we're not being strict then just having all needed cols is fine
        valueToReturn <- TRUE
      } else if (strict & !extraColumns){
        # if we are being strict we also check there are no extra cols
        valueToReturn <- TRUE
      }
    }
    valueToReturn
  })]

  # Check if there are any entries which have invalid field names
  if (length(badEntries) > 0) {
    validRDBESDataObject <- FALSE
    if (strict){
      warningTextPrefix <- paste0("objectToCheck contains the following ",
        "tables which either don't contain all required fields or they ",
        "include unnecessary columns ")
      if (!verbose){
        warningTextPrefix <- paste0(warningTextPrefix,
                            " (run with verbose = TRUE to see more details) " )
      }
    } else {
      warningTextPrefix <- paste0("objectToCheck contains the following ",
        "tables which don't contain all required fields: ")
    }
    warningText <-
      paste(warningTextPrefix,
        paste(names(badEntries), collapse = ","),
        sep = ""
      )
  }


  # Return our outcome as a list
  list("validRDBESDataObject" = validRDBESDataObject,
       "warningText" = warningText)
}
