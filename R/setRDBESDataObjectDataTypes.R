#' For a given RDBESDataObject convert the required
#' columns to the correct data types.  (This function can cause an error if we
#' have data in the columns that can't be cast to the desired data type.)
#'
#' @param RDBESDataObjectToConvert list - the raw item for conversion
#'
#' @return An RDBESDataObject with the correct date types for the required
#' fields
#'
#' @import data.table
setRDBESDataObjectDataTypes <- function(RDBESDataObjectToConvert){

  # For each entry in our list convert the columns to the correct format
  # This could cause an error if we have data in the columns that can't be
  # cast to the desired data type
  alteredObject <- lapply(RDBESDataObjectToConvert, function(x){
      # Only process the non-null entries
      if (!is.null(x)){
        # Assume the first field name accurately gives us the table name
        tableName <- substring(names(x)[1], 1, 2)
        # Find information for the relevent columns
        requiredColumns <-
          RDBEScore::mapColNamesFieldR[
            RDBEScore::mapColNamesFieldR$Table.Prefix == tableName, ]
        # Change to numeric
        myCols <-
          requiredColumns[requiredColumns$RDataType == "numeric","R.Name"]
        colsToChange <- names(x)[names(x) %in% myCols]
        if (length(colsToChange)>0){
          x[, colsToChange] <-
            x[, lapply(.SD, as.numeric), .SDcols = colsToChange]
        }
        # Change to integer
        myCols <-
          requiredColumns[requiredColumns$RDataType == "integer","R.Name"]
        colsToChange <- names(x)[names(x) %in% myCols]
        if (length(colsToChange)>0){
          x[, colsToChange] <-
            x[, lapply(.SD, as.integer.or.dbl), .SDcols = colsToChange]
        }

        # Change to character
        myCols <-
          requiredColumns[requiredColumns$RDataType == "character","R.Name"]
        colsToChange <- names(x)[names(x) %in% myCols]
        if (length(colsToChange)>0){
          x[, colsToChange] <-
            x[, lapply(.SD, as.character), .SDcols = colsToChange]
        }
      }
      x
    }
  )


  # Update the original object so we don't lose its class type
  for (aTable in names(RDBESDataObjectToConvert)){
    if (!is.null(alteredObject[[aTable]])){
      RDBESDataObjectToConvert[[aTable]] <- alteredObject[[aTable]]
      # SET KEY
      # essentially orders rows by id column?
      check_key_column(RDBESDataObjectToConvert[[aTable]], paste0(aTable,"id"))
      data.table::setkeyv(RDBESDataObjectToConvert[[aTable]], paste0(aTable,"id"))
    }

  }

  RDBESDataObjectToConvert


}
