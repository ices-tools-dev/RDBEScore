#' Convert List of Data Frames to a RDBES Data Object
#'
#' This function converts a list of data frames into an object of class
#' `RDBESDataObject`.
#'
#' @param myList A `list` of data tables. Each element of the list should be a
#'   data frame with an RDBES two-letter name (e.g. "DE").
#' @param castToCorrectDataTypes logical. Indicates whether to cast the columns
#'   to the correct data types. Default is `TRUE`.
#' @param strict logical. Indicates level of validation of the `RDBESDataObject`
#'   it creates - should the validation be strict? Default is `TRUE`.
#' @importFrom stats setNames
#'
#' @return An `RDBESDataObject` with each element being a data table.
#'
#' @keywords internal
#' @md
#'
#' @details Tables in the input list should have the correct 2-letter RDBES name
#'   (e.g. "DE", "LE", etc.). The function converts all data frames to
#'   `data.table`. `NULL` tables are left as `NULL`.
#'
#'   If `castToCorrectDataTypes = TRUE`, it ensures all columns are of the
#'   correct data type using `setRDBESDataObjectDataTypes`.
#'
#'   Column names are replaced with the RDBES 'R names' from the model
#'   documentation.
#'
#'   The function then sets a key on each table using the 'XXid' column as the
#'   key, where 'XX' is the name of that table. It also replaces all empty
#'   strings with `NA`.
#'
#'   It then uses the `newRDBESDataObject` function to create a new
#'   `RDBESDataObject` from the input.
#'
#'   Finally, it validates the RDBESDataObject using
#'   `RDBEScore::validateRDBESDataObject` and returns it.

importRDBESDataDFS <- function(myList,
                               castToCorrectDataTypes = TRUE,
                               strict = TRUE,
                               ...){
  # Checks for different names than the ones expected and duplicate table names in the list
  wrongNames <- setdiff(names(myList), unique(mapColNamesFieldR$Table.Prefix))
  dupNamesBool <- duplicated(names(myList))
  if(length(wrongNames) > 0) {
    stop("You have given list names that are not valid:\n",
         paste(wrongNames, collapse = ", "))
  }
  if(any(dupNamesBool)) {
    stop("You have given list names that have duplicate table names:\n",
         paste0(names(myList)[dupNamesBool], collapse = ", "))
  }


  dt <- RDBEScore::newRDBESDataObject(DE = makeDT(myList[["DE"]]),
                                      SD = makeDT(myList[["SD"]]),
                                      VS = makeDT(myList[["VS"]]),
                                      FT = makeDT(myList[["FT"]]),
                                      FO = makeDT(myList[["FO"]]),
                                      TE = makeDT(myList[["TE"]]),
                                      LO = makeDT(myList[["LO"]]),
                                      OS = makeDT(myList[["OS"]]),
                                      LE = makeDT(myList[["LE"]]),
                                      SS = makeDT(myList[["SS"]]),
                                      SA = makeDT(myList[["SA"]]),
                                      FM = makeDT(myList[["FM"]]),
                                      BV = makeDT(myList[["BV"]]),
                                      VD = makeDT(myList[["VD"]]),
                                      SL = makeDT(myList[["SL"]]),
                                      CL = makeDT(myList[["CL"]]),
                                      CE = makeDT(myList[["CE"]]))

  # Ensure all the columns are the correct data type
  if(castToCorrectDataTypes) dt <- setRDBESDataObjectDataTypes(dt)

  #the correct Name mapping has all Correct names both ways
  nameMap <- c(setNames(mapColNamesFieldR$R.Name, mapColNamesFieldR$Field.Name),
               setNames(mapColNamesFieldR$R.Name, mapColNamesFieldR$R.Name))
  nameMap <- nameMap[!duplicated(names(nameMap))]

  # Set a key on any data tables in myList - use the XXid column as the key
  for(aTable in names(dt)){
    #skip redundant tables
    # aTable = "DE"
    if(is.null(dt[[aTable]])){
      next
    } else {
      # SET KEY
      data.table::setkeyv(dt[[aTable]], paste0(aTable,"id")) # essentially orders rows by id column?
      # SET NAMES if/where needed
      oldNames <- data.table::copy(colnames(dt[[aTable]]))
      #essure that any strange/wrong names are retained in the result
      extraCols <- setdiff(oldNames, names(nameMap))
      names(extraCols) <- extraCols
      newNames <- c(nameMap, extraCols)
      data.table::setnames(dt[[aTable]], old = oldNames,
                           new = newNames[oldNames], skip_absent = TRUE)
      # SET all empty strings to NA
      # dt[[aTable]][dt[[aTable]]==""] <- NA
    }
  }

  #check the data
  validateRDBESDataObject(dt,
                          checkDataTypes = castToCorrectDataTypes,
                          strict = strict,
                          ...)

  return(dt)
}
