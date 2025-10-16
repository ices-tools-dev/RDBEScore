#' Checks the data types of the columns in an
#' RDBESDataObject against an expected list of data types.  Any differences
#' are returned
#'
#' @param objectToCheck An RDBESDataObject to check
#'
#' @return A data frame containing any data type differences (an empty data
#' frame if there are no differences)
#'
validateRDBESDataObjectDataTypes <- function(objectToCheck){

  # Get the data types of all our columns
  myDataTypes <- lapply(objectToCheck, function(x){
    myTypes <- lapply(x,class)
    myTypes
  }
  )


  # Find any differences in actual and expected data types
  myDiffs <- lapply(names(myDataTypes),function(x){

    # Create an empty data frame with the right stucture
    myDiffs <- stats::setNames(data.frame(matrix(ncol = 6, nrow = 0)),
                        c("Table.Prefix","Field.Name","R.Name",
                          "Type","RDataType.x","RDataType.y"))
    # This is what we got
    myActualCols <- as.data.frame(do.call(rbind, myDataTypes[[x]]))

    # If this table isn't null in the list
    if(nrow(myActualCols)>0){

      myActualCols <-
        cbind(rownames(myActualCols), data.frame(myActualCols, row.names=NULL))
      names(myActualCols) <- c("FieldName","RDataType")

      # This is what we expect
      myRequiredCols <- RDBEScore::mapColNamesFieldR[
        RDBEScore::mapColNamesFieldR$Table.Prefix == x,
        ]

      # Compare the two lists of column data types
      myColCompare <-
        dplyr::inner_join(myRequiredCols,myActualCols,
                          by=c("R.Name"="FieldName"))

      # Get the rows that are different (ignoring the rows where we don't
      # know the data types)
      myDiffs <- myColCompare[!is.na(myColCompare$RDataType.x) &
                                !is.na(myColCompare$RDataType.y) &
                                myColCompare$RDataType.x != myColCompare$RDataType.y,]

    }
    myDiffs
  })

  # Put all the differences together
  myDiffs <- do.call("rbind", myDiffs)

  # Make some friendlier column names
  names(myDiffs)[which(names(myDiffs) == "RDataType.x")] <- "RDataType_expected"
  names(myDiffs)[which(names(myDiffs) == "RDataType.y")] <- "RDataType_actual"

  # Treat integer and numeric as compatible in both directions (issue #251)
  myDiffs <- myDiffs[!
    ((myDiffs$RDataType_expected == "numeric" & myDiffs$RDataType_actual == "integer") |
     (myDiffs$RDataType_expected == "integer" & myDiffs$RDataType_actual == "numeric"))
  ,]

  # Return the differences
  myDiffs

}
