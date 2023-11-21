#' Summarise the selection methods in an RDBESDataObject
#'
#' @param objectToSummarise An RDBESDataObject that you want to summarise the
#' selection methods used
#' @param yearToUse The year to summarise the data for
#' @param country The country to summarise the data for
#' @param hierarchyToSummarise The hierarchy to summarise the data for
#'
#' @return A list containing:
#' i) summaryDataByStrataAndMethod: A list of the tables in the RDBESDataObject
#' grouped by strata and selection method, with the number of rows, number
#' of units sampled, and number of units not sampled calculated
#' ii) summaryDataJoined: The data from (i) joined in a single data frome,
#' using strata names as the link
#' iii) selectionMethodSummary: A list of data frames with summary information
#' about the selection methods used in each level of the sampling.
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'   createRDBESDataObject(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#'
#'
#' mySummary <- summariseSelectionMethods(
#'   objectToSummarise = myH1RawObject,
#'   yearToUse = 1965,
#'   country = "ZW",
#'   hierarchyToSummarise = "H1"
#' )
#' }
summariseSelectionMethods <- function(objectToSummarise,
                                      yearToUse,
                                      country,
                                      hierarchyToSummarise) {



  # For testing - to be removed
  # myRDBESData <-
  #      createRDBESDataObject(""..\\..\\..\\tests\\testthat\\h1_v_1_19"")
  # objectToSummarise <- myRDBESData
  # yearToUse <- 1965
  # country <- 'ZW'
  # hierarchyToSummarise <- 'H1'


  print(paste0(
    "This function assumes that FT data is present for hierarchy",
    " 1. It also does not summarise the SS table or below.  Please also be ",
    " aware that this function does not consider the clustered",
    " sampling variables yet"
  ))

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(objectToSummarise, verbose = FALSE)

  requiredTables <- RDBEScore::tablesInRDBESHierarchies

  # Find which tables we need for this file type
  upperHierarchy <- substr(hierarchyToSummarise, 2, nchar(hierarchyToSummarise))
  myRequiredTables <- requiredTables[[hierarchyToSummarise]]

  ## Step 1 - Filter the data
  # Filter by year, country, and hieracrchy at the same time
  # - safe because we know the fields won't have the same values
  myCSData <- filterRDBESDataObject(objectToSummarise,
    fieldsToFilter = c("DEhierarchy", "DEyear", "SDctry"),
    valuesToFilter = c(yearToUse, country, upperHierarchy)
  )

  # Make sure we don't have any orphan records
  myCSData <- RDBEScore::findAndKillOrphans(myCSData, verbose = FALSE)


  # Lists to hold the outputs
  myData <- list()
  myDataGroupedStrataMethod <- list()
  myDataGroupedMethod <- list()
  myJoinedData <- NULL
  previousRequiredTable <- NULL

  # STEP 2 Process each table to get the data in the format we need
  # We won't worry about SS,SA,FM,BV at this point
  for (aRequiredTable in myRequiredTables
  [!myRequiredTables %in% c("SS", "SA", "FM", "BV")]) {

    # aRequiredTable <- 'DE'

    # Get the data we will work with
    myTempData <- myCSData[[aRequiredTable]]

    # Process the different tables
    #
    # We need particular fields for DE
    if (aRequiredTable == "DE") {
      # Create a stratum name for DE
      myTempData$DEfullStratumName <- paste(
        myTempData[, "DEsampScheme"][[1]], myTempData[, "DEyear"][[1]],
        myTempData[, "DEstratumName"][[1]],
        sep = "_"
      )

      # Save the data to our list (DE doesn't need to be grouped)
      myData[[aRequiredTable]] <-
        myTempData[, c(
          "DEid", "DEstratumName", "DEfullStratumName",
          "DEsampScheme", "DEyear", "DEhierarchyCor", "DEhierarchy"
        )]
      myDataGroupedStrataMethod[[aRequiredTable]] <-
        myData[[aRequiredTable]][, c(
          "DEstratumName", "DEfullStratumName",
          "DEsampScheme", "DEyear", "DEhierarchyCor", "DEhierarchy"
        )]
      myDataGroupedMethod[[aRequiredTable]] <-
        myDataGroupedStrataMethod[[aRequiredTable]] %>%
        dplyr::group_by(DEyear, DEhierarchyCor, DEhierarchy) %>%
        dplyr::summarise(
          DEstratumName =
            paste(unique(DEstratumName), collapse = ",")
        )

      # We need particualr fields for SD
    } else if (aRequiredTable == "SD") {

      # Join SD to the previous table (DE in this case)
      previousHierarchyTable <- myData[[previousRequiredTable]]
      ## Assume the primary key is the first field
      previousPrimaryKey <- names(previousHierarchyTable)[1]
      currentPrimaryKey <- names(myTempData)[1]
      myTempData <- inner_join(myTempData, previousHierarchyTable,
        by = previousPrimaryKey
      )

      # Rename the stratum from DE as parent stratum name
      names(myTempData)[names(myTempData) == "DEfullStratumName"] <-
        "SDparentFullStratumName"

      # Create a stratum name and full stratum names for SD
      myTempData$SDstratumName <- paste(myTempData[, "SDctry"][[1]],
        myTempData[, "SDinst"][[1]],
        sep = "_"
      )
      myTempData$SDfullStratumName <-
        paste(myTempData[, "SDparentFullStratumName"][[1]],
          myTempData[, "SDstratumName"][[1]],
          sep = ":"
        )

      # Save the data to our list
      myData[[aRequiredTable]] <- myTempData[,
        c(
          currentPrimaryKey, previousPrimaryKey, "SDparentFullStratumName",
          "SDstratumName", "SDfullStratumName", "SDctry", "SDinst"
        ),
        with = FALSE
      ]
      myDataGroupedStrataMethod[[aRequiredTable]] <-
        myData[[aRequiredTable]][, c(
          "SDparentFullStratumName", "SDstratumName",
          "SDfullStratumName", "SDctry", "SDinst"
        )]
      # Group SD by country and institute (1 row for each country with
      # institute concatenated)
      myDataGroupedMethod[[aRequiredTable]] <-
        myDataGroupedStrataMethod[[aRequiredTable]] %>%
        dplyr::group_by(SDctry) %>%
        dplyr::summarise(SDinst = paste(unique(SDinst), collapse = ","))
    } else {

      # Get the names of the fields we are interested in
      currentStratificationField <-
        paste(aRequiredTable, "stratification", sep = "")
      currentStratumNameField <-
        paste(aRequiredTable, "stratumName", sep = "")
      currentNumberSampledField <-
        paste(aRequiredTable, "numSamp", sep = "")
      currentNumberTotalField <-
        paste(aRequiredTable, "numTotal", sep = "")
      currentSelectionMethodField <-
        paste(aRequiredTable, "selectMeth", sep = "")
      currentSampledField <-
        paste(aRequiredTable, "samp", sep = "")
      currentFullStratumNameField <-
        paste(aRequiredTable, "fullStratumName", sep = "")
      currentParentFullStratumName <-
        paste(aRequiredTable, "parentFullStratumName", sep = "")

      # Join to the previous table
      previousHierarchyTable <- myData[[previousRequiredTable]]
      ## Assume the primary key is the first field
      previousPrimaryKey <- names(previousHierarchyTable)[1]
      currentPrimaryKey <- names(myTempData)[1]
      myTempData <- inner_join(myTempData, previousHierarchyTable,
        by = previousPrimaryKey
      )

      # Rename the fullStratumName from the previous table as
      # parentFullStratumName
      names(myTempData)[names(myTempData) == paste(
        previousRequiredTable, "fullStratumName",
        sep = ""
      )] <-
        currentParentFullStratumName

      # Need to modify the stratum name values for unstratified data to include
      # the parent xxID value (otherwise all 'U' records will look like they are
      # are from the same stratum)
      unStratifiedRecords <-
        as.vector(myTempData[, ..currentStratificationField] == "N")
      newStratumNames1 <-
        myTempData[unStratifiedRecords, ..currentStratumNameField][[1]]
      newStratumNames2 <-
        myTempData[unStratifiedRecords, ..previousPrimaryKey][[1]]
      newStratumNames <-
        paste(newStratumNames1, "(", newStratumNames2, ")", sep = "")
      myTempData[unStratifiedRecords, currentStratumNameField] <-
        newStratumNames

      # Create the full stratum name field
      myTempData[, currentFullStratumNameField] <-
        paste(myTempData[, ..currentParentFullStratumName][[1]],
              myTempData[, ..currentStratumNameField][[1]], sep = ":")


      # Save the data to our list
      myData[[aRequiredTable]] <-
        myTempData[, c(
          currentPrimaryKey, previousPrimaryKey,
          currentParentFullStratumName, currentStratumNameField,
          currentFullStratumNameField, currentStratificationField,
          currentNumberSampledField, currentNumberTotalField,
          currentSelectionMethodField, currentSampledField
        ),
        with = FALSE
        ]

      # Now we will group and summarise the data

      # Get the data we want to group
      myTempToGroup <- myData[[aRequiredTable]][,
        c(
          currentParentFullStratumName,
          currentStratumNameField,
          currentFullStratumNameField,
          currentStratificationField,
          currentSelectionMethodField,
          currentSampledField,
          currentNumberSampledField,
          currentNumberTotalField
        ),
        with = FALSE
      ]

      # CHanges names so we can hard-code them below
      # - dplyr can't seem to cope with variables - grrr
      names(myTempToGroup) <-
        substr(names(myTempToGroup), 3, nchar(names(myTempToGroup)))

      # Add fields to hold the calcualted number of things
      # we did and didn't sample
      myTempToGroup$numberNotSampledCalc <- NA
      myTempToGroup$numberSampledCalc <- NA

      myTempToGroup[myTempToGroup$samp == "N" &
        !is.na(myTempToGroup$samp), "numberNotSampledCalc"] <- 1
      myTempToGroup[myTempToGroup$samp == "Y" &
        !is.na(myTempToGroup$samp), "numberSampledCalc"] <- 1


      # Group 1) - Group by strata and selection method and summarise
      myDataGroupedStrataMethod[[aRequiredTable]] <- myTempToGroup %>%
        dplyr::group_by(
          parentFullStratumName,
          stratumName,
          fullStratumName,
          stratification,
          selectMeth
        ) %>%
        dplyr::summarise(
          numberOfRows = n(),
          numberNotSampledCalc =
            sum(!is.na(numberNotSampledCalc)),
          numberSampledCalc = sum(!is.na(numberSampledCalc))
        )

      names(myDataGroupedStrataMethod[[aRequiredTable]]) <-
        paste(aRequiredTable,
              names(myDataGroupedStrataMethod[[aRequiredTable]]), sep = ""
        )

      # Group 2) - Group by selection method and summarise
      myDataGroupedMethod[[aRequiredTable]] <- myTempToGroup %>%
        dplyr::group_by(selectMeth) %>%
        dplyr::summarise(
          numberOfRows = n(),
          numberNotSampledCalc =
            sum(!is.na(numberNotSampledCalc)),
          numberSampledCalc = sum(!is.na(numberSampledCalc))
        )

      names(myDataGroupedMethod[[aRequiredTable]]) <-
        paste(aRequiredTable, names(myDataGroupedMethod[[aRequiredTable]]),
          sep = ""
        )
    }

    previousRequiredTable <- aRequiredTable
  }

  # We'll nor produce some summaries of the data

  myTotalSummary <- NULL
  myJoinedData <- NULL
  previousRequiredTable <- NULL

  # Now join our grouped data together
  for (aRequiredTable in
    myRequiredTables[!myRequiredTables %in% c("SS", "SA", "FM", "BV")]) {
    myTotalSummary[[aRequiredTable]] <- myDataGroupedMethod[[aRequiredTable]]

    if (aRequiredTable == "DE") {
      myJoinedData <- myDataGroupedStrataMethod[["DE"]]
    } else {
      previousTableJoinField <-
        paste(previousRequiredTable, "fullStratumName", sep = "")
      currentTableJoinField <-
        paste(aRequiredTable, "parentFullStratumName", sep = "")
      myJoinedData <-
        dplyr::left_join(myJoinedData,
          myDataGroupedStrataMethod[[aRequiredTable]],
          by =
            setNames(
              currentTableJoinField,
              previousTableJoinField
            )
        )


    }

    previousRequiredTable <- aRequiredTable
  }

  # Return values
  list(
    summaryDataByStrataAndMethod = myDataGroupedStrataMethod,
    summaryDataJoined = myJoinedData,
    selectionMethodSummary = myTotalSummary
  )
}
