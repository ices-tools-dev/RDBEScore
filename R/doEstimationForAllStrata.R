#' Estimate totals and means, and try to generate samples variances for all
#' strata in an RDBESEstObject
#'
#' @param RDBESEstObjectForEstim The RDBESEstObject to generate estimates for
#' @param verbose (Optional) If set to TRUE more detailed text will be printed
#' out by the function.  Default is FALSE
#' @param targetValue The field to estimate for, for example "SAsampWtLive"
#'
#' @return A data frame containing estimates for all strata
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'   importRDBESDataCSV(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#'
#' # Update our test data with some random sample measurements
#' myH1RawObject[["SA"]]$SAsampWtLive <-
#'   round(runif(n = nrow(myH1RawObject[["SA"]]), min = 1, max = 100))
#'
#' myH1EstObj <- createRDBESEstObject(myH1RawObject, 1)
#'
#' myStrataEst <- doEstimationForAllStrata(
#'   RDBESDataObjectForEstim = myH1EstObj,
#'   targetValue = 'SAsampWtLive'
#' )
#' }
doEstimationForAllStrata <- function(RDBESEstObjectForEstim,
                                     targetValue,
                                     verbose = FALSE) {



  # Check we have a valid RDBESEstObject before doing anything else
  RDBEScore::validateRDBESEstObject(RDBESEstObjectForEstim, verbose = verbose)

  #do clustering
  handle_clustering(RDBESEstObjectForEstim)

  # Clear out the variable that will hold our results
  myStrataResults <- NULL

  foundTargetValue <- FALSE

  # Find what tables we need
  suLevels <-
    names(RDBESEstObjectForEstim)[
      grep("^su.table$", names(RDBESEstObjectForEstim))
    ]
  tablesToCheck <- c("DE", "SD")
  # TODO I'm sure there's a better way to do this...
  for (myLevel in suLevels){
    myValues <- RDBESEstObjectForEstim[, ..myLevel]
    myValues <- na.omit(myValues)
    data.table::setorder(myValues)
    tablesToCheck <- c(tablesToCheck,myValues[1,][[1]])
  }
  tablesToCheck <- na.omit(tablesToCheck)
  tablesToCheck <- unique(tablesToCheck)

  suLevels <- gsub("table", "", suLevels)

  # Loop through our tables, starting at the right and working to left
  for (i in length(tablesToCheck):1) {
    currentTable <- tablesToCheck[i]
    results <- process_table(i, tablesToCheck, suLevels, targetValue, RDBESEstObjectForEstim, myStrataResults, foundTargetValue, verbose)
    myStrataResults <- results$myStrataResults
    myTable <- results$myTable
    foundTargetValue <- results$foundTargetValue

    # See we if already have some results - if we don't have any results yet we
    # can't join to previous values
    if (length(is.null(myStrataResults)) == 1 && is.null(myStrataResults)) {
      myTableWithValues <- myTable

      # if we do have previous results lets join our table with them
    } else {

      # Get the results from the previous table - we'll use these as inputs
      # for the estimation
      myPrevEstimates <- myStrataResults[parentTable == currentTable,
                                         .(studyVariable = sum(est.total, na.rm = TRUE)),
                                         by = parentTableID]



      # Join our current table with the previous results
      data.table::setkey(myTable, id)
      data.table::setkey(myPrevEstimates, parentTableID)

      myTableWithValues <- myTable[myPrevEstimates]

      # Join myStrataResults with myTable
      myStrataResults <- myTable[,
                                 .(id, parentIDandStratum)][myStrataResults,
                                                            on = .(id = parentTableID)]
      if("i.id" %in% colnames(myStrataResults)){
        myStrataResults <- myStrataResults[parentTable == currentTable,
                                           id := i.id]
        myStrataResults <-  myStrataResults[, i.id := NULL]

      }

      # Update the parentTableStratum column of myStrataResults only for the rows where parentTable equals currentTable
      myStrataResults$parentTableStratum <- as.character(myStrataResults$parentTableStratum)
      myStrataResults <- myStrataResults[parentTable == currentTable,
                                         parentTableStratum := i.parentIDandStratum]
      nullCols <- c("i.parentIDandStratum", "parentIDandStratum","id")
      myStrataResults <-  myStrataResults[, (nullCols) := NULL]

    }

    myResultsTemp <- handle_table(myTableWithValues, currentTable, myTable)


    # Combine the results from this loop with all the previous results
    myStrataResults <- data.table::rbindlist(list(myStrataResults, myResultsTemp), fill=T)
  }


  as.data.frame(myStrataResults)
}


# Define a function to add a column if it doesn't exist
add_column_if_not_exists <- function(df, column_name, default_value = NA) {
  if (!(column_name %in% names(df))) {
    df[[column_name]] <- default_value
  }
  df
}

# Define the main function
get_vars_needed <- function(currentTable, parentTable, suLevels, i, targetValue) {
  if (currentTable == "SD") {
    varsNeeded <- paste0(currentTable, c("id", "recType", "ctry", "inst"))
    varsNeeded <- c(varsNeeded, paste0(parentTable, "id"))
  } else if (currentTable == "DE") {
    varsNeeded <- paste0(currentTable, c(
      "id", "recType", "sampScheme",
      "year", "stratumName",
      "hierarchy", "samp"
    ))
  } else {
    varsNeeded1 <- paste0(suLevels[i - 2], c(
      "stratification", "stratumName",
      "selectMeth", "numTotal",
      "numSamp", "selProb",
      "incProb", "samp"
    ))
    varsNeeded2 <- paste0(currentTable, c("id", "recType"))
    varsNeeded <- c(varsNeeded1, varsNeeded2, paste0(parentTable, "id"))
    if (currentTable == "SA") {
      varsNeeded <- c(varsNeeded, targetValue)
    }
  }
  return(varsNeeded)
}

# Define the main function
handle_table <- function(myTableWithValues, currentTable, myTable) {
  # DE/SD/SS need to be handled differently because we can't estimate with these tables
  if (currentTable %in% c("DE", "SD")) {
    # Add on a parentTableID and stratumName columns if they don't exist
    myTableWithValues <- add_column_if_not_exists(myTableWithValues, "parentTableID")
    myTableWithValues <- add_column_if_not_exists(myTableWithValues, "stratumName")

    # Can't estimate with these tables - just sum up the values
    myResultsTemp <- myTableWithValues[, .(
      est.results.available = FALSE,
      est.total = sum(studyVariable, na.rm = TRUE),
      est.mean = NA_real_,
      var.total = NA_real_,
      var.mean = NA_real_,
      se.total = NA_real_,
      se.mean = NA_real_
    ), by = .(recType, parentTable, parentTableID, parentTableStratum, stratumName, parentIDandStratum)]

  } else {

    # For the other tables we'll run the estimation function
    myTableWithValues <- as.data.frame(myTableWithValues)
    # Split by parent ID and stratum name
    myTableList <- split(myTableWithValues, f = myTable$parentIDandStratum)
    # apply estimate function to each unique parent ID and stratum name
    # combination
    myResults <- lapply(myTableList, getEstimForStratum)
    # Combine our results into a data frame
    myResultsTemp <- do.call(rbind, myResults)

  }

  data.table::setDT(myResultsTemp)
}


#' Private function used by doEstimationForAllStrata to get the estimates
#'
#' @param x The input
#'
#' @return Data frame with estimated values
#'
getEstimForStratum <- function(x) {
  myReturnValues <- data.frame(
    "recType" = unique(x$recType),
    "parentTable" = unique(x$parentTable),
    "parentTableID" = unique(x$parentTableID),
    "parentTableStratum" = unique(x$parentTableStratum),
    "stratumName" = unique(x$stratumName),
    "parentIDandStratum" = unique(x$parentIDandStratum)
  )
  myEstim <- NA
  try(
    myEstim <- RDBEScore::estimMC(
      x$studyVariable,
      x$numSamp,
      x$numTotal,
      unique(x$selectMeth),
      x$selProb,
      x$incProb
    )
  )
  if (length(is.na(myEstim)) == 1 && is.na(myEstim)) {
    myReturnValues$est.results.available <- FALSE
    myReturnValues$est.total <- NA
    myReturnValues$est.mean <- NA
    myReturnValues$var.total <- NA
    myReturnValues$var.mean <- NA
    myReturnValues$se.total <- NA
    myReturnValues$se.mean <- NA
  } else {
    myReturnValues$est.results.available <- TRUE
    myReturnValues$est.total <- myEstim$est.total
    myReturnValues$est.mean <- myEstim$est.mean
    myReturnValues$var.total <- myEstim$var.total
    myReturnValues$var.mean <- myEstim$var.mean

    # Calculate the standard error from the variance
    if (is.numeric(myEstim$var.total) &&
        !is.nan(myEstim$var.total) &&
        !is.na(myEstim$var.total) &&
        myEstim$var.total > 0) {
      myReturnValues$se.total <- sqrt(myEstim$var.total)
    } else {
      myReturnValues$se.total <- NA
    }

    if (is.numeric(myEstim$var.mean) &&
        !is.nan(myEstim$var.mean) &&
        !is.na(myEstim$var.mean) &&
        myEstim$var.mean > 0) {
      myReturnValues$se.mean <- sqrt(myEstim$var.mean)
    } else {
      myReturnValues$se.mean <- NA
    }

  }
  # Get rid of row names
  rownames(myReturnValues) <- NULL
  myReturnValues
}

handle_clustering <- function(RDBESEstObjectForEstim){
  # Stop if we have any clustering - not developed yet
  clustFields <-
    names(RDBESEstObjectForEstim)[grep(
      "^su.clustering$",
      names(RDBESEstObjectForEstim)
    )]
  if (any(!is.na(RDBESEstObjectForEstim[, ..clustFields]) &
          RDBESEstObjectForEstim[, ..clustFields] != "N")) {
    stop("Clustering is present in the data - this cannot be handled yet.")
  }
}

add_extra_columns <- function(myTable, currentTable) {
  # Make some changes for specific tables
  if (currentTable == "DE") {
    # Combine the year with the stratum name
    myTable$stratumName <- paste0(myTable$year, "-", myTable$stratumName)
  }
  if (currentTable == "SD") {
    # Create a pseudo-stratum name with the country and institute code
    myTable$stratumName <- paste0(myTable$ctry, "-", myTable$inst)
  }

  # Create a new field combining parent ID and stratum names - this ensures
  # that records with the same stratum name but different parent records
  # are easily distinguished
  myTable$parentIDandStratum <- paste0(myTable$parentTableID, ":", myTable$stratumName)

  return(myTable)
}

process_table <- function(i, tablesToCheck, suLevels, targetValue, RDBESEstObjectForEstim, myStrataResults, foundTargetValue, verbose=FALSE) {
  currentTable <- tablesToCheck[i]
  parentTable <- NA
  if (i > 1) {
    parentTable <- tablesToCheck[i - 1]
  }
  if (verbose) {
    print(paste0("Processing ", currentTable))
  }

  # Work out what fields we need
  varsNeeded <- get_vars_needed(currentTable, parentTable, suLevels, i, targetValue)

  # Get our data
  myTable <- RDBESEstObjectForEstim[, ..varsNeeded]
  # De-duplicate (because columns to the right might be different
  # but have disappeared)
  myTable <- unique(myTable)

  # Get the parent table ID
  names(myTable)[names(myTable) == paste0(parentTable, "id")] <-
    paste0(currentTable, "parentTableID")

  # Remove the two letter prefix from column names so they will
  # be consistent between tables
  names(myTable) <-
    gsub(currentTable, "", names(myTable), ignore.case = FALSE)

  # change suxStratumName to stratumName
  if (i - 2 > 0) {
    names(myTable) <-
      gsub(suLevels[i - 2], "", names(myTable), ignore.case = FALSE)
  }

  myTable$parentTable <- parentTable

  # We'll fill in the values for parent stratum as we go through the tables
  myTable$parentTableStratum <- NA

  # Rename the variable we want to estimate
  # TODO - check this works for estimating SA variables when there is
  # sub-sampling
  if (!foundTargetValue){
    if (substring(targetValue, 3) %in% names(myTable)) {
      names(myTable)[names(myTable) == substring(targetValue, 3)] <-
        "studyVariable"
      foundTargetValue <- TRUE
    }
  }

  myTable <- add_extra_columns(myTable, currentTable)

  return(list(myStrataResults=myStrataResults, myTable=myTable, foundTargetValue=foundTargetValue))
}


