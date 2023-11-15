#' Generate any missing SS rows.
#' When FOcatchReg=="All" it is expected that SScatchFraction is either "Catch"
#' OR "Lan"+"Dis". In the latter case, if one is missing the other is to
#' be assumed 0.
#' This function generates SS rows for any missing catch fractions.
#'
#' @param RDBESDataObject A valid RDBESDataObject
#' @param speciesListName The name of the Species List you want to use
#' for any SS rows that are created.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return A data table of SS data with any missing rows added
#' @export
#'
#' @examples
#' # To follow
generateMissingSSRows <- function(RDBESDataObject,
                                  speciesListName,
                                  verbose = FALSE,
                                  strict = TRUE) {
  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(RDBESDataObject,
    verbose = verbose,
    strict = strict
  )

  # Check we have some SL data
  if (length(RDBESDataObject[["SL"]]) <= 1) {
    if (is.null(RDBESDataObject[["SL"]])) {
      stop("SL data does not exist in the input data but it is required")
    }
  }

  # Check we have some SS data
  if (length(RDBESDataObject[["SS"]]) <= 1) {
    if (is.null(RDBESDataObject[["SS"]])) {
      stop("SS data does not exist in the input data but it is required")
    }
  }

  # Check we have some FO data
  if (length(RDBESDataObject[["FO"]]) <= 1) {
    if (is.null(RDBESDataObject[["FO"]])) {
      stop("FO data does not exist in the input data but it is required")
    }
  }

  # Check if speciesListName exists for this year and country
  #print("TODO: Need to use year, country when we check the species list name")
  # For the time being just check if it exists at all

  if (!speciesListName %in% unique(RDBESDataObject[["SL"]]$SLspeclistName)) {
    stop("The requested species list name does not exist in the input data")
  }

  # take a copy of the data so we don't change the original
  myFO <- data.table::copy(RDBESDataObject[["FO"]])
  mySS <- data.table::copy(RDBESDataObject[["SS"]])
  mySL <- data.table::copy(RDBESDataObject[["SL"]])


  # New we can check the supplied species list name is valid for all SS records

  # Append the year and country to our SS data
  mySSUnique <- mySS
  mySSUnique$SSyear <- extractHigherFields(RDBESDataObject, "SS", "DEyear")
  mySSUnique$SSctry <- extractHigherFields(RDBESDataObject, "SS", "SDctry")
  mySSUnique$SSspecListName <- speciesListName

  # Get the unique combinations of SS country, year, and the request list name
  mySSUnique <- unique(mySSUnique[,c("SSctry","SSyear","SSspecListName")])

  # Get the unique combinations of SL country, year, and list names
  mySLUnique <- unique(mySL[,c("SLcou","SLyear","SLspeclistName")])

  mySSSLUnique <- dplyr::inner_join(mySSUnique,
                                   mySLUnique,
                                   by = c("SSyear" = "SLyear",
                                          "SSctry" = "SLcou",
                                          "SSspecListName" = "SLspeclistName"))

  if (nrow(mySSSLUnique) < 1){
    stop(paste0("The requested species list is not compatible with the ",
    "combination of SS country and year"))
  }

  # OK, if we made it here we can now get on with things

  # We only care about sampled FO records
  myFO <- myFO[myFO$FOsamp == "Y", ]

  # We only want to check SS records that are linked to the supplied, sampled FO data
  mySSLinked <- mySS[!is.na(mySS$FOid) & mySS$FOid %in% myFO$FOid, ]

  if (verbose) {
    print(paste0(nrow(myFO), " rows of FO data have been sampled in the input data"))
    print(paste0(nrow(mySSLinked), " rows of SS data are linked to the sampled FO records in the input data"))
  }

  ## STEP 1) FO "All"

  # Check FO "All" rows against SS "Catch" rows
  if (verbose) {
    print("Checking FOcatReg = 'All' against SS SScatchFra = 'Catch'")
  }
  # These are the FO rows that aren't matching to Catch SS records
  SSAllMissing <- getMissingSSCatchFraction(myFO, mySSLinked, "All", verbose)

  # Now we also need to check whether these "All" records match to Lan
  # and Dis SS records
  if (length(SSAllMissing) > 0) { # 1
    if (verbose) {
      print(paste0("Now we also need to check FOcatReg = 'All' against ",
      "SS SScatchFra = 'Lan' and 'Dis'"))
    }
    # FO "All" records that didn't match to "Catch"
    myFOToCheck <- myFO[myFO$FOid %in% SSAllMissing, ]
    # Change FO "All" rows to "Lan" and "Dis" so we can try and match SS
    # Lan
    myFOToCheckLan <- myFOToCheck
    myFOToCheckLan$FOcatReg <- "Lan"
    SSAllMissing_1 <- getMissingSSCatchFraction(
      myFOToCheckLan,
      mySSLinked,
      "Lan",
      verbose
    )
    SSAllToAdd_1 <- generateSSRows(
      SSAllMissing_1,
      speciesListName,
      "Lan"
    )
    if (verbose) {
      print(paste0(
        nrow(SSAllToAdd_1),
        " landing rows added to SS (generated by FOcatReg = 'All')"
      ))
    }
    # Dis
    myFOToCheckDis <- myFOToCheck
    myFOToCheckDis$FOcatReg <- "Dis"
    SSAllMissing_2 <- getMissingSSCatchFraction(
      myFOToCheckDis,
      mySSLinked,
      "Dis",
      verbose
    )
    SSAllToAdd_2 <- generateSSRows(
      SSAllMissing_2,
      speciesListName,
      "Dis"
    )
    if (verbose) {
      print(paste0(
        nrow(SSAllToAdd_2),
        " discard rows added to SS (generated by FOcatReg = 'All')"
      ))
    }
  } else {
    # If we got to this point then the FO "All" records all matched to SS
    # "Catch" records so we don't need to add anything for "All".
    # We just create some empty data frames to make life easier at the
    # end of the function
    SSAllToAdd_1 <- generateSSRows(integer(), "", "")
    SSAllToAdd_2 <- generateSSRows(integer(), "", "")
  }



  ## STEP 2)  FO "Lan"
  if (verbose) {
    print("Checking FOcatReg = 'Lan'")
  }
  SSLanMissing <- getMissingSSCatchFraction(myFO, mySSLinked, "Lan", verbose)
  SSLanToAdd <- generateSSRows(
    SSLanMissing,
    speciesListName,
    "Lan"
  )
  if (verbose) {
    print(paste0(nrow(SSLanToAdd), " landing rows added to SS ",
    "(generated by FOcatReg = 'Lan')"))
  }

  ## STEP 3) FO "Dis"
  if (verbose) {
    print("Checking FOcatReg = 'Dis'")
  }
  SSDisMissing <- getMissingSSCatchFraction(myFO, mySSLinked, "Dis", verbose)
  SSDisToAdd <- generateSSRows(
    SSDisMissing,
    speciesListName,
    "Dis"
  )
  if (verbose) {
    print(paste0(nrow(SSDisToAdd), " discard rows added to SS ",
    "(generated by FOcatReg = 'Dis')"))
  }

  ## STEP 4) FO None
  # Do nothing :-)


  ## STEP 5) Return SS with any extra rows
  SStoAdd <- rbind(SSAllToAdd_1, SSAllToAdd_2, SSLanToAdd, SSDisToAdd)
  if (verbose) {
    print(paste0(nrow(SStoAdd), " rows of SS data have been added"))
  }
  SStoReturn <- rbind(mySS, SStoAdd, fill = TRUE)
  if (verbose) {
    print(paste0(nrow(SStoReturn), " rows of SS data will be returned"))
  }

  # Create SSids for our any new rows with NA SSid values
  currentMaxSSid <- max(SStoReturn$SSid, na.rm = TRUE)
  numberOfNAs <- nrow(SStoReturn[is.na(SStoReturn$SSid),])
  if (numberOfNAs > 0){
    newIds <- seq(currentMaxSSid+1,currentMaxSSid+numberOfNAs)
    SStoReturn[is.na(SStoReturn$SSid),"SSid"] <- newIds
  }

  # Ensure we return SS as a data table with keys set
  data.table::setDT(SStoReturn)
  data.table::setkeyv(SStoReturn,"SSid")

  SStoReturn
}

#' Private function to find which FO rows are not matching SS
#'
#' @param FOdata The FOdata
#' @param SSdata The SSdata
#' @param catchFra The catchfra
#' @param verbose verbose or not?
#'
#' @return Vector of FOids that aren't matching SS rows
getMissingSSCatchFraction <- function(FOdata, SSdata, catchFra, verbose) {
  myFOFraction <- FOdata[FOdata$FOcatReg == catchFra, ]
  if (verbose) {
    print(paste0(
      "Processing ", nrow(myFOFraction),
      " rows of FO data for FOcatReg = ", catchFra
    ))
  }

  # If we're checking the FO "All" records then we'll change it to "Catch"
  # to make our join easier (SScatchFra has values Lan, Dis, or Catch)
  if (catchFra == "All") {
    myFOFraction[myFOFraction$FOcatReg == catchFra, "FOcatReg"] <- "Catch"
  }

  myFOSS <- dplyr::left_join(
    myFOFraction,
    SSdata,
    by = c(
      "FOid" = "FOid",
      "FOcatReg" = "SScatchFra"
    )
  )
  mySSFractionMissing <- myFOSS[is.na(myFOSS$SSid), ]

  mySSFractionMissing$FOid
}

#' Private function to generate SS rows
#'
#' @param FOids Vector of FOids
#' @param speciesListName  Name of the species list
#' @param catchFra The catch fraction to create
#'
#' @return SS data frame
#'
generateSSRows <- function(FOids, speciesListName, catchFra) {
  if (length(FOids) > 0) {
    myNAs <- replicate(length(FOids), NA)
    specListNames <- replicate(length(FOids), speciesListName)
    catchFractions <- replicate(length(FOids), catchFra)
  } else {
    myNAs <- integer()
    specListNames <- character()
    catchFractions <- character()
  }



  # Create an SS data frame using the column definitions
  SSfields <- RDBEScore::mapColNamesFieldR[
    RDBEScore::mapColNamesFieldR$Table.Prefix == "SS",]

    # Always start with SSid
  SSDF <- data.frame(SSid = as.integer(myNAs))

   # Add the fields as the correct data type
  for (i in seq(2,nrow(SSfields))){
    #print(i)
    myField <- SSfields[i,]
    myFieldName <- myField[,"R.Name"]
    #print(myFieldName)
    if (myFieldName == "FOid"){
      SSDF[,myFieldName] <- FOids
    } else if (myFieldName == "SScatchFra") {
      SSDF[,myFieldName] <- catchFractions
    } else if (myFieldName == "SSspecListName"){
      SSDF[,myFieldName] <- specListNames
    } else {
      if (myField$RDataType == "integer"){
        SSDF[,myFieldName] <- as.integer(myNAs)
      } else if (myField$RDataType == "character"){
        SSDF[,myFieldName] <- as.character(myNAs)
      } else if (myField$RDataType == "numeric"){
        SSDF[,myFieldName] <- as.numeric(myNAs)
      } else {
        SSDF[,myFieldName] <- as.logical(myNAs)
      }
    }
  }

  SSDF
}
