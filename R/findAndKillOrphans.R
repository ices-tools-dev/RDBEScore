#' This function finds and removed any orphan records in an
#' RDBESDataObject.  Normally data that has been downloaded from the RDBES
#' will not contain orphan records - however if the data is subsequently
#' filtered it is possible to introduce orphan records.
#'
#' @param objectToCheck an RDBESDataObject.
#' @param verbose (Optional) If set to TRUE more detailed text will be printed
#' out by the function.  Default is FALSE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return an RDBESDataObject with any orphan records removed
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#' importRDBESDataCSV(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#' myFields <- c("SDctry","VDctry","VDflgCtry","FTarvLoc")
#' myValues <- c("ZW","ZWBZH","ZWVFA" )
#' myFilteredObject <- filterRDBESDataObject(myH1RawObject,
#'                                         fieldsToFilter = myFields,
#'                                         valuesToFilter = myValues )
#' myObjectNoOrphans <- findAndKillOrphans(objectToCheck = myFilteredObject,
#'                                        verbose = FALSE)
#' }
findAndKillOrphans <- function(objectToCheck,
                               verbose = FALSE,
                               strict = TRUE) {

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(objectToCheck, verbose = verbose, strict = strict)

  # Get all the XXid fields and SAparSequNum
  myIds <- RDBEScore::mapColNamesFieldR[
    grepl("^..id$", RDBEScore::mapColNamesFieldR$R.Name),
    c("Table.Prefix", "R.Name")]

  # Get rid of the primary key ids from each table
  myForeignKeyIds <- myIds[!sapply(seq_along(myIds$Table.Prefix),
                        function(i)
                         grepl(myIds$Table.Prefix[i],
                               myIds$R.Name[i],
                               fixed = TRUE)
                        ), ]

  # Checking for invalid vessel links is handled by removeBrokenVesselLinks()
  # Checking for invalid species list links is handled by
  # removeBrokenSpeciesListLinks()
  myForeignKeyIds <- myForeignKeyIds[
    !(myForeignKeyIds$R.Name == "VDid" | myForeignKeyIds$R.Name == "SLid"), ]

  # Special case for SA - need to add in the parent sequence number field
  myParSeqNum <- RDBEScore::mapColNamesFieldR[
    grepl("^SAparSequNum$", RDBEScore::mapColNamesFieldR$R.Name),
    c("Table.Prefix", "R.Name")]

  myForeignKeyIds <- rbind(myForeignKeyIds, myParSeqNum)

  if (verbose){
    print("Number of rows in non-null tables before killing orphans")
    print(unlist(lapply(objectToCheck, nrow)))
  }

  nonNullEntries <- names(objectToCheck[sapply(objectToCheck, Negate(is.null))])

  # Don't bother checking CE and CL - they are big and can't have orphans anyway
  nonNullEntries <- nonNullEntries[
    !(nonNullEntries == "CE" | nonNullEntries == "CL")]

  orphanCheckNumber <- 1

  if (verbose) {
    print("Starting to hunt down the orphans")
  }

  # Removing orphans can then have the knock-on effect of creating more
  # orphans lower in the hierarchy so we will keep need to check for orphans
  # until we don't find any.
  while (1 == 1) {

    # Sanity check to stop infinite loops
    if (orphanCheckNumber > 20) {
      errorMessage <- paste0("Checking for orphans is taking too long",
      "- there might be a problem - please check your data")
      stop(errorMessage)
    }

    if (verbose) {
      print(paste0("Orphan check round number ", orphanCheckNumber))
    }

    # Ok, now let's search for orphans
    myResults <- lapply(nonNullEntries,
                        findOrphansByTable,
                        objectToCheck = objectToCheck,
                        foreignKeyIds = myForeignKeyIds,
                        verbose = verbose)

    myResults <- do.call("rbind", myResults)

    # Work out how many orphans were found (need to allow for the case
    # where all tables were NULL though)
    if (length(is.null(myResults)) == 1 & is.null(myResults)){
      numberOfOrphansFound <- 0
    } else {
      numberOfOrphansFound <- nrow(myResults)
    }

    if (verbose) {
      print(paste0("Total number of orphans found: ", numberOfOrphansFound))
    }

    if (numberOfOrphansFound > 0) {
      objectToCheck <- killOrphans(objectToCheck = objectToCheck,
                                   orphansToRemove = myResults)
    }

    # If we didn't find any orphans this time we can stop looking
    if (numberOfOrphansFound == 0) {
      if (verbose){
        print("All orphans have been killed")
      }
      break
    }

    orphanCheckNumber <- orphanCheckNumber + 1
  }

  if (verbose){
    print("Number of rows in non-null tables after killing orphans")
    print(unlist(lapply(objectToCheck, nrow)))
  }

  objectToCheck
}
