#' Remove rows which are not pointing to a valid SpecliestListDetails (SL)
#' records i.e.those rows which have a value of SpeciesListName that does not
#' exist in the SL table.
#'
#' @param objectToCheck an RDBESDataObject.
#' @param verbose (Optional) If set to TRUE more detailed text will be printed
#' out by the function.  Default is TRUE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return an RDBESDataObject with any records with an invalid SpeciesListName
#' rows removed
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'   importRDBESDataCSV(rdbesExtractPath = "tests\\testthat\\h1_v_1_19")
#' myFields <- c("SLspeclistName")
#' myValues <- c("WGRDBES-EST TEST 5 - sprat data")
#' myFilteredObject <- filterRDBESDataObject(myH1RawObject,
#'   fieldsToFilter = myFields,
#'   valuesToFilter = myValues
#' )
#' myObjectValidSpeciesListLinks <- removeBrokenSpeciesListLinks(
#'   objectToCheck = myFilteredObject,
#'   verbose = FALSE
#' )
#' }
removeBrokenSpeciesListLinks <- function(objectToCheck,
                                         verbose = FALSE,
                                         strict = TRUE) {


  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(objectToCheck, verbose = verbose, strict = strict)

  # Just check the non-null entries
  nonNullEntries <- names(objectToCheck[sapply(objectToCheck, Negate(is.null))])

  if (!"SL" %in% nonNullEntries){
    stop(paste0("The SL entry in in objectToCheck is null - ",
                   "cannot check for broken species list links"))
  }

  # Only SS links to the species list table
  tableToCheck <- "SS"

  # Check any non-null tables
  if (tableToCheck %in% nonNullEntries) {

    if (verbose){
      print(paste0(
        "Number of rows in relevent non-null tables before removing ",
        "broken species list links"
      ))
      print(unlist(lapply(objectToCheck[tableToCheck], nrow)))
    }

    myTable <- objectToCheck[[tableToCheck]]

    # if we have the possibility of orphans - let's check for them
    if (nrow(myTable) > 0) {
      if (verbose) {
        print(paste0(
          "Checking for broken species list links in table ",
          tableToCheck
        ))
      }

      # Create a data frame to hold the results
      myOrphanResults <- data.table::data.table(myTable[, 1])
      names(myOrphanResults) <- c("pk")
      myOrphanResults$Table <- tableToCheck
      # Default to link not existing
      myOrphanResults[, "slExists"] <- FALSE


# Add year and country to SS ----------------------------------------------------

myTable$SSyear <- extractHigherFields(objectToCheck, "SS", "DEyear")
myTable$SSctry <- extractHigherFields(objectToCheck, "SS", "SDctry")

# -------------------------------------------------------------------------


      # Inner join to the SL table
      joinedTables <- dplyr::inner_join(myTable,
                                    objectToCheck[["SL"]],
                                    by = c("SSspecListName" = "SLspeclistName",
                                           "SScatchFra" = "SLcatchFrac",
                                           "SSyear" = "SLyear",
                                           "SSctry" = "SLcou"),
                                    multiple = "all",
                                    relationship="many-to-many"
      )


# Remove year and ctry columns from SS ------------------------------------

myTable <- myTable[, SSyear := NULL]
myTable <- myTable[, SSctry := NULL]

# -------------------------------------------------------------------------


      # Remove year and ctry columns from SL


      # Update the results for any matches we found
      if (nrow(joinedTables) > 0) {

        # Find which PKs are in the joined table - these are matches
        myOrphanResults[
          myOrphanResults[[1]] %in% joinedTables[[1]],
          "slExists"
        ] <- TRUE
      }

      # We want to remove any rows where the record did not exist in VD
      rowsToRemove <- myOrphanResults[!myOrphanResults$slExists, 1]
      rowsToRemove <- rowsToRemove[[1]]

      if (verbose) {
        print(paste0(
          "Found ", length(rowsToRemove),
          " records with broken species list links"
        ))
      }

      # Just keep the rows that aren't in the list to remove
      objectToAlter <- myTable
      objectToAlter <-
        objectToAlter[!objectToAlter[[1]] %in% rowsToRemove, ]

      objectToCheck[[tableToCheck]] <- objectToAlter

      if (verbose) {
        print(paste0("Records with broken species list links have been removed"))
      }
    }
  }

  if (verbose){
    print(paste0(
      "Number of rows in relevent non-null tables after removing ",
      "broken species list links"
    ))
    print(unlist(lapply(objectToCheck[tableToCheck], nrow)))
  }

  return(objectToCheck)
}

