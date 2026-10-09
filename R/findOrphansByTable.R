#' Internal function to identify orphan records in a given
#' RDBESDataObject table
#'
#' @param tableToCheck The two letter code for the table to check
#' @param objectToCheck An RDBESDataObject
#' @param foreignKeyIds A vetor of the foreign key field names to check
#' @param verbose (Optional) If set to TRUE more detailed text will be printed
#' out by the function.  Default is TRUE.
#'
#' @return A data frame with the primary keys of the table checked, the two
#' letter table identifier, and their orphan status.
#'
#' @section Development review:
#' - AI-assisted: No
#' - Human review: rix133
findOrphansByTable <- function(tableToCheck,
                               objectToCheck,
                               foreignKeyIds,
                               verbose = FALSE) {
  if (verbose) {
    print(paste0("Checking table ", tableToCheck))
  }

  myTable <- objectToCheck[[tableToCheck]]

  fkIDsToCheck <- foreignKeyIds[foreignKeyIds$Table.Prefix ==
    tableToCheck, c("R.Name")]

  # Create a data frame to hold the results
  myOrphanResults <- data.table::data.table(myTable[, 1])
  names(myOrphanResults) <- c("pk")
  myOrphanResults$Table <- tableToCheck
  # Default to not being orphans
  myOrphanResults[, "isOrphan"] <- FALSE

  # if we have the possibility of orphans - let's check for them
  if (nrow(myOrphanResults) > 0 & length(fkIDsToCheck) > 0) {
    myOrphanResults[, paste(fkIDsToCheck, "Exists", sep = "")] <- FALSE

    # For each FK we need to check
    for (myFK in fkIDsToCheck) {
      if (verbose) {
        print(paste0("Checking field ", myFK))
      }
      fkTable <- substr(myFK, 1, 2)

      # Try joining to the foreign table (if it exists)
      if (!is.null(objectToCheck[[fkTable]])) {
        if (myFK == "SAparSequNum") {
          # Need to handle SAparSequNum differently
          joinedTables <- joinDT(myTable,
            objectToCheck[[fkTable]],
            by = c("SAseqNum" = "SAparSequNum"),
            type = "inner"
          )
        } else {
          joinedTables <- joinDT(myTable,
            objectToCheck[[fkTable]],
            by = myFK,
            type = "inner"
          )
        }

        # Update the orphan results for any matches we found
        if (nrow(joinedTables) > 0) {
          myColName <- paste(myFK, "Exists", sep = "")
          # Find which PKs are in the joined table - these are matches
          myOrphanResults[
            myOrphanResults[[1]] %in% joinedTables[[1]],
            myColName
          ] <- TRUE
        }
      }
    }
    myColNames <- paste(fkIDsToCheck, "Exists", sep = "")

    # Get rows that are FALSE for all of the FKs we are checking
    # - these are orphans
    orphanCheckRows <-
      apply(myOrphanResults[, ..myColNames], 1, function(x) (all(!x)))


    # Update the orphan results
    if (nrow(myOrphanResults[orphanCheckRows, ]) > 0) {
      myOrphanResults[orphanCheckRows, "isOrphan"] <- TRUE
    }

    # Just keep the first 3 columns
    myOrphanResults <- myOrphanResults[, 1:3]
  }

  # Just keep the orphan results
  myOrphanResults <- myOrphanResults[myOrphanResults$isOrphan, ]

  if (verbose) {
    print(paste0("Found ", nrow(myOrphanResults), " orphans"))
  }

  myOrphanResults
}
