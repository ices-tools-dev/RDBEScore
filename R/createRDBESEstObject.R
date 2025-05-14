#' Creates an rdbesEStObject from prepared RDBES data
#'
#' @param rdbesPrepObject The prepared RDBES object that should be used to
#' create an estimation object
#' @param hierarchyToUse (Optional) The upper RDBES hierarchy to use. An integer value
#' between 1 and 13. If NULL, the hierarchy will be determined from the DE table
#'
#' @param stopTable (Optional) The table to stop at in the RDBES hierarchy.
#' If specified, only tables up to and including this table will be included in the
#' resulting RDBESEstObject. The default is NULL, which means all tables in the hierarchy
#' will be included.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return An object of class RDBESEstObject ready for use in design based
#' estimation
#' @export
#'
#' @examples
#' #Creates an rdbesEStObject from prepared RDBES data
#' myH1EstObj <- createRDBESEstObject(H1Example, 1, "SA")
#'
#'
#' @param rdbesPrepObject The prepared RDBES object that should be used to
#' create an estimation object
#' @param hierarchyToUse The upper RDBES hiearchy to use
#'
#' @param stopTable (Optional) The table to stop at in the RDBES hierarchy.
#' If specified, only tables up to and including this table will be included in the
#' resulting RDBESEstObject. The default is NULL, which means all tables in the hierarchy
#' will be included.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return An object of class RDBESEstObject ready for use in design based
#' estimation
#' @export
#'
#' @examples
#' myH1EstObj <- createRDBESEstObject(H1Example, 1, "SA")
createRDBESEstObject <- function(rdbesPrepObject,
                                 hierarchyToUse =NULL,
                                 stopTable = NULL,
                                 verbose = FALSE,
                                 strict = TRUE) {

  DEhierarchy <- summary(rdbesPrepObject)$hierarchy
  if(is.null(hierarchyToUse)){
    hierarchyToUse <- DEhierarchy
    if(length(hierarchyToUse) > 1){
      stop("Mixed hierarchy RDBESDataObject!", call.=FALSE)
    }
  }

  if (!hierarchyToUse %in% 1:13) {
    stop(paste0(
      "An invalid value was used for the 'hierarchyToUse' parameter",
      " - createRDBESEstObject will not proceed"
    ))
  }
  if(!(hierarchyToUse %in% DEhierarchy) & !is.null(DEhierarchy)){
    stop(paste0(
      "The hierarchyToUse parameter is not the hierarchy stated on the DE table",
      " - createRDBESEstObject will not proceed"
    ))
  }
  if(length(DEhierarchy) > 1){
    stop("Mixed hierarchy RDBESDataObject! not supported", call.=FALSE)
  }

  validateRDBESDataObject(rdbesPrepObject, verbose = verbose, strict = strict)

  # Copy the input data table so we don't change the original data
  rdbesPrepObjectCopy <- data.table::copy(rdbesPrepObject)

  # Change text columns to factors to try and reduce final RDBESEstObject size
  # Loop over each table in the list
  for (tableName in names(rdbesPrepObjectCopy)) {
    dt <- rdbesPrepObjectCopy[[tableName]]
    if (is.data.table(dt)) {
      char_cols <- names(dt)[sapply(dt, is.character)]
      if (length(char_cols) > 0) {
        dt[, (char_cols) := lapply(.SD, as.factor), .SDcols = char_cols]
      }
      rdbesPrepObjectCopy[[tableName]] <- dt
    }
  }


  # See if the user has specified a table to stop at
  #take out the optional as this messes up est object creation
  if (hierarchyToUse == '9'){ #temporary fix
    targetTables <-c("DE", "SD", "LO", "TE", "SS", "SA" ,"LE" ,"FM", "BV")
  }else{
    targetTables <-
      RDBEScore::getTablesInRDBESHierarchy(hierarchyToUse, includeOptTables = F)
  }
  if (length(is.null(stopTable)) == 1 &&
    !is.null(stopTable)) {
    stopTableLoc <- which(targetTables == stopTable)
    if (length(stopTableLoc) > 0) {
      targetTables <- targetTables[1:stopTableLoc]
    }
  }

  gc()
  # See if we need to process the lower hieriarchy tables - this
  # needs to be done before any changes required due to sub-sampling
  if (any(targetTables %in% c("FM", "BV"))) {
    processLowerHierarchy <- TRUE
    if (verbose) {
      print("Processing lower hierarachy data")
    }
    allLower <-
      procRDBESEstObjLowHier(rdbesPrepObjectCopy, verbose = verbose)
    if (verbose){
      print(paste0("Number of rows of lower hierarchy data ", nrow(allLower)))
    }
  } else {
    processLowerHierarchy <- FALSE
    if (verbose) {
      print("Not processing lower hierarachy data")
    }
  }

  # Handle any sub-sampling
  # Check if we have any SA data
  if (length(is.null(rdbesPrepObjectCopy[["SA"]])) == 1 &&
    is.null(rdbesPrepObjectCopy[["SA"]])) {
    if (verbose) {
      print("No SA data found - can't check for sub-sampling")
    }
  } else {

    # Deal with the sub-sampling
    if (verbose) {
      print("Checking for sub-sampling")
    }

    # The latest RDBES downloads don't have a field for SAparentID
    # If we don't have a field for SAparentID we need to make one
    # using the SAparentSequenceNumber
    if (!"SAparentID" %in%  names(rdbesPrepObjectCopy[["SA"]])){
      # Find the SAid for a given value of SAparSequNum
      # myResults <- sapply(rdbesPrepObjectCopy[["SA"]]$SAparSequNum,function(x){
      #   valueToReturn <- NA
      #   if (!is.na(x)){
      #     # TODO - I don't think we can assume that SAparSequNum is unique
      #     valueToReturn <-
      #       rdbesPrepObjectCopy[["SA"]][rdbesPrepObjectCopy[["SA"]]$SAseqNum ==
      #                                     x,]
      #     if (nrow(valueToReturn) == 1){
      #       valueToReturn <- valueToReturn$SAid
      #     } else {
      #       warning(paste0("Could not find unique matching parent sequence ",
      #       "number - sub-sampling has not been processed correctly"))
      #       valueToReturn <- NA
      #     }
      #   }
      #   valueToReturn
      # })
      # For easier reference
      SA <- rdbesPrepObjectCopy[["SA"]]

      # Self-join: child.SAparSequNum matches parent.SAseqNum, and SSid matches
      # The sequence number is not unique so we also use SSid to match the child rows to parent rows
      SA_with_parent <- SA[
        SA,
        on = .(SAseqNum = SAparSequNum, SSid),
        allow.cartesian = TRUE,
        nomatch = NA
      ]

      # Count matches per child row (i.SAid = child row's SAid)
      match_counts <- SA_with_parent[, .N, by = .(i.SAid)]

      # Warn about no match
      no_match_ids <- SA[!SAid %in% match_counts$i.SAid, SAid]
      if (length(no_match_ids) > 0) {
        warning(paste("No parent match found for SAid(s):", paste(no_match_ids, collapse = ", ")))
      }

      # Warn about non-unique matches
      non_unique_ids <- match_counts[N > 1, i.SAid]
      if (length(non_unique_ids) > 0) {
        warning(paste("Multiple parent matches found for SAid(s):", paste(non_unique_ids, collapse = ", ")))
      }

      # Filter to only rows where there's exactly one match
      unique_matches <- match_counts[N == 1]
      unique_links <- SA_with_parent[i.SAid %in% unique_matches$i.SAid]

      # Add the parent SAid to the child row as SAparentID
      SA[unique_links, on = .(SAid = i.SAid), SAparentID := SAid]

      # Save back
      rdbesPrepObjectCopy[["SA"]] <- SA

      #rdbesPrepObjectCopy[["SA"]]$SAparentID <- myResults
      gc()
    }

    if (verbose) print("Sub-sampling: checking for SAid self-references")
    # find any value of SAid that is the same as SAparentID - set that
    # SAparentID to NA
    rdbesPrepObjectCopy[["SA"]][
      rdbesPrepObjectCopy[["SA"]]$SAid ==
        rdbesPrepObjectCopy[["SA"]]$SAparentID,
      "SAparentID"
    ] <- NA

    # Find the top level SAid and sampling level for each SAid
    SAdata <- rdbesPrepObjectCopy[["SA"]]
    # Calculate the lookup table which contains the top level SAid and sampling level for each SAid
    # (much faster than the recursive "getSubSampleLevel()" function it was using before)
    lookupDT <- prepareSubSampleLevelLookup(SAdata)
    # Apply it
    subSampleLevels <- dplyr::left_join(SAdata, lookupDT, by = "SAid")
    subSampleLevels <- subSampleLevels[,c("SAid","topLevelSAid","subSampleLevel")]

    rdbesPrepObjectCopy[["SA"]][, "SAtopLevelSAid"] <-
      subSampleLevels$topLevelSAid
    rdbesPrepObjectCopy[["SA"]][, "SAsubSampleLevel"] <-
      subSampleLevels$subSampleLevel
    numberOfSampleLevels <-
      max(rdbesPrepObjectCopy[["SA"]][, "SAsubSampleLevel"])

    if (verbose) {
      print(paste0("Max levels of sampling: ", numberOfSampleLevels))
    }

    if (numberOfSampleLevels > 1) {
      # Create new entries for each level of sampling
      for (i in 1:numberOfSampleLevels) {
        saNameNew <- paste0("SA", i)
        rdbesPrepObjectCopy[[saNameNew]] <-
          rdbesPrepObjectCopy[["SA"]][
            rdbesPrepObjectCopy[["SA"]]$SAsubSampleLevel == i,
          ]
        rdbesPrepObjectCopy[[saNameNew]]$SArecType <-
          paste0(rdbesPrepObjectCopy[[saNameNew]]$SArecType, i)
        # Rename the columns to SA2 etc (don't bother for SA1 because we
        # are going to chaneg its name to SA in a moment)
        if (i > 1) {
          names(rdbesPrepObjectCopy[[saNameNew]]) <-
            gsub("^SA", saNameNew, names(rdbesPrepObjectCopy[[saNameNew]]))
        }
      }
      # Get rid of the overall SA entry
      rdbesPrepObjectCopy[["SA"]] <- NULL
      # Rename SA1 to SA for consistency
      names(rdbesPrepObjectCopy)[names(rdbesPrepObjectCopy) == "SA1"] <- "SA"
    }

    # Replace "SA" in the target tables with "SA", "SA2" etc to handle
    # sub-sampling (if required)
    SAloc <- which(targetTables == "SA")
    if (length(SAloc) > 0 &&
      length(grep("^SA.+$", names(rdbesPrepObjectCopy))) > 0) {
      targetTables <- append(targetTables,
        names(rdbesPrepObjectCopy)[
          grep("^SA.+$", names(rdbesPrepObjectCopy))
        ],
        after = SAloc
      )
    }
  }

  if (verbose) {
    print("Processing upper hierarachy data")
  }

  # Combine the upper hierachy tables
  upperHierarchy <- procRDBESEstObjUppHier(
    rdbesPrepObject = rdbesPrepObjectCopy,
    hierarchyToUse = hierarchyToUse,
    targetTables = targetTables,
    verbose = verbose
  )

  if (verbose){
    print(paste0("Number of rows of upper hierarchy data ",
                 nrow(upperHierarchy)))
  }


  # Join the upper and lower hierarchy tables together
  # Check if we have any lower hierarchy data first, and whether we actually
  # need to process the lower hiaerarcy - if not just return the upper results
  if (length(is.null(rdbesPrepObjectCopy[["SA"]])) == 1 &&
    is.null(rdbesPrepObjectCopy[["SA"]])) {
    if (verbose) {
      print(paste0(
        "No sample data found - ",
        "won't try to combine upper and lower hierarchy data"
      ))
    }
    myRDBESEstObj <- upperHierarchy
  } else if (!processLowerHierarchy) {
    myRDBESEstObj <- upperHierarchy
  } else if (is.null(nrow(allLower))) {
    myRDBESEstObj <- upperHierarchy
  } else {
    if (verbose) {
      print("Combining upper and lower hierarachy data")
    }

    # Join the upper and lower hierarchy data together - ensuring we handle
    # any sub-sampling correctly

    myRDBESEstObj <- NULL

    for (j in numberOfSampleLevels:1) {
      tempLower <- data.table::copy(allLower)

      if (j > 1) {
        saJoinField <- paste0("SA", j, "id")
        tempLower[[saJoinField]] <- tempLower[["SAid"]]
        tempLower[, SAid := NULL]
      } else {
        saJoinField <- "SAid"
      }
      if (verbose) {
        print(paste0(
          "Trying to join upper and lower hierarchy data using ",
          saJoinField
        ))
      }


      tempUpper <- data.table::copy(upperHierarchy)

      # Use a left join on the deepest level of sampling so that we
      # get all the upper hierarchy rows
      if (j == numberOfSampleLevels) {
        tempRDBESEstObj <- dplyr::left_join(tempUpper,
          tempLower,
          by = saJoinField,
          multiple = "all"

        )
      } else {

        # Since we are now checking against higher levels of sampling we
        # need to get rid of the data from the deeper SA levels from
        # tempUpper so that it's clear what we have joined to...

        # Find the column names for the deeper SA level (SAx and sux notation)
        colsToRemove <- names(tempUpper)[grep(paste0("^SA", j + 1),
                                              names(tempUpper))]
        suTableCols <- names(tempUpper)[grep("^su.*table", names(tempUpper))]
        mySUCol <- NA
        for (myCol in suTableCols) {
          uniqueValues <- unique(tempUpper[[myCol]])
          uniqueValues <- uniqueValues[!is.na(uniqueValues)]
          if (uniqueValues == paste0("SA", j + 1)) {
            mySUCol <- substr(myCol, 1, nchar(myCol) - 5)
            break
          }
        }
        suColsToRemove <- paste0(mySUCol, RDBEScore::designVariables)
        allColsToRemove <- c(colsToRemove, suColsToRemove)
        allColsToRemove <- allColsToRemove[
          allColsToRemove %in% names(tempUpper)]

        # Set the SA level data that we don't need any more to NA
        tempUpper[, (allColsToRemove)] <- NA
        tempUpper <- unique(tempUpper)

        # Use an inner join on the
        # currnt SA level so that we just get the matching rows
        tempRDBESEstObj <- dplyr::inner_join(tempUpper,
          tempLower,
          by = saJoinField,
          multiple = "all"
        )
      }
      if (verbose) {
        print(paste0("Number of rows joined ", nrow(tempRDBESEstObj)))
      }
      myRDBESEstObj <- rbind(myRDBESEstObj, tempRDBESEstObj)
    }
  }

  # Choose which VDid field to keep - to avoid confusion
  vdIDFields <- names(myRDBESEstObj)[grepl("^VDid.*", names(myRDBESEstObj))]
  vdIDFieldToKeep <- NA
  if (length(vdIDFields) > 1) {
    # read through all the VDid fields and note the name of the first
    # one that has some data - this is the one we will name "VDid"
    for (myVDid in vdIDFields) {
      if (is.na(vdIDFieldToKeep) && any(!is.na(myRDBESEstObj[[myVDid]]))) {
        vdIDFieldToKeep <- myVDid
        break
      }
    }
    # Rename the VDid field we want to keep
    if (!is.na(vdIDFieldToKeep)) {
      names(myRDBESEstObj)[names(myRDBESEstObj) == vdIDFieldToKeep] <- "VDid"
      vdIDFieldsToRemove <- vdIDFields[!(vdIDFields == vdIDFieldToKeep)]
    } else {
      vdIDFieldsToRemove <- vdIDFields
    }
    # Get rid of the other VDid fields
    myRDBESEstObj[, (vdIDFieldsToRemove) := NULL]
  }

  # If SAparentID exists - remove it to avoid confusion
  # (the RDBES now uses parentSequenceNumber to link samples and sub-samples)
  if ("SAparentID" %in% names(myRDBESEstObj)){
    myRDBESEstObj$SAparentID <- NULL
  }

  # If myRDBESEstObj is null let's create an empty data table to return
  if (length(is.null(myRDBESEstObj)) == 1 &&
    is.null(myRDBESEstObj)) {
    if (verbose) {
      print("Returning an empty RDBESEstObject")
    }
    myRDBESEstObj <- data.table()
  }

  # Set the class of the object
  class(myRDBESEstObj) <- c("RDBESEstObject", class(myRDBESEstObj))


  myRDBESEstObj
}

#' Private function to process the lower hierarchies when creating
#' the RDBESEstObject
#'
#' @param rdbesPrepObject A prepared RDBESRawObj
#' @param verbose logical. Output messages to console.
#'
#' @return allLower - the FM and BV tables combined
#'
procRDBESEstObjLowHier <- function(rdbesPrepObject,
                                   verbose = FALSE) {

gc()
  # Check if we have any SA data - if not we'll just stop now
  if (length(is.null(rdbesPrepObject[["SA"]])) == 1 &&
    is.null(rdbesPrepObject[["SA"]])) {
    if (verbose) {
      print("No SA data found - can't process the lower hierarchies")
    }
    return(NULL)
  }

  # Break out the lower hierachies - they need to join to different tables
  lowerA <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "A")[, 1], "SAid"
  ]
  lowerB <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "B")[, 1], "SAid"
  ]
  lowerC <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "C")[, 1], "SAid"
  ]
  lowerD <- rdbesPrepObject[["SA"]][
    (rdbesPrepObject[["SA"]][, "SAlowHierarchy"] == "D")[, 1], "SAid"
  ]

  # Join FM and BV - we do it in both ways - left join on FM and right join on
  # BV.  This is because not all of the lower hierachies have FM data.

  # FMBV

  # We need to check if we have Fm and BV data before joining them.
  if (is.null(nrow(rdbesPrepObject[["FM"]])) &
      is.null(nrow(rdbesPrepObject[["BV"]]))){

    # Both Fm and BV are null
    fMBV <- NULL
  } else if (is.null(nrow(rdbesPrepObject[["FM"]]))) {

    # FM is null
    fMBV <- NULL
  } else if (is.null(nrow(rdbesPrepObject[["BV"]]))) {

    # Just BV is null
    fMBV <- rdbesPrepObject[["FM"]]
  } else {
    gc()
    # if we have both FM and BV data - join them together
    fMBV <-
      dplyr::left_join(rdbesPrepObject[["FM"]],
        rdbesPrepObject[["BV"]],
        by = "FMid",
        multiple = "all"
      )
    gc()
    # sort out the wrong SAid column name after the join
    names(fMBV)[names(fMBV) == "SAid.x"] <- "SAid"
    fMBV[, "SAid.y" := NULL]
  }

  # Now get rid of any unneccessary id fields to avoid trouble with field naming
  allowedIds <- c("SAid", "FMid", "BVid")
  unallowedIDs <-
    paste0(names(rdbesPrepObject), "id")[
      !(paste0(names(rdbesPrepObject), "id") %in% allowedIds)
    ]
  unallowedIDs <-
    names(fMBV)[names(fMBV) %in% unallowedIDs]
  if (length(unallowedIDs) > 0) fMBV[, (unallowedIDs) := NULL]

  # BVFM

  # We need to check if we have Fm and BV data before joining though.
  if (is.null(nrow(rdbesPrepObject[["FM"]])) &
      is.null(nrow(rdbesPrepObject[["BV"]]))){

    # Both Fm and BV are null
    bVFM <- NULL
  } else if (is.null(nrow(rdbesPrepObject[["BV"]]))) {

    # BV is null
    bVFM <- NULL
  } else if (is.null(nrow(rdbesPrepObject[["FM"]]))) {

    # Just FM is null
    bVFM <- rdbesPrepObject[["BV"]]
  } else {
gc()
    # if we have both FM and BV data - join them together
    bVFM <- dplyr::right_join(rdbesPrepObject[["FM"]],
      rdbesPrepObject[["BV"]],
      by = "FMid",
      multiple = "all"
    )
    # sort out the wrong SAid column name after the join
    names(bVFM)[names(bVFM) == "SAid.y"] <- "SAid"
    bVFM[, "SAid.x" := NULL]
  }

  # Now get rid of any unneccessary id fields to avoid trouble with field naming
  allowedIds <- c("SAid", "FMid", "BVid")
  unallowedIDs <-
    paste0(names(rdbesPrepObject), "id")[
      !(paste0(names(rdbesPrepObject), "id") %in% allowedIds)
    ]
  unallowedIDs <-
    names(bVFM)[names(bVFM) %in% unallowedIDs]
  if (length(unallowedIDs) > 0) bVFM[, (unallowedIDs) := NULL]

  # Now we join the data to create the output
  allLower <- NULL

  # If we have data join our lowerX with the fmBV or BVFm data
  if (!is.null(nrow(fMBV))){
    lowerA <- dplyr::left_join(lowerA, fMBV, by = "SAid", multiple = "all")
    lowerB <- dplyr::left_join(lowerB, fMBV, by = "SAid", multiple = "all")
    #lowerD <- dplyr::left_join(lowerD, fMBV, by = "SAid")
    lowerD <- NULL
    allLower <- rbind(allLower,lowerA, lowerB, lowerD)
  }

  if (!is.null(nrow(bVFM))){
    # Note the difference in lowerC
    lowerC <- dplyr::left_join(lowerC, bVFM, by = "SAid", multiple = "all")
    allLower <- rbind(allLower,lowerC)
  }

  # Ok, this shoudl be all the FM, and BV data for all lower hierarchies
  allLower
}

#' Private function to process the upper hierarchies when creating
#' the RDBESEstObject
#'
#' @param myRDBESEstObj An RDBESEstObj to add data to
#' @param rdbesPrepObject A prepared RDBESDataObject
#' @param hierarchyToUse The hierarchy we are using
#' @param targetTables The RDBES tables we are interested in
#' @param i Integer to keep track of where we are in the list of tables
#' @param verbose logical. Output messages to console.
#'
#' @return A partial RDBESEstObject with the data from the upper hierarchy
#'
procRDBESEstObjUppHier <- function(myRDBESEstObj = NULL,
                                   rdbesPrepObject,
                                   hierarchyToUse,
                                   i = 1,
                                   targetTables,
                                   verbose = FALSE) {
  thisTable <- targetTables[i]
gc()
  if (thisTable %in% c("FM", "BV") || (i > length(targetTables))) {
    # if we've got to FM or BV, or we've reached the end of the target tables
    # we're done so lets stop
    return(myRDBESEstObj)
  } else {
    if (verbose) {
      print(paste0("Processing ", thisTable))
    }

    # if we don't have an Est object yet we must be at the start - let's go!
    if (is.null(myRDBESEstObj)) {
      myRDBESEstObj <- rdbesPrepObject[[thisTable]][
        rdbesPrepObject[[thisTable]]$DEhierarchy == hierarchyToUse,
      ]
    } else {
      # If we already have an Est object let's join the new data to it

      # get the sampling unit level
      suLevel <- paste0("su", i - 2)

      grep("^SA.+$", thisTable)
      grepl("^SA.+$", "SA2")

      if (i > 1) {
        if (hierarchyToUse == 7 && thisTable == 'SA') {
          joinField <- paste0(targetTables[i - 2], "id")
        } else{
          joinField <- paste0(targetTables[i - 1], "id")
        }
      } else {
        joinField <- NA
      }

      # First get rid of any unneccessary id fields to avoid trouble
      # with field naming
      allowedIds <- c(
        paste0(thisTable, "id"),
        joinField,
        "VDid",
        "SLid",
        paste0(thisTable, "parentID")
      )
      unallowedIDs <-
        paste0(names(rdbesPrepObject), "id")[
          !(paste0(names(rdbesPrepObject), "id") %in% allowedIds)
        ]
      unallowedIDs <-
        names(rdbesPrepObject[[thisTable]])[
          names(rdbesPrepObject[[thisTable]]) %in% unallowedIDs
        ]
      if (length(unallowedIDs) > 0) {
        rdbesPrepObject[[thisTable]][, (unallowedIDs) := NULL]
      }

      # If we're dealing with sub-sampling rename SAxparentID to the join field
      # - shoudl mean we can easily join with the data from the level above
      if (grepl("^SA.+$", thisTable)) {
        names(rdbesPrepObject[[thisTable]])[
          names(rdbesPrepObject[[thisTable]]) == paste0(thisTable, "parentID")
        ] <- joinField
      }

      # Field names of the design variables
      designVariables <- RDBEScore::designVariables

      # if this table has design variable columns
      if (any(names(rdbesPrepObject[[thisTable]])
      %in% paste0(thisTable, designVariables))) {

        # Rename design variables using su ("sampling unit") style names
        # (we'll just ignore any of the "old" names that aren't present)
        data.table::setnames(rdbesPrepObject[[thisTable]],
          old = paste0(thisTable, designVariables),
          new = paste0(suLevel, designVariables),
          skip_absent = TRUE
        )


        # Add a column showing which table this sampling unit is from
        suTable <- paste0(suLevel, "table")
        rdbesPrepObject[[thisTable]][, suTable] <- thisTable
      }
      gc()
      # Join this new table to the existing data
      # myRDBESEstObj <-
      #   dplyr::left_join(myRDBESEstObj,
      #     rdbesPrepObject[[thisTable]],
      #     by = joinField
      #     , multiple = "all"
      #   )
      # use data.table to do a left join instead of dplyr
      # (the order of the tables is to ensure the columns are returned in the right order)
      #myRDBESEstObj <- rdbesPrepObject[[thisTable]][myRDBESEstObj, on = joinField]
      myRDBESEstObj <- myRDBESEstObj[rdbesPrepObject[[thisTable]],  on = joinField]
    }
    gc()
    # recursively call this function
    procRDBESEstObjUppHier(myRDBESEstObj,
      rdbesPrepObject = rdbesPrepObject,
      hierarchyToUse = hierarchyToUse,
      i = i + 1,
      targetTables = targetTables,
      verbose = verbose
    )
  }
}


#' Private function to get sub-sample level and top-level SAid for SA data
#'
#' @param SAdata The SA data to check
#'
#' @returns A data.table with SAid, topLevelSAid and subSampleLevel
#'
prepareSubSampleLevelLookup <- function(SAdata) {

  # Make a shallow copy to avoid modifying the original
  SA <- data.table::copy(SAdata)

  # Ensure SAparentID is numeric
  if (!is.numeric(SA$SAparentID)) {
    SA[, SAparentID := as.numeric(SAparentID)]
  }

  # Get rid of uneeded columns
  SA <- SA[,SAid, SAparentID]

  # Build an index for fast lookup
  #setkey(SA, SAid)

  # Set inital values
  SA$subSampleLevel <- 1L
  SA$topLevelSAid <- SA$SAid

  # Initially, unresolved rows (those with non-NA parent)
  unresolved <- SA[!is.na(SA$SAparentID),]

  while (nrow(unresolved) > 0) {

    #SA_tmp <- dplyr::inner_join(unresolved, SA, by = c("SAparentID" = "SAid"))
    #SA_tmp$subSampleLevel <- SA_tmp$subSampleLevel.x + 1L
    #SA_tmp$topLevelSAid <- SA_tmp$SAparentID
    #SA_tmp$SAparentID <- SA_tmp$SAparentID.y
    # Do an inner join using data.table
    SA_tmp <- SA[unresolved, on = .(SAid = SAparentID), nomatch = 0,
                 .(
                   unresolved_SAparentID = i.SAparentID,  # from unresolved
                   unresolved_SAid = i.SAid,
                   unresolved_subSampleLevel = i.subSampleLevel,
                   unresolved_topLevelSAid = i.topLevelSAid,
                   SA_SAparentID = SAparentID, # from SA
                   SA_SAid = SAid, # I'm not sure this value appears correctly - I don't actually use it anyway
                   SA_subSampleLevel = subSampleLevel,
                   SA_topLevelSAid = topLevelSAid
                 )]
    SA_tmp$SAid <- SA_tmp$unresolved_SAid
    SA_tmp$subSampleLevel <- SA_tmp$unresolved_subSampleLevel + 1L
    SA_tmp$topLevelSAid <- SA_tmp$SA_topLevelSAid
    SA_tmp$SAparentID <- SA_tmp$SA_SAparentID
    SA_tmp <- SA_tmp[, c("SAid", "SAparentID", "subSampleLevel", "topLevelSAid")]

    if (nrow(SA_tmp) == 0) break

    # Update main table
    SA[SA_tmp, `:=`(
      subSampleLevel = i.subSampleLevel,
      topLevelSAid = i.topLevelSAid
    ), on = .(SAid)]

    # Prepare next set of unresolved rows (moving up one level)
    unresolved <- SA_tmp[!is.na(SA_tmp$SAparentID),]
  }

  return(SA[, .(SAid, topLevelSAid, subSampleLevel)])
}


