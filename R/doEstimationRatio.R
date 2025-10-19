#' Estimate Numbers and Mean Values by Length or Age Class
#'
#' The function is under development and does not work yet.
#'
#' @param RDBESDataObj A validated RDBESDataObject containing hierarchical sampling and biological data. Must include appropriate tables (e.g., CL, CE, SA, FM, or BV) depending on estimation requirements.
#' @param targetValue A character string specifying the type of composition to estimate. Options are "LengthComp" or "AgeComp".
#' @param raiseVar The variable used to construct the ratio.
#' @param classUnits Units of the class intervals for length or age, typically "mm" for millimeters or "cm" for centimeters. Used in defining class intervals. In lower hierarchy A, the argument is used to check if that the length classes provided. For lower hierarchy B, it transforms the lengths. Codes: https://vocab.ices.dk/?ref=1608
#' @param classBreaks A numeric vector of three values: minimum value, maximum value, and class width (e.g., c(100, 300, 10)). Defines the class intervals for grouping lengths. In lower hierarchy A, the argument is used to check if that the length classes provided. For lower hierarchy B, it transforms the lengths.
#' @param LWparam A numeric vector of length two specifying parameters (a, b) for the weight-length relationship (W = a * L^b). Used if no direct weights are available but lengths are provided.
#' @param lowerAux A numeric or character vector referencing a variable in the SA table used as an auxiliary variable for ratio estimation (e.g., sample weights, sub-sample expansion factors).
#' @param verbose Logical; if TRUE, detailed messages are printed during processing.
#'
#' @return A list or data.table containing the estimated numbers at length or age and associated mean values such as weight and length, depending on input and target type.
# InterCatch age composition data
# Numbers at age
# Mean weight at age
# Mean length at age
# Intercatch length composition data
# Numbers at legth
# Mean weight at length
## Measure indv weight of fish
## LW relationship: a, b parameters
# For now assume we have all the data we need
# Counts, indv weights, lengths (mm) and ages
# TODO check differences if length classes are defined later
# Unit conversion later step : for LC
# TODO add check for unique sampling scheme
# TODO add an argument to toggle stratification in estimation on/off
# TODO need to implement the BV conversion from typeMeas to typeAssess
# TODO add info about the strata (species, area, season, metier/gear/fleet)
#' @importFrom utils tail
doEstimationRatio <- function(RDBESDataObj,
                              targetValue = "LengthComp",
                              raiseVar = "Weight",
                              classUnits = "mm",
                              classBreaks = c(100, 300, 10), # cut
                              LWparam = NULL, # vector of two values
                              lowerAux = NULL, # should we keep this ?The aux var is now included as a field in the RDBES
                              verbose = FALSE){
RDBESDataObj <- myFilteredObject
targetValue <- "AgeComp"
  raiseVar <- "Weight"
  # H1 <- H1Example
  #
  # myFields <- c("SAlowHierarchy")
  # myValues <- c("A")
  # RDBESDataObj <- filterRDBESDataObject(H1,
  #                                           fieldsToFilter = myFields,
  #                                           valuesToFilter = myValues,
  #                                           strict = FALSE, # this is to skip the validation function
  #                                           killOrphans = TRUE)


  classUnits = "mm"
  classBreaks = c(10, 100, 10)


# Checks ------------------------------------------------------------------
  # Check we have a valid RDBESEstObject before doing anything else
  # Need that because the selection is by column name
  RDBEScore::validateRDBESDataObject(RDBESDataObj, verbose = FALSE)

  # Unique upper hierarchy
  if(length(unique(RDBESDataObj$DE$DEhierarchy)) > 1){
    stop("Multiple upper hierarchies not implemented")}
  # Unique lower hierarchy
  if(length(unique(RDBESDataObj$SA$SAlowHierarchy)) > 1){
    stop("Multiple lower hierarchies not allowed")
  }

  # Filter out NULL tables
  RDBESEstRatioObj <- Filter(Negate(is.null),RDBESDataObj)

  # If no individual weight of fish in BV, then can't run raiseVar = Weight
  # because we don't have the weight of the subsample
  if(unique(RDBESEstRatioObj$SA$SAlowHierarchy) == "A") {
    weightVar <- grep("(?i)weight", unique(RDBESEstRatioObj$BV$BVtypeMeas), value = TRUE)
    if (is.null(    weightVar) || length(weightVar) == 0 || all(is.na(weightVar))){
      stop("no individual weight measured")
    }
  }

  # Does anything exist after SA?
  # Do we need that?

  if(length(unique(names(RDBESEstRatioObj))) > 1){
    if(!tail(names(RDBESEstRatioObj), n = 1) %in% c("FM", "BV")){
      stop("No FM or BV tables provided")
    }
  }

  # Add if object has only one species

  # if(length(unique(myFilteredObject$SA$SAspeCode)) > 1){
  #     stop("Multiple species raising not yet implemented")
  # }

  # Function to check consistency between the user input and the FM table for
  # length raising
  # TODO Should this be moved to utils?

  checkLC <- function(fm, classUnits, classBreaks) {
    # https://vocab.ices.dk/?ref=1608
    vocabUnits <-   c("mm", "25mm", "cm", "5cm", "scm", "smm")
    fmUnits <- unique(fm$FMaccuracy)
    # Check user input validity
    if (!classUnits %in% vocabUnits) {
      stop(paste("Invalid classUnits:", classUnits,
                 "\nMust be one of:", paste(vocabUnits, collapse = ", ")))
    }

    if (length(fmUnits) > 1) {
      # Needs to break here
      stop("Multiple class units found in data: ", paste(fmUnits, collapse = ", "))
    }

    # Compare user input with data
    if (!classUnits %in% fmUnits) {
      stop(paste("Mismatch between user-specified classUnits (", classUnits,
                 ") and data units (", paste(fmUnits, collapse = ", "), ")."))
    }

    # Check classBreaks consistency
    fmBreaks <- range(fm$FMclassMeas, na.rm = TRUE)
    userRange <- range(seq(classBreaks[1], classBreaks[2], classBreaks[3]))

    if (any(userRange != fmBreaks)) {
      stop("classBreaks (", paste(userRange, collapse = "-"),
              ") differ from observed data range (", paste(fmBreaks, collapse = "-"), ").")
    }

    message("Length class checks passed: Units and breaks are consistent.")
  }

  # TODO (to be developed) match with pop (Landings or Effort)
  # RDBESEstRatioObj <- RDBESDataObj[c("CL", "CE",  RDBEScore::getTablesInRDBESHierarchy(DEhierarchy))]


# raiseVar options --------------------------------------------------------

  # Can have multiple types of weight measured for the same individual
  # If the user defined in the raiseVar argument one of the options in the ICES vocab for
  # the weight codes in the field BVtypeMeas
  # If there is only one present, this is used by default
  # If more than one are present, allow the user to choose
  possibleValues  <- unique(RDBESDataObj$BV$BVtypeMeas)
  if(!raiseVar %in% possibleValues){
    if(raiseVar == "Weight"){
      if(unique(RDBESDataObj$SA$SAlowHierarchy) == "B" ){
        stop("Lower hierarchy B not implemented for weight")
      }else{
        weightVar <- grep("(?i)weight", unique(RDBESDataObj$BV$BVtypeMeas), value = TRUE)
        if (interactive()) {
          if(length(unique(weightVar)) > 1) {
            # Print a numbered menu and get user's selection
            idx <- utils::menu(weightVar, title = "Select the BV weight type to use:")
            if (idx == 0L) stop("Selection cancelled.")
            wcol <- weightVar[idx]
          } else {
            message("Only one weight type present. Using: ", weightVar[1L])
            wcol <- weightVar[1L]
          }
        }
      }
    }
  }






# Length composition ------------------------------------------------------
  if(targetValue == "LengthComp"){


# LH A & B ----------------------------------------------------------------
    if(unique(RDBESEstRatioObj$SA$SAlowHierarchy) %in% c("A", "B")){

      # TODO mean weight at length

      if(!is.null(LWparam)){

        stop("Not yet implemented")


      }else{
        # else stop
        stop("Nor an auxiliary variable nor lw params are provided. Not possible to produce the mean weight at length")
      }
      # Select only FM data for now - BV possibly used for ALK
      warning("If lower hierarchy A, only the FM table is used to calculate the numbers at length.")

      fm <- data.table::setDT(RDBESEstRatioObj$FM)
      sa <- data.table::setDT(RDBESEstRatioObj$SA)

      # Need to check uniqueness of FM length type assess
      if(length(unique(fm$FMtypeAssess)) > 1){
        stop("The measurement type of the class needed for assessment (FMtypeAssess) needs to be unique")
      }

      # It should break here if there is no match
      checkLC(
        fm = fm,
        classUnits = classUnits,
        classBreaks = classBreaks
      )

      fm <- fm[fm, unique(.SD), .SDcols = c("SAid", "FMid", "FMclassMeas", "FMnumAtUnit", "FMaccuracy", "FMtypeAssess")]
      sa <- sa[, unique(.SD), .SDcols = c("SAid", "SAlowHierarchy", "SAtotalWtMes" , "SAsampWtMes",  "SAnumTotal", "SAnumSamp", "SAauxVarValue", "SAauxVarUnit" )]

      brks <- seq(classBreaks[1], classBreaks[2], by = classBreaks[3])
      fm[, LengthClass := cut(
        FMclassMeas,
        breaks = brks,
        right = FALSE,             # [)
        include.lowest = TRUE,
        labels = head(brks, -1)
      )]

      fm1 <- fm[
        , .(FMNumbersAtLength = .N),
        by = .(SAid, LengthClass)
      ][
        # add total count per SAid
        , FMTotCount := sum(FMNumbersAtLength), by = SAid
      ]

      su <- merge(fm1, sa, by = c("SAid"))

      if(raiseVar == "Weight"){

        su$raiseFactor <- su$SAtotalWtMes/su$SAsampWtMes
        su$NumbersAtLength <- su$raiseFactor*su$FMNumbersAtLength

      }else if(raiseVar == "Count"){

        su$raiseFactor <- su$SAnumTotal/su$SAnumSamp
        su$NumbersAtLength <- su$raiseFactor*su$FMNumbersAtLength

      }else{

        su$NumbersAtLength <- su$SAauxVarValue*su$FMNumbersAtLength

      }

      return(su)





# LH C --------------------------------------------------------------------
    }else if(unique(RDBESEstRatioObj$SA$SAlowHierarchy) == "C"){


      bv <- data.table::setDT(RDBESEstRatioObj$BV)
      bv <- bv[, unique(.SD), .SDcols = c("SAid", "BVfishId", "BVtypeMeas", "BVvalueMeas")]
      bv <- dcast(bv, ... ~ BVtypeMeas , value.var = c("BVvalueMeas"), drop = TRUE)
      bv[, BVweight := as.numeric(get(wcol))]
      bv[, LengthTotal := as.numeric(LengthTotal)]
      # TODO this probably needs to be an argument
      # or needs to be defined later on?
      bv$LengthClass <- floor(bv$LengthTotal/10) # TODO This needs to be defined by the user

      bv1 <- bv[
        , .(BVMeanWeight = mean(BVweight, na.rm = TRUE),
            BVNumbersAtLength = .N),
        by = .(SAid, LengthClass)
      ][
        # add total count per SAid
        , BVTotCount := sum(BVNumbersAtLength), by = SAid
      ][
        # add total weight per SAid
        bv[, .(BVTotWeight = sum(BVweight, na.rm = TRUE)), by = SAid],
        on = "SAid"
      ]

      # bv1$BVLengthClassProp <- bv1$BVNumbersAtLength/bv1$TotCount


      sa <- data.table::setDT(RDBESEstRatioObj$SA)
      sa <- sa[, unique(.SD), .SDcols = c("SAid", "SAlowHierarchy", "SAtotalWtMes" , "SAsampWtMes",  "SAnumTotal", "SAnumSamp", "SAauxVarValue", "SAauxVarUnit" )] # Do not need the
      # species, the filtering of the "strata" variables will be done before the estimation
      # To test
      sa[, SAauxVarValue  := as.numeric(SAauxVarValue )]
      sa$SAauxVarValue <- 10

      # TODO add check for subsampling

      su <- merge(bv1, sa, by = c("SAid"))

      if(raiseVar == "Weight"){

        su$raiseFactor <- su$SAtotalWtMes/su$SAsampWtMes
        su$NumbersAtLength <- su$raiseFactor*su$BVNumbersAtLength

      }else if(raiseVar == "Count"){

        su$raiseFactor <- su$SAnumTotal/su$SAnumSamp
        su$NumbersAtLength <- su$raiseFactor*su$BVNumbersAtLength

      }else{

        su$NumbersAtLength <- su$SAauxVarValue*su$BVNumbersAtLength

      }

      return(su)



      # su$SANumbersAtLength <- su$BVNumbersAtLength * su$SAauxVarValue

      # From here onwards need the column names

      # s <- RDBEScore::getTablesInRDBESHierarchy(DEhierarchy)
      # keywords <- c("SA|FM|BV")
      # hierarchyTabs <-s[!is.na(gsub(keywords, NA, s))]
      # # Get the table before the SA (most likely SS)
      # nextTab <- tail(hierarchyTabs, n = 1)
      # # Get table SA - 2
      # nextTab1 <- tail(hierarchyTabs, n = )
      #
      #
      # upperHier1 <- setDT(RDBESEstRatioObj[[nextTab]])
      # cnames1 <- paste0(nextTab, "year")
      # upperHier1 <- upperHier1[, unique(.SD), .SDcols = c("SAid", "SAspeCode","SAlowHierarchy", "SAauxVarValue")]



      # if both lengths and weight exist
      # if(isTRUE(any(grepl("Length", RDBESEstRatioObj$BV$BVtypeAssess)) & any(grepl("Weight", RDBESEstRatioObj$BV$BVtypeAssess)))){
      #
      # su$Wratio <- su$SAtotalWtMes/su$SAsampWtMes
      # su$SANumbersAtLength <- su$BVNumbersAtLength * su$Wratio
      #
      # }else{
      #   stop("Not yet implemented")
      # }

    }



    # Age composition ---------------------------------------------------------
  }else if(targetValue == "AgeComp"){



    # LH C --------------------------------------------------------------------


    if(unique(RDBESDataObj$SA$SAlowHierarchy) == "C"){

      bv <- data.table::setDT(RDBESEstRatioObj$BV)
      bv <- bv[, unique(.SD), .SDcols = c("SAid", "BVfishId", "BVtypeMeas", "BVvalueMeas")]
      bv <- dcast(bv, ... ~ BVtypeMeas , value.var = c("BVvalueMeas"), drop = TRUE)
      bv[, BVweight := as.numeric(get(wcol))]
      # TODO this probably needs to be an argument
      # or needs to be defined later on?

      bv1 <- bv[
        , .(BVMeanWeight = mean(BVweight, na.rm = TRUE),
            BVNumbersAtAge = .N),
        by = .(SAid, Age)
      ][
        # add total count per SAid
        , BVTotCount := sum(BVNumbersAtAge), by = SAid
      ][
        # add total weight per SAid
        bv[, .(BVTotWeight = sum(BVweight, na.rm = TRUE)), by = SAid],
        on = "SAid"
      ]

      # bv1$BVLengthClassProp <- bv1$BVNumbersAtLength/bv1$TotCount


      sa <- data.table::setDT(RDBESEstRatioObj$SA)
      sa <- sa[, unique(.SD), .SDcols = c("SAid", "SAlowHierarchy", "SAtotalWtMes" , "SAsampWtMes",  "SAnumTotal", "SAnumSamp", "SAauxVarValue", "SAauxVarUnit" )] # Do not need the
      # species, the filtering of the "strata" variables will be done before the estimation
      # To test
      sa[, SAauxVarValue  := as.numeric(SAauxVarValue )]
      sa$SAauxVarValue <- 10

      # TODO add check for subsampling

      su <- merge(bv1, sa, by = c("SAid"))

      if(raiseVar == "Weight"){

        su$raiseFactor <- su$SAtotalWtMes/su$SAsampWtMes
        su$NumbersAtAge <- su$raiseFactor*su$BVNumbersAtAge

      }else if(raiseVar == "Count"){

        su$raiseFactor <- su$SAnumTotal/su$SAnumSamp
        su$NumbersAtAge <- su$raiseFactor*su$BVNumbersAtAge

      }else{

        su$NumbersAtAge <- su$SAauxVarValue*su$BVNumbersAtAge

      }

      return(su)
      # Check which biol data are present

      # if age exists

      # if indv weights + lengths  exist

      # then Full data set back

      # if only indv weights
      # then you don't have the mean length at age unless you use the inverse LW relationship :provide a, b parameters or model them (Future work)
      # if only lengths
      # the you don't have the mean weight at age unless LW: a, b or model (Future work)
      # else stop you don't sufficient data

      # else stop


      # LH A --------------------------------------------------------------------


    }else if(unique(RDBESDataObj$SA$SAlowHierarchy) == "A"){

      bv <- data.table::setDT(RDBESEstRatioObj$BV)
      fm <- data.table::setDT(RDBESEstRatioObj$FM)
      sa <- data.table::setDT(RDBESEstRatioObj$SA)
      bv <- bv[, unique(.SD), .SDcols = c( "FMid","BVfishId", "BVtypeMeas", "BVvalueMeas")]
      bv <- dcast(bv, ... ~ BVtypeMeas , value.var = c("BVvalueMeas"), drop = TRUE)
      bv[, BVweight := as.numeric(get(wcol))]

      bv1 <- bv[
        , .(BVMeanWeight = mean(BVweight, na.rm = TRUE),
            BVNumbersAtAge = .N),
        by = .(FMid, Age)
      ][
        # add total count per SAid
        , BVTotCount := sum(BVNumbersAtAge), by = FMid
      ][
        # add total weight per SAid
        bv[, .(BVTotWeight = sum(BVweight, na.rm = TRUE)), by = FMid],
        on = "SAid"
      ]



      fm <- fm[fm, unique(.SD), .SDcols = c("SAid", "FMid", "FMclassMeas", "FMnumAtUnit")]
      sa <- sa[, unique(.SD), .SDcols = c("SAid", "SAlowHierarchy", "SAtotalWtMes" , "SAsampWtMes",  "SAnumTotal", "SAnumSamp", "SAauxVarValue", "SAauxVarUnit" )]

      fm1 <- unique(
        fm[FMclassMeas %chin% c("LengthTotal","LengthMeasured","Length"),
           .(FMid, SAid, FMnumAtUnit)]
      )

      bv1 <- fm_len[bv1, on = "FMid"][,
                                      num_raise := fifelse(BVTotCount > 0, FMnumAtUnit / BVTotCount, NA_real_)
      ][
        , N_at_age := BVNumbersAtAge * num_raise
      ]



      # subsample -> sample weights -> weight from where the sample came from

      # if age exists

      # if indv weights + lengths  exist

      # then Full data set back

      # if only indv weights
      # then you don't have the mean length at age unless you use the inverse LW relationship :provide a, b parameters or model them (Future work)
      # if only lengths
      # the you don't have the mean weight at age unless LW: a, b or model (Future work)
      # else stop you don't sufficient data

      # else stop

      # TODO include FM. For now the FM is not yet implemented
    }else{
      stop("Age composition can't be calculated with lower hierachy B.")
    }
  }

}
