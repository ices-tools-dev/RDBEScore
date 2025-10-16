#' Estimate Numbers and Mean Values by Length or Age Class
#'
#' The function is under development and does not work yet.
#'
#' @param RDBESDataObj A validated RDBESDataObject containing hierarchical sampling and biological data. Must include appropriate tables (e.g., CL, CE, SA, FM, or BV) depending on estimation requirements.
#' @param targetValue A character string specifying the type of composition to estimate. Options are "LengthComp" or "AgeComp".
#' @param raiseVar The variable used to construct the ratio.
#' @param classUnits Units of the class intervals for length or age, typically "mm" for millimeters or "cm" for centimeters. Used in defining class intervals.
#' @param classBreaks A numeric vector of three values: minimum value, maximum value, and class width (e.g., c(100, 300, 10)). Defines the class intervals for grouping lengths or ages.
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
                              lowerAux = NULL, # you can use a strongly correlated value present in your data for the estimation of the values of interest
                              verbose = FALSE){

  raiseVar <- "Weight"
  RDBESDataObj <- H8ExampleEE1
  # Check we have a valid RDBESEstObject before doing anything else
  RDBEScore::validateRDBESDataObject(RDBESDataObj, verbose = FALSE)

  # RDBESDataObj <- createRDBESDataObject(input = c("./NLdata/2025_10_14_093927.zip",
  #                                       "./NLdata/HCL_2025_10_06_102840215.zip"))
  # validateRDBESDataObject(h1, verbose = TRUE)

  # Check upper hierarchy
  DEhierarchy <- unique(RDBESDataObj$DE$DEhierarchy)
  if(length(unique(DEhierarchy )) > 1){
    stop("Multiple upper hierarchies not yet implemented")}
  # Check lower hierarchy
  if(length(unique(RDBESDataObj$SA$SAlowHierarchy)) > 1){
    stop("Multiple lower hierarchies not allowed")
  }

  RDBESEstRatioObj <- RDBESDataObj[c("CL", "CE",  RDBEScore::getTablesInRDBESHierarchy(DEhierarchy))]

  # Filter out NULL tables
  RDBESEstRatioObj <- Filter(Negate(is.null), RDBESEstRatioObj)

  # If raiseVar == possible
  possibleValues  <- unique(RDBESDataObj$BV$BVtypeMeas)

  if(!raiseVar %in% possibleValues){

#----------------------
  if(raiseVar == "Weight"){
    if(unique(RDBESDataObj$SA$SAlowHierarchy) == "B" ){
      stop("Lower hierarchy B not yet implemented for weight")
    }else{
      weightVar <- grep("(?i)weight", unique(RDBESDataObj$BV$BVtypeMeas), value = TRUE)
      # weightVar <- c("WeightLive", "WeightGutted", "WeightMeasured")
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



# Do we need both CL and CE? Allow the user to define the population (i.e. effort or landings or both)?

  # Not working check

  # if(!names(RDBESDataObj) %in% c("CL", "CE")){
  #   stop("The object does not have population data")
  # }

# Check which tables exist after SA

# Does anything exist after SA?

 if(length(unique(names(RDBESEstRatioObj))) > 1){
   if(!tail(names(RDBESEstRatioObj), n = 1) %in% c("FM", "BV")){
     stop("No FM or BV tables provided")
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
      fm <- fm[fm, unique(.SD), .SDcols = c("SAid", "FMid", "FMclassMeas", "FMnumAtUnit")]
      sa <- sa[, unique(.SD), .SDcols = c("SAid", "SAlowHierarchy", "SAtotalWtMes" , "SAsampWtMes",  "SAnumTotal", "SAnumSamp", "SAauxVarValue", "SAauxVarUnit" )]
      bv$LengthClass <- floor(bv$LengthTotal/10) # TODO This needs to be defined by the user
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
