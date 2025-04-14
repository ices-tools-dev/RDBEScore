#' Estimate Numbers and Mean Values by Length or Age Class
#'
#' The function is under development and does not work yet.
#'
#' @param RDBESDataObj A validated RDBESDataObject containing hierarchical sampling and biological data. Must include appropriate tables (e.g., CL, CE, SA, FM, or BV) depending on estimation requirements.
#' @param targetValue A character string specifying the type of composition to estimate. Options are "LengthComp" or "AgeComp".
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
#' @importFrom utils tail
doEstimationRatio <- function(RDBESDataObj,
                              targetValue = "LengthComp",
                              classUnits = "mm",
                              classBreaks = c(100, 300, 10), # cut
                              LWparam = NULL, # vector of two values
                              lowerAux = NULL, # you can use a strongly correlated value present in your data for the estimation of the values of interest
                              verbose = FALSE){


  RDBESDataObj <- H8ExampleEE1
  # Check we have a valid RDBESEstObject before doing anything else
  RDBEScore::validateRDBESDataObject(RDBESDataObj, verbose = FALSE)



# TODO implement for a combination of lower hierarchies
# For now only works on one lower hierarchy at a time

  if(length(unique(RDBESDataObj$SA$SAlowHierarchy)) > 1){
    stop("Multiple lower hierarchies not yet implemented")}

# Do we need both CL and CE? Allow the user to define the population (i.e. effort or landings or both)?

  # Not working check

  # if(!names(RDBESDataObj) %in% c("CL", "CE")){
  #   stop("The object does not have population data")
  # }

# Order table based on hierarchy - it's a bottom up estimation
  DEhierarchy <- unique(RDBESDataObj$DE$DEhierarchy)
  if(length(unique(DEhierarchy )) > 1){
    stop("Multiple upper hierarchies not yet implemented")}

  RDBESEstRatioObj <- RDBESDataObj[c("CL", "CE",  RDBEScore::getTablesInRDBESHierarchy(DEhierarchy))]

# Filter out NULL tables
  RDBESEstRatioObj <- Filter(Negate(is.null), RDBESEstRatioObj)

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


      # Select only FM data for now - BV possibly used for ALK
      warning("Only FM table used. BV is not yet implemented")








        # if aux exist use aux
        if(all(!is.na(RDBESDataObj[[paste0(substr(tail(suLevels, n = 1), 1, 3), "auxVarValue")]]))){




          # else LW relationship: give a, b parameters
        }else if(!is.null(LWparam)){

            stop("Not yet implemented")


        }else{
          # else stop
          stop("Nor an auxiliary variable nor lw params are provided. Not possible to produce the mean weight at length ")
        }

        # otherwise check if you can calculate it
        # otherwise stop






# LH C --------------------------------------------------------------------
    }else if(unique(RDBESEstRatioObj$SA$SAlowHierarchy) == "C"){


      bv <- setDT(RDBESEstRatioObj$BV)
      bv <- bv[, unique(.SD), .SDcols = c("SAid", "BVfishId", "BVtypeMeas", "BVvalueMeas")]
      bv <- dcast(bv, ... ~ BVtypeMeas , value.var = c("BVvalueMeas"), drop = TRUE)
      bv[, `:=`(LengthTotal = as.numeric(LengthTotal), WeightMeasured = as.numeric(WeightMeasured))]
      # TODO this probably needs to be an argument
      # or needs to be defined later on?
      bv$LengthClass <- floor(bv$LengthTotal/10) # or ceiling option? Half cm?

      bv1 <- bv[, .(BVMeanWeight = mean(WeightMeasured, na.rm = TRUE),
                    BVNumbersAtLength = .N),
                by = .(SAid, LengthClass)][, TotCount := sum(BVNumbersAtLength), by = SAid]
      bv1$BVLengthClassProp <- bv1$BVNumbersAtLength/bv1$TotCount


      sa <- setDT(RDBESEstRatioObj$SA)
      sa <- sa[, unique(.SD), .SDcols = c("SSid","SAid", "SAspeCode","SAlowHierarchy", "SAauxVarValue")]

      # To test
      sa[, SAauxVarValue  := as.numeric(SAauxVarValue )]
      sa$SAauxVarValue <- 10

      # TODO add check for subsampling STOP it does not work

      su <- merge(bv1, sa, by = c("SAid"))

      su$SANumbersAtLength <- su$BVNumbersAtLength * su$SAauxVarValue

      # From here onwards need the column names

      s <- RDBEScore::getTablesInRDBESHierarchy(DEhierarchy)
      keywords <- c("SA|FM|BV")
      hierarchyTabs <-s[!is.na(gsub(keywords, NA, s))]
      # Get the table before the SA (most likely SS)
      nextTab <- tail(hierarchyTabs, n = 1)
      # Get table SA - 2
      nextTab1 <- tail(hierarchyTabs, n = )


      upperHier1 <- setDT(RDBESEstRatioObj[[nextTab]])
      cnames1 <- paste0(nextTab, "year")
      upperHier1 <- upperHier1[, unique(.SD), .SDcols = c("SAid", "SAspeCode","SAlowHierarchy", "SAauxVarValue")]



      # if both lengths and weight exist
      # if(isTRUE(any(grepl("Length", RDBESEstRatioObj$BV$BVtypeAssess)) & any(grepl("Weight", RDBESEstRatioObj$BV$BVtypeAssess)))){
      #
      #
      #
      # }else{
      #   stop("Not yet implemented")
      # }

    }



# Age composition ---------------------------------------------------------
  }else if(targetValue == "AgeComp"){


# LH C --------------------------------------------------------------------


    if(unique(RDBESDataObj$SA$SAlowHierarchy) == "C"){
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
    }
  }

}
