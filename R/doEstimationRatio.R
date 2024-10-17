#' Title
#'
#' @param RDBESDataObj
#' @param targetValue
#' @param classUnits
#' @param classBreaks
#' @param LWparam
#' @param lowerAux
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples




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



doEstimationRatio <- function(RDBESDataObj,
                              targetValue = "LengthComp",
                              classUnits = "mm",
                              classBreaks = c(100, 300, 10), # cut
                              LWparam = NULL, # vector of two values
                              lowerAux = NULL, # you can use a strongly correlated value present in your data for the estimation of the values of interest
                              verbose = FALSE){

  # Check we have a valid RDBESEstObject before doing anything else
  RDBEScore::validateRDBESDataObject(RDBESDataObj, verbose = FALSE)



# TODO implement for a combination of lower hierarchies
# For now only works on one lower hierarchy at a time

  if(length(unique(RDBESDataObj$SA$SAlowHierarchy)) > 1){
    stop("Multiple lower hierarchies not yet implemented")}

# Do we need both CL and CE? Allow the user to define the population (i.e. effort or landings or both)?

  if(!names(RDBESDataObj) %in% c("CL", "CE")){
    stop("The object does not have population data")
  }

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
    }else if(unique(RDBESEstRatioObj$SAlowHierarchy) == "C"){

      # Only BV data

      # If length exists
      #if weight exists run
      # else ask for a, b
      # else stop

      # if both lengths and weight exist
      if(isTRUE(any(grepl("Length", RDBESEstRatioObj$BV$BVtypeAssess)) & any(grepl("Weight", RDBESEstRatioObj$BV$BVtypeAssess)))){

      }else{
        stop("Not yet implemented")
      }

    }



# Age composition ---------------------------------------------------------
  }else if(targetValue == "AgeComp"){


# LH C --------------------------------------------------------------------


    if(unique(RDBESDataObj$SAlowHierarchy) == "C"){
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


    }else if(unique(RDBESDataObj$SAlowHierarchy) == "A"){



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
