#' Title
#'
#' @param RDBESEstObjectForEstim
#' @param targetValue
#' @param lenClassUnits
#' @param lenClassBreaks
#' @param PlusGroup
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



doEstimationRatio <- function(RDBESEstObjectForEstim,
                              targetValue = "LengthComp",
                              classUnits = "mm",
                              classBreaks = c(100, 300, 10), # cut
                              LWparam = NULL, # vector of two values
                              lowerAux = NULL, # you can use a strongly correlated value present in your data for the estimation of the values of interest
                              verbose = FALSE){

  # Check we have a valid RDBESEstObject before doing anything else
  RDBEScore::validateRDBESEstObject(RDBESEstObjectForEstim, verbose = verbose)

  # Take out SU levels depending on the hierarchy
  suLevels <-
    names(RDBESEstObjectForEstim)[
      grep("^su.table$", names(RDBESEstObjectForEstim))
    ]

# TODO implemented for a combination of lower hierarchies
# For now only works on one lower hierarchy at a time

  if(length(unique(RDBESEstObjectForEstim$SAlowHierarchy)) > 1){
    stop("Multiple lower hierarchies not yet implemented")}




# Length composition ------------------------------------------------------
  if(targetValue == "LengthComp"){


# LH A & B ----------------------------------------------------------------
    if(unique(RDBESEstObjectForEstim$SAlowHierarchy) %in% c("A", "B")){


      # Select only FM data for now - BV possibly used for ALK




      # if both lengths and weight exist
      if(isTRUE(any(grepl("Length", RDBESEstObjectForEstim$BVtypeAssess)) & any(grepl("Weight", RDBESEstObjectForEstim$BVtypeAssess)))){


        # if aux exist use aux
        if(all(!is.na(RDBESEstObjectForEstim[[paste0(substr(tail(suLevels, n = 1), 1, 3), "auxVarValue")]]))){




          # else LW relationship: give a, b parameters
        }else{

          # else stop
          stop("Missing values in auxiliary variable sample table")


        }

        # otherwise check if you can calculate it
        # otherwise stop
      }





# LH C --------------------------------------------------------------------
    }else if(unique(RDBESEstObjectForEstim$SAlowHierarchy) == "C"){

      # Only BV data

      # If length exists
      #if weight exists run
      # else ask for a, b
      # else stop
      # else stop

      stop("Not yet implemented")
    }



# Age composition ---------------------------------------------------------
  }else if(targetValue == "AgeComp"){


# LH C --------------------------------------------------------------------


    if(unique(RDBESEstObjectForEstim$SAlowHierarchy) == "C"){
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


    }else if(unique(RDBESEstObjectForEstim$SAlowHierarchy) == "A"){



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
