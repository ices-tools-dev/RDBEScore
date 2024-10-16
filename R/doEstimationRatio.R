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



doEstimationRatio <- function(RDBESEstObjectForEstim,
                              targetValue = "LengthComp",
                              classUnits = "mm",
                              classBreaks = c(100, 300, 10), # cut
                              LWparam = NULL, # vector of two values
                              verbose = FALSE){



  # internal fun

  # 1. Combine BV
  # 2. Combine FM

  if(targetValue == "LengthComp"){ # numbers at length

    if(lowerhierarchy %in% c("A", "B"){

      # Only FM data

      # if both lengths and weight exist
       # then do length compo
      # else LW relationship: give a, b parameters
      # else stop

    }else if(lowerhierarchy == "C"){

      # Only BV data

      # If length exists
        #if weight exists run
        # else ask for a, b
        # else stop
      # else stop
    }


  }else if(targetValue == "AgeComp"){ # numbers at age

    if(lowerhierarchy == "C"){
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


    }else if(lowerhierarchy == "A"){

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


  }else if (targetValue == "SexComp") { # sex comp

  }else { # Weight comp
    stop("Not implemented")
  }

}
