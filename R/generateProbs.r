#' Generate vector of selection or inclusion probabilities
#'
#' @param x RDBES data object
#' @param probType  "selection" or "inclusion" for selection and inclusion
#' probabilities respectively
#'
#' @details When the selection method is SRSWR selection probabilities are
#' calculated as \eqn{1 / N} and inclusion probabilities as
#' \eqn{1 - (1 - 1 / N)^n}. When the selection method is SRSWOR selection
#' probabilities are not currently implemented. Inclusion probabilities are
#' calculated as \eqn{n/N}. When the selection method is CENSUS both types of
#' probabilities are set to 1. Probabilities for selection methods UPSWR and
#' UPSWOR are not calculated (they need to be supplied by the user). The same
#' happens with regards to non-probabilistic methods
#' @return A vector or probabilities
#' @export
#'
#' @examples
#' \dontrun{
#' generateProbs(x = Pckg_SDAResources_agstrat_H1_WGRDBES_EST_TEST_1[["VS"]], probType = ("inclusion"))
#' # population size
#' a<-generateProbs(x = Pckg_SDAResources_agstrat_H1_WGRDBES_EST_TEST_1[["VS"]], probType = ("inclusion"))
#' sum(1/a$VSincProb)
#' # returns error
#' generateProbs(x = Pckg_SDAResources_agstrat_H1_WGRDBES_EST_TEST_1[["VS"]], probType = ("selection"))
#'
#' }
generateProbs <- function(x, probType) {

  # Only allow "inclusion" or "selection" probType at the moment
  if (!probType %in% c("inclusion", "selection")) {
    stop(paste0("Unallowed value supplied for 'probType': ", probType))
  }

  methColNames <- grep("^..selectMeth$", names(x), value = TRUE)
  a <- as.character(unique(x[, ..methColNames]))


  if (sum(is.na(a)) > 0) stop("cannot proceed: NAs in SSselectMeth")
  if (length(a) > 1) stop("two different selection methods")

  numSampColNames <- grep("^..numSamp$", names(x), value = TRUE)
  vecSmallN <- x[[numSampColNames]]
  numTotalColNames <- grep("^..numTotal$", names(x), value = TRUE)
  vecBigN <- x[[numTotalColNames]]

  vecProb <- NA

  if (probType == "selection") {
    vecProbColNames <- grep("^..selProb$", names(x), value = TRUE)
    vecProb <- x[[vecProbColNames]]

    print(a)
    if (a %in% c("SRSWR", "SRSWOR")) {
      if (a == "SRSWR") vecProb <- 1 / vecBigN
      if (a == "SRSWOR") stop("depends on order")
    }
    if (a %in% c("UPSWR", "UPSWOR")) {
      if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
      vecProb <- vecProb
    }
    if (a == "CENSUS"){
      vecProb <- rep(1,length(vecProb))
    }
    x[[vecProbColNames]] <- vecProb
  }


  if (probType == "inclusion") {
    vecProbColNames <- grep("^..incProb$", names(x), value = TRUE)
    vecProb <- x[[vecProbColNames]]

    if (length(a) > 1) {
      stop("two different selection methods")
    } else {
      print(a)
      if (a %in% c("SRSWR", "SRSWOR")) {
        if (sum(is.na(vecBigN)) > 0) stop("cannot proceed: NAs in total")
        if (sum(is.na(vecSmallN)) > 0) stop("cannot proceed: NAs in sampled")
        if (a == "SRSWR") vecProb <- 1 - (1 - 1 / vecBigN)^vecSmallN
        if (a == "SRSWOR") vecProb <- vecSmallN / vecBigN
      }
      if (a %in% c("UPSWR", "UPSWOR")) {
        if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
        vecProb <- vecProb
      }
      if (a == "CENSUS"){
        vecProb <- rep(1,length(vecProb))
      }
    }
    x[[vecProbColNames]] <- vecProb
  }

  x
}
