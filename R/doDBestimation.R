#' Create design-based point and variance estimates from
#' RDBES estimation object (rdbesEstimObj)
#'
#' @param x a data.frame (or data.table) in rdbesEstimObj format
#' with value of target variable in column targetValue
#' @param estimateType a string with type of estimate. As of now only
#' "total" is defined
#' @param pointEstimator a string with type of point estimator. As
#' of now only "Unbiased" is defined
#' @param varEstimator a string with type of variance estimator. As
#' of now only "WRonPSUviaPik" is defined
#' @param stage a natural number (0,1,..) with sampling stage of
#' estimate. 0 corresponds to DE level.
#' @param domainOfinterest list ofdomains of interest (e.g., SAarea). As
#' of now only NULL (=no domain estimate) is defined
#'
#' @return a list of values for pointEstimate, varEstimate and
#' estimation options
#'
#' @import data.table
#'
#' @examples
#' data(shrimps)
#' doDBestimation (x = shrimps, estimateType = "total",
#' pointEstimator = "Unbiased", varEstimator = "WRonPSUviaPsi",stage = 0,
#' domainOfinterest = NULL )

doDBestimation <- function(x = rdbesEstimObj,
                           estimateType = "total",
                           pointEstimator = "Unbiased",
                           varEstimator = "WRonPSUviaPik",
                           stage = 0,
                           domainOfinterest = NULL) {

if (!is.data.frame(x)) stop("x must be data.frame or data.table")
if (!is.data.table(x)) {
  print(1)
  x <- data.table(x)
}

if (stage == 0) {
  targetProbColumns <- colnames(x)[grepl(colnames(x), pattern = "inclusionProb") &
                                     !grepl(colnames(x), pattern = "Cluster")]
} else {
  stop("stage not implemented")
}
x$finalInclusionProb <- apply(x[, ..targetProbColumns], 1, prod)

if (estimateType == "total") {
  if (pointEstimator == "Unbiased") {

    # pointEstimate
    pointEstimate <- sum(1 / x$finalInclusionProb * x$targetValue)

    # varEstimate
    if (varEstimator == "WRonPSUviaPik") {
      targetProbColumns2 <- targetProbColumns[!grepl(targetProbColumns,
                                                     pattern = "su1inclusionProb")]
      x$finalInclusionProb_to_su1 <- apply(x[, ..targetProbColumns2], 1, prod)

      aux1 <- x[,
                list(su1_total = sum(1 / finalInclusionProb_to_su1 * targetValue)),
                list(su1unitName, su1inclusionProb)]
      aux1 <- aux1$su1_total / aux1$su1inclusionProb
      # mean of estimates at psu level [
      aux2 <- pointEstimate / unique(x$su1numberSampled)
      # final variance
      varPointEstimate <-
        sum((aux1 - aux2)^2) * unique(x$su1numberSampled) / (unique(x$su1numberSampled) - 1)
    }

    if (varEstimator == "WRonPSUviaPsi") {
      target_prob_columns2 <-
        targetProbColumns[!grepl(targetProbColumns, pattern = "su1inclusionProb")]
      x$finalInclusionProb_to_su1 <- apply(x[, ..target_prob_columns2], 1, prod)

      aux1 <-
        x[, list(su1_total = sum(1 / finalInclusionProb_to_su1 * targetValue)),
          list(su1unitName, su1selectionProb)]
      aux1 <- aux1$su1_total / aux1$su1selectionProb

      # final variance
      varPointEstimate <-
        1 / (unique(x$su1numberSampled) * (unique(x$su1numberSampled) - 1)) * sum((aux1 - pointEstimate)^2)
    }
  } else {
    stop("pointEstimator not implemented")
  }
} else {
  stop("estimateType not implemented")
}

list(
  pointEstimate = pointEstimate, varPointEstimate = varPointEstimate,
  sePointEstimate = sqrt(varPointEstimate),
  options = list(estimateType = estimateType, pointEstimator = pointEstimator,
                 varEstimator = varEstimator, stage = stage,
                 domainOfinterest = domainOfinterest)
)
}

