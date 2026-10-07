#' Estimate totals of SA variables for all strata of an RDBESDataObject
#'
#' Design-based estimation directly on an RDBESDataObject: builds the design
#' tree ([createDesignTree()]), takes the values of `targetValue` from the
#' lowest SA units ([getLeafValuesSA()]) and estimates the totals and their
#' variances for every stratum of every sampling stage
#' ([doEstimationOnDesignTree()]). For variables from BV or FM, or for ratio
#' estimation to known totals, use those functions and
#' [doRatioEstimationToTotal()] directly.
#'
#' @param RDBESDataObject An RDBESDataObject with a single upper hierarchy.
#' @param targetValue One or more SA fields to estimate, for example
#' "SAsampWtLive".
#' @param na How to treat missing values of `targetValue`, see
#' [getLeafValuesSA()].
#' @param zeroIfNoChildren Optional tables whose units without child units
#' have a zero total, see [doEstimationOnDesignTree()].
#' @param verbose (Optional) If set to TRUE more detailed text will be printed
#' out by the function. Default is FALSE.
#' @param ... Further arguments to [createDesignTree()], e.g.
#' `ratio = c(SA = "weight")`, `methodMap`, `nonResponse`,
#' `strictSampleSize` or `varianceAsWR`.
#'
#' @return A data frame with one row per parent unit, child stratum and
#' target value: `recType` (the table of the units in the stratum),
#' `parentTable`, `parentTableID`, `stratumName`, `targetValue`, the number
#' of sampled units `n.units`, `ratio` (ratio estimator used), `est.total`,
#' the variance components `var.between` and `var.within`, `var.total` and
#' `se.total`. The rows with `recType == "DE"` hold the totals of each DE
#' stratum.
#' @export
#'
#' @examples
#' res <- doEstimationForAllStrata(H8ExampleEE1, "SAsampWtMes",
#'                                 strictSampleSize = FALSE)
#' res[res$recType == "TE", ]
doEstimationForAllStrata <- function(RDBESDataObject,
                                     targetValue,
                                     na = "stop",
                                     zeroIfNoChildren = NULL,
                                     verbose = FALSE,
                                     ...) {
  if (!inherits(RDBESDataObject, "RDBESDataObject")) {
    stop("RDBESDataObject must be of class RDBESDataObject. Estimation is ",
         "done directly on the RDBESDataObject; the (deprecated) ",
         "RDBESEstObject is no longer used for estimation.")
  }
  if (!all(startsWith(targetValue, "SA"))) {
    stop("targetValue must be SA fields. For BV or FM variables use ",
         "createDesignTree(), getLeafValuesBV() or getLeafValuesFM() and ",
         "doEstimationOnDesignTree().")
  }
  tree <- createDesignTree(RDBESDataObject, lowestTable = "SA",
                           verbose = verbose, ...)
  leaves <- getLeafValuesSA(tree, RDBESDataObject, targetValue, na = na)
  strata <- doEstimationOnDesignTree(tree, leaves,
                                     zeroIfNoChildren = zeroIfNoChildren)$strata

  data.frame(recType = strata$table,
             parentTable = sub(":.*$", "", strata$parent),
             parentTableID = sub("^[^:]*:", "", strata$parent),
             stratumName = strata$stratum,
             targetValue = strata$var,
             n.units = strata$n_g,
             ratio = strata$ratio,
             est.total = strata$t,
             var.between = strata$vb,
             var.within = strata$vw,
             var.total = strata$v,
             se.total = strata$se)
}
