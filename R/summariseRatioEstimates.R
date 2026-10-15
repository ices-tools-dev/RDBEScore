#' Summarise raised ratio estimates per domain and class
#'
#' Last step of the ratio estimation workflow (see [doRatioEstimation()]).
#' Sums the raised quantities from [raiseToDE()] over strata, applies an
#' optional plus group and derives the means. Mean weight and mean length are
#' ratios of raised totals, never averages of sample means.
#'
#' The values that belong to a whole domain (estimated live weight, SOP,
#' sample counts) are in [summariseRatioDomains()].
#'
#' @param raised Output of [raiseToDE()].
#' @param plusGroup Optional age. Ages greater than or equal to it are
#'   combined. Only for `target = "Age"`.
#'
#' @return A `data.table` with one row per domain and class: the domain
#'   variables, `age` or `lengthClass_mm` (lower bound of the class), `N`
#'   (numbers), `meanWeight_g` (live), `meanLength_mm`, `totalWeight_kg`
#'   (`N` times `meanWeight_g`) and `nFish` (fish measured or aged).
#' @seealso [doRatioEstimation()], [summariseRatioDomains()]
#' @export
summariseRatioEstimates <- function(raised, plusGroup = NULL) {
  target <- checkRaisedInput(raised)
  domainVars <- attr(raised, "domainVars")
  est <- classTotals(raised, domainVars, target, plusGroup)
  classCol <- if (target == "Age") "age" else "lengthClass_mm"
  out <- est[, c(domainVars, "class", "N", "meanWeight", "meanLength",
                 "totalWeight", "nFish"), with = FALSE]
  # Units in the column names: weights of fish in g, lengths in mm,
  # landings in kg
  data.table::setnames(out,
    c("class", "meanWeight", "meanLength", "totalWeight"),
    c(classCol, "meanWeight_g", "meanLength_mm", "totalWeight_kg"))
  data.table::setorderv(out, c(domainVars, classCol))
  out[]
}

#' Summarise raised ratio estimates per domain
#'
#' The values of [doRatioEstimation()] that belong to a whole domain rather
#' than to one age or length class: the estimated live weight of the species,
#' the sum-of-products check and the number of samples and sampled units.
#'
#' The sum-of-products check is
#' \eqn{SOP = \sum_a N_a \bar w_a / X}, where X (`liveWeightEst_kg`) is the
#' estimated live weight
#' of the species from the sample weights (see [raiseToDE()]). It compares
#' the raised fish weights (BV) with the raised sample weights (SA), and flags
#' unit or weight-presentation errors when it is far from 1. A value of 1 does
#' not prove the weights are right: a consistent presentation error cancels
#' out. `weightCoverage` is the share of the estimated numbers that had an
#' individual weight.
#'
#' @param raised Output of [raiseToDE()].
#'
#' @return A `data.table` with one row per domain: the domain variables,
#'   `liveWeightEst_kg` (estimated live weight of the species),
#'   `liveWeightSampled_kg` (live weight of the samples weighted by the stage
#'   weights, before the DE and totals ratios), `rApplied`
#'   (`liveWeightEst_kg / liveWeightSampled_kg`, the factor applied), `SOP`,
#'   `weightCoverage`, `nSamples` and the number of sampled units per stage:
#'   `nVessels`, `nTrips` and `nHauls` in hierarchy 1, `nOnshoreEvents`,
#'   `nLandingEvents` (and `nTrips` when FT is a stage) in hierarchy 5.
#' @seealso [doRatioEstimation()], [summariseRatioEstimates()]
#' @export
summariseRatioDomains <- function(raised) {
  target <- checkRaisedInput(raised)
  domainVars <- attr(raised, "domainVars")
  est <- classTotals(raised, domainVars, target, plusGroup = NULL)
  dom <- est[, .(sop = sum(totalWeight, na.rm = TRUE),
                 weightCoverage = sum(NW) / sum(N)), by = domainVars]
  dom <- merge(dom, attr(raised, "domainSummary"), by = domainVars,
               all.x = TRUE)
  dom[, SOP := sop / liveWeightEst_kg]
  dom[, sop := NULL]
  dom[, rApplied := liveWeightEst_kg / liveWeightSampled_kg]
  lead <- c(domainVars, "liveWeightEst_kg", "liveWeightSampled_kg", "rApplied",
            "SOP", "weightCoverage", "nSamples")
  data.table::setcolorder(dom, c(lead, setdiff(names(dom), lead)))
  data.table::setorderv(dom, domainVars)
  dom[]
}

#' Check that an object is the output of raiseToDE() and return its target
#' @keywords internal
#' @noRd
checkRaisedInput <- function(raised) {
  target <- attr(raised, "target")
  if (is.null(target) || is.null(attr(raised, "domainVars"))) {
    stop("raised must be the output of raiseToDE().", call. = FALSE)
  }
  target
}

#' Raised totals per domain and class, summed over strata, with the means
#' @keywords internal
#' @noRd
classTotals <- function(raised, domainVars, target, plusGroup) {
  r <- data.table::copy(raised)
  if (!is.null(plusGroup)) {
    if (target != "Age") stop("plusGroup is only used for target = 'Age'.")
    r[class >= plusGroup, class := plusGroup]
  }
  est <- r[, .(N = sum(N), NW = sum(NW), W = sum(W_g), NL = sum(NL),
               L = sum(L_mm), nFish = sum(nFish)), by = c(domainVars, "class")]
  est[, meanWeight := data.table::fifelse(NW > 0, W / NW, NA_real_)]
  est[, meanLength := data.table::fifelse(NL > 0, L / NL, NA_real_)]
  est[, totalWeight := N * meanWeight / 1000]
  est
}
