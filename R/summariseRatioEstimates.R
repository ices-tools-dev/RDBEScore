#' Summarise raised ratio estimates per domain and class
#'
#' Last step of the ratio estimation workflow (see [doRatioEstimation()]).
#' Sums the raised quantities from [raiseToDE()] over strata, applies an
#' optional plus group and derives the means. Mean weight and mean length are
#' ratios of raised totals, never averages of sample means.
#'
#' The sum-of-products check is
#' \eqn{SOP = \sum_a N_a \bar w_a / X}, where X is the estimated live weight
#' of the species from the sample weights (see [raiseToDE()]). It compares
#' the raised fish weights (BV) with the raised sample weights (SA), and flags
#' unit or weight-presentation errors when it is far from 1. A value of 1 does
#' not prove the weights are right: a consistent presentation error cancels
#' out. `weightCoverage` is the share of the estimated numbers that had an
#' individual weight. SOP is only meaningful when it is close to 1.
#'
#' @param raised Output of [raiseToDE()].
#' @param plusGroup Optional age. Ages greater than or equal to it are
#'   combined. Only for `target = "Age"`.
#'
#' @return A `data.table` with the domain variables, `age` or
#'   `lengthClass_mm` (lower bound of the class), `N` (numbers),
#'   `meanWeight_g` (live), `meanLength_mm`, `totalWeight_kg`, `nFish` (fish
#'   measured or aged), and per domain `X_kg` (estimated live weight of the
#'   species), `SOP`, `weightCoverage`, `nSamples`, `sumX_kg` (weighted live
#'   weight of the samples before the DE ratio) and the number of sampled
#'   units per stage: `nVessels`, `nTrips` and `nHauls` in hierarchy 1,
#'   `nOnshoreEvents`, `nLandingEvents` (and `nTrips` when FT is a stage) in
#'   hierarchy 5.
#' @seealso [doRatioEstimation()]
#' @export
summariseRatioEstimates <- function(raised, plusGroup = NULL) {
  target <- attr(raised, "target")
  domainVars <- attr(raised, "domainVars")
  domSum <- attr(raised, "domainSummary")
  if (is.null(target) || is.null(domainVars)) {
    stop("raised must be the output of raiseToDE().", call. = FALSE)
  }
  r <- data.table::copy(raised)
  if (!is.null(plusGroup)) {
    if (target != "Age") stop("plusGroup is only used for target = 'Age'.")
    r[class >= plusGroup, class := plusGroup]
  }
  est <- r[, .(N = sum(N), NW = sum(NW), W = sum(W), NL = sum(NL),
               L = sum(L), nFish = sum(nFish)), by = c(domainVars, "class")]
  est[, meanWeight := data.table::fifelse(NW > 0, W / NW, NA_real_)]
  est[, meanLength := data.table::fifelse(NL > 0, L / NL, NA_real_)]
  est[, totalWeight := N * meanWeight / 1000]

  dom <- est[, .(sop = sum(totalWeight, na.rm = TRUE),
                 weightCoverage = sum(NW) / sum(N)), by = domainVars]
  dom <- merge(dom, domSum, by = domainVars, all.x = TRUE)
  dom[, SOP := sop / X]
  dom[, sop := NULL]

  out <- merge(est[, c(domainVars, "class", "N", "meanWeight", "meanLength",
                       "totalWeight", "nFish"), with = FALSE],
               dom, by = domainVars)
  # Units in the column names: weights of fish in g, lengths in mm,
  # landings and sample weights in kg
  classCol <- if (target == "Age") "age" else "lengthClass_mm"
  data.table::setnames(out,
    c("class", "meanWeight", "meanLength", "totalWeight", "X", "sumX"),
    c(classCol, "meanWeight_g", "meanLength_mm", "totalWeight_kg", "X_kg",
      "sumX_kg"), skip_absent = TRUE)
  lead <- c(domainVars, classCol, "N", "meanWeight_g", "meanLength_mm",
            "totalWeight_kg", "nFish", "X_kg", "SOP", "weightCoverage")
  data.table::setcolorder(out, c(lead, setdiff(names(out), lead)))
  data.table::setorderv(out, lead[seq_len(length(domainVars) + 1L)])
  out[]
}
