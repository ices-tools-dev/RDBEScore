#' Estimate a total under multistage SRSWOR sampling
#'
#' Estimate a population total and its analytical variance, optionally with
#' Preston's multistage rescaled bootstrap, from an RDBESEstObject.
#' Only simple random sampling without replacement (SRSWOR) and census stages
#' are supported. Selection with replacement, unequal probabilities and explicit
#' RDBES clustering are rejected rather than approximated.
#'
#' @param estObject An RDBESEstObject created by [createRDBESEstObject()] with
#'   default strict validation, stopping at the table containing `targetValue`.
#' @param targetValue Name of the numeric response column. Every terminal
#'   sampling unit must have one response row. Missing responses are omitted
#'   with a warning. Non-missing responses must be finite.
#' @param nBoot Number of bootstrap replicates. Use zero (the default) for
#'   analytical estimation only, or an integer of at least two. Use [set.seed()]
#'   before calling for reproducible replicates.
#' @param confidenceLevel Confidence level for normal-approximation intervals,
#'   strictly between zero and one.
#' @param censusLinks Character vector of table names explicitly identifying
#'   artificial one-to-one census links. Each must have method `CENSUS`, equal
#'   declared sample and population sizes, and exactly one observed child per
#'   parent. Such links have effective sample and population sizes of one.
#'   A warning identifies any discrepancy with the declared counts. This is
#'   intended for documented dummy tables in converted teaching datasets.
#' @param singleton How to handle a non-census group with one sampled unit.
#'   `"zero"` (the default) sets that group's variance contribution to zero and
#'   warns. `"fail"` stops because its variance cannot be estimated. The zero
#'   convention can understate uncertainty and is not a solution for singleton
#'   sampling designs.
#'
#' @details
#' Sample and population sizes are checked within each parent and stratum.
#' One DE/SD branch is supported. Nested SA subsampling is not supported.
#' Expansion uses the recorded sample and population sizes. Supplied inclusion
#' probabilities must agree with `n/N`. Supplied selection probabilities are
#' checked for validity and equality within each group but do not define the
#' expansion weights.
#' Census stages contribute no sampling variance of their own, but retain
#' variance from any sampled descendants. Missing responses are omitted from
#' point and bootstrap sums, with the original sample and population sizes retained.
#' This performs no nonresponse adjustment and does not establish that a missing
#' response is a true zero. Totals may consequently understate the population
#' total. The analytical variance, standard error and interval are unavailable
#' (`NA`) when any responses are missing. Bootstrap uncertainty then describes
#' only the observed response contributions and excludes nonresponse uncertainty.
#' An entirely missing response vector is rejected.
#'
#' For child totals \eqn{t_i}, estimated child variances \eqn{v_i}, sample size
#' \eqn{n} and population size \eqn{N}, the recursion is
#' \deqn{\hat{T} = (N/n)\sum_i t_i}
#' \deqn{\hat{V} = N^2(1-n/N)s_t^2/n + (N/n)\sum_i v_i.}
#' Independent strata are added within each parent before proceeding upwards.
#'
#' The bootstrap uses an SRSWOR subsample of size \eqn{\lfloor n/2\rfloor}
#' within each parent and stratum, with the finite population corrections and
#' additive multistage weight recurrence in Preston (2009). It does not multiply
#' independently rescaled stage weights. Census singleton links leave later
#' stages active. Under the explicit `singleton = "zero"` convention,
#' non-census singletons also have zero perturbation.
#'
#' Both intervals are Wald intervals centred on the original total. The
#' bootstrap variance is the sample variance of the replicate totals. These
#' intervals assume an adequate normal approximation. They are not percentile
#' intervals, and no small-sample coverage guarantee is made.
#'
#' This is a restricted companion to [doEstimationForAllStrata()], which
#' supports additional sampling methods. It does not change that estimator.
#'
#' @return A list with `estimates`, a data frame containing `method`,
#'   `est.total`, `var.total`, `se.total`, `lower` and `upper`; `replicates`,
#'   the bootstrap totals (empty if `nBoot = 0`); and `design`, the checked
#'   stage mapping including effective and originally declared counts.
#'
#' @references
#' Preston, J. (2009). Rescaled bootstrap for stratified multistage sampling.
#' Survey Methodology 35(2), 227-234.
#' <https://www150.statcan.gc.ca/n1/pub/12-001-x/2009002/article/11044-eng.pdf>
#'
#' The investigation of IEOstats motivated this implementation. Its algorithms
#' were assessed rather than imported as a package dependency.
#' <https://github.com/joel-ieo/IEOstats>
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: Pending
#' - Notes/scope: Restricted to SRSWOR/census designs. Independent
#'   scientific review is pending.
#'
#' @export
#'
#' @examples
#' H1DataObj <- Pckg_SDAResources_agstrat_H1
#' H1EstObj <- createRDBESEstObject(H1DataObj, 1, stopTable = "SA")
#' set.seed(278)
#' result <- estimateSrsworTotal(H1EstObj, "SAsampWtMes", nBoot = 99)
#' result$estimates
estimateSrsworTotal <- function(estObject, targetValue, nBoot = 0L,
                                confidenceLevel = 0.95,
                                censusLinks = character(),
                                singleton = c("zero", "fail")) {
  singleton <- match.arg(singleton)
  if (!is.numeric(nBoot) || length(nBoot) != 1L || is.na(nBoot) ||
      !is.finite(nBoot) || nBoot != floor(nBoot) ||
      !(nBoot == 0 || nBoot >= 2) || nBoot > .Machine$integer.max) {
    stop("nBoot must be zero or an integer of at least two.", call. = FALSE)
  }
  if (!is.numeric(confidenceLevel) || length(confidenceLevel) != 1L ||
      is.na(confidenceLevel) || !is.finite(confidenceLevel) ||
      confidenceLevel <= 0 || confidenceLevel >= 1) {
    stop("confidenceLevel must be a number strictly between zero and one.",
         call. = FALSE)
  }

  prepared <- prepareSrsworData(estObject, targetValue, censusLinks)
  if (anyNA(prepared$response)) {
    warning(paste0(sum(is.na(prepared$response)), " missing response(s) are ",
                   "omitted without changing design counts or adjusting for ",
                   "nonresponse. Analytical variance is unavailable. Bootstrap ",
                   "uncertainty excludes nonresponse uncertainty."),
            call. = FALSE)
  }
  singletons <- vapply(prepared$stages, function(stage) {
    any(stage$n == 1 & stage$N > 1)
  }, logical(1))
  if (any(singletons)) {
    affectedTables <- paste(prepared$tables[singletons], collapse = ", ")
    if (singleton == "fail") {
      stop(paste0("Non-census singleton sampling groups in ", affectedTables,
                  ": their variance cannot be estimated. Use singleton = ",
                  "\"zero\" only to explicitly accept zero contributions."),
           call. = FALSE)
    }
    warning(paste0("Non-census singleton sampling groups in ", affectedTables,
                   " use zero variance contributions. Uncertainty may be ",
                   "understated, including in bootstrap replicates."),
            call. = FALSE)
  }

  analytical <- calculateSrsworTotal(prepared$stages, prepared$response)
  estimates <- data.frame(method = "analytical",
                          est.total = unname(analytical["total"]),
                          var.total = unname(analytical["variance"]))
  replicates <- numeric()
  if (nBoot > 0) {
    replicates <- generateSrsworReplicates(prepared$stages, prepared$response,
                                         as.integer(nBoot))
    estimates <- rbind(estimates,
                      data.frame(method = "bootstrap",
                                 est.total = unname(analytical["total"]),
                                 var.total = stats::var(replicates)))
  }
  estimates$se.total <- sqrt(estimates$var.total)
  margin <- stats::qnorm((1 - confidenceLevel) / 2, lower.tail = FALSE) *
    estimates$se.total
  estimates$lower <- estimates$est.total - margin
  estimates$upper <- estimates$est.total + margin

  design <- do.call(rbind, prepared$stages)
  rownames(design) <- NULL
  list(estimates = estimates, replicates = replicates, design = design)
}
