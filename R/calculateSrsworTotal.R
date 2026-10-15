#' Calculate a Total and Variance for Validated SRSWOR Stages
#'
#' Expand estimated child totals through the sampling hierarchy, including
#' parent-specific finite population corrections and child variance estimates.
#'
#' @param stages A list of validated stage data frames, ordered from first to
#'   last. Each contains unique `key` values, `parent` keys, within-parent
#'   `stratum` labels, and the sample and population sizes `n` and `N`.
#' @param response A numeric vector aligned with the final stage. Missing
#'   responses are omitted from total contributions, retaining all design units.
#'
#' @return A named numeric vector containing `total` and `variance`.
#'
#' @details For a group of sampled children, the expanded total is
#'   \eqn{(N/n)\sum_i t_i}. Its variance estimate is
#'   \eqn{N^2(1-n/N)s_t^2/n + (N/n)\sum_i v_i}, where the child total and
#'   variance estimates are \eqn{t_i} and \eqn{v_i}. Independent strata within
#'   a parent are summed before proceeding to the preceding stage. Singleton
#'   groups contribute zero between-child variance. The caller must explicitly
#'   handle the scientific limitation of non-census singleton groups.
#'   Whenever a response is missing, the analytical variance is returned as
#'   `NA`. Omitting a total contribution does not establish the variance of the
#'   full population total or an assumption about the missing response.
#'
#' @references Preston, J. (2009). Rescaled bootstrap for stratified multistage
#'   sampling. Survey Methodology, 35(2), 227--234, Section 2.
#'   \url{https://www150.statcan.gc.ca/n1/pub/12-001-x/2009002/article/11044-eng.pdf}
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: Pending
#' - Notes/scope: Non-census singleton contributions require an explicit policy
#'   in the calling function.
#' @keywords internal
calculateSrsworTotal <- function(stages, response) {
  missingResponse <- anyNA(response)
  childTotals <- response
  childVariances <- numeric(length(response))

  for (r in rev(seq_along(stages))) {
    stage <- stages[[r]]
    groupId <- interaction(match(stage$parent, unique(stage$parent)),
                           match(stage$stratum, unique(stage$stratum)),
                           drop = TRUE)
    groups <- split(seq_len(nrow(stage)), groupId)
    groupTotals <- groupVariances <- numeric(length(groups))
    parents <- character(length(groups))

    for (g in seq_along(groups)) {
      rows <- groups[[g]]
      n <- stage$n[rows[1L]]
      N <- stage$N[rows[1L]]
      weight <- N / n
      groupTotals[g] <- weight * sum(childTotals[rows], na.rm = TRUE)
      betweenVariance <- if (n > 1 && N > n) {
        N^2 * (1 - n / N) * stats::var(childTotals[rows]) / n
      } else {
        0
      }
      groupVariances[g] <- betweenVariance +
        weight * sum(childVariances[rows])
      parents[g] <- as.character(stage$parent[rows[1L]])
    }

    byParent <- rowsum(cbind(total = groupTotals, variance = groupVariances),
                       group = parents, reorder = FALSE)
    if (r > 1L) {
      parentRows <- match(as.character(stages[[r - 1L]]$key),
                          rownames(byParent))
      childTotals <- byParent[parentRows, "total"]
      childVariances <- byParent[parentRows, "variance"]
    } else {
      result <- colSums(byParent)
      if (missingResponse) result["variance"] <- NA_real_
      return(result)
    }
  }
}

#' Generate Preston Rescaled Bootstrap Totals for Validated SRSWOR Stages
#'
#' @param stages A list of validated stage data frames as described in
#'   [calculateSrsworTotal()].
#' @param response A numeric vector aligned with the final stage. Missing
#'   responses are omitted from replicate totals, retaining all design units.
#' @param nReplicates The number of replicates to generate.
#'
#' @return A numeric vector of bootstrap total estimates.
#'
#' @details Within each parent and stratum, independently select
#'   \eqn{m_r=\lfloor n_r/2\rfloor} sampled units without replacement. Let
#'   \eqn{\delta_r} indicate selection, \eqn{q_r=n_r/m_r}, and
#'   \eqn{f_r=n_r/N_r}. For a terminal unit's sampling path, use
#'   \deqn{\lambda_r=\sqrt{m_r(\prod_{s<r}f_s)(1-f_r)/(n_r-m_r)}}
#'   and the combined replicate factor
#'   \deqn{A=1+\sum_r\lambda_r(q_r\delta_r-1)
#'     \prod_{s<r}(\sqrt{q_s}\delta_s).}
#'   Multiply the ordinary expansion weight by \eqn{A}. This directly computes
#'   the product of the stage weights in Preston's equations, avoiding division
#'   by intermediate adjusted weights. Singleton groups use a zero perturbation
#'   and a unit subsampling factor. Their sampling fraction is retained for
#'   descendant stages. The caller must handle non-census singleton groups.
#'   Omitted response contributions do not represent imputed zeros. Replicates
#'   do not account for nonresponse uncertainty or estimate missing responses.
#'
#' @references Preston, J. (2009). Rescaled bootstrap for stratified multistage
#'   sampling. Survey Methodology, 35(2), 227--234, Section 3.
#'   \url{https://www150.statcan.gc.ca/n1/pub/12-001-x/2009002/article/11044-eng.pdf}
#'
#'   Bruch, C. (2022). Applying the rescaling bootstrap under imputation for a
#'   multistage sampling design. Computational Statistics, 37, 1461--1494,
#'   equations (1)--(6).
#'   \doi{10.1007/s00180-021-01164-6}
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: Pending
#' - Notes/scope: SRSWOR and census stages only. Omission of missing response
#'   contributions does not account for nonresponse uncertainty.
#' @keywords internal
generateSrsworReplicates <- function(stages, response, nReplicates) {
  # Prepare deterministic path quantities once. Numeric grouping codes avoid
  # collisions when user identifiers contain punctuation used by interaction().
  preparedStages <- vector("list", length(stages))
  cumulativeFraction <- expansionWeight <- 1
  for (r in seq_along(stages)) {
    stage <- stages[[r]]
    parentRows <- if (r > 1L) {
      match(stage$parent, stages[[r - 1L]]$key)
    } else {
      rep(1L, nrow(stage))
    }
    fraction <- stage$n / stage$N
    subsampleSize <- pmax(1, floor(stage$n / 2))
    lambda <- numeric(nrow(stage))
    nonsingleton <- stage$n > 1
    lambda[nonsingleton] <- sqrt(
      subsampleSize[nonsingleton] *
        cumulativeFraction[parentRows[nonsingleton]] *
        (1 - fraction[nonsingleton]) /
        (stage$n[nonsingleton] - subsampleSize[nonsingleton])
    )
    groupId <- interaction(match(stage$parent, unique(stage$parent)),
                           match(stage$stratum, unique(stage$stratum)),
                           drop = TRUE)
    preparedStages[[r]] <- list(
      parentRows = parentRows,
      groups = split(seq_len(nrow(stage)), groupId),
      subsampleSize = subsampleSize,
      q = stage$n / subsampleSize,
      lambda = lambda
    )
    cumulativeFraction <- cumulativeFraction[parentRows] * fraction
    expansionWeight <- expansionWeight[parentRows] / fraction
  }

  totals <- numeric(nReplicates)
  for (b in seq_len(nReplicates)) {
    combinedFactor <- ancestorFactor <- 1
    for (r in seq_along(preparedStages)) {
      stage <- preparedStages[[r]]
      delta <- numeric(length(stage$q))
      for (rows in stage$groups) {
        m <- stage$subsampleSize[rows[1L]]
        selected <- if (length(rows) == 1L) {
          rows
        } else {
          rows[sample.int(length(rows), m)]
        }
        delta[selected] <- 1
      }
      combinedFactor <- combinedFactor[stage$parentRows] +
        stage$lambda * (stage$q * delta - 1) *
        ancestorFactor[stage$parentRows]
      ancestorFactor <- ancestorFactor[stage$parentRows] *
        sqrt(stage$q) * delta
    }
    totals[b] <- sum(response * expansionWeight * combinedFactor, na.rm = TRUE)
  }
  totals
}
