#' Raising factors of the sampled units of one stage
#'
#' The engine behind stage-wise raising (see [raisingScheme()]). Works on a
#' plain table with one row per unit, so it can be used and tested without
#' RDBES data. For every group (parent x stratum, given by `by`) it computes
#' \eqn{factor = T / \sum_{i \in s} v_i}, where \eqn{T} is the total of the
#' group and the sum runs over its sampled units. With `method = "prob"` the
#' factor is \eqn{1 / v_i}, with `method = "none"` it is 1.
#'
#' Checks, each of which stops the run with the groups concerned:
#' * the total is missing, or differs between units of the same group (the
#'   chosen strata do not match how the totals were reported),
#' * a value of a sampled unit is missing,
#' * a group has a positive total but no sampled unit, or a sum of values
#'   of 0,
#' * an inclusion probability outside (0, 1].
#'
#' Logged: groups whose number of sampled units differs from `numSamp`,
#' and groups whose total is smaller than the sum of the values.
#'
#' @param units A `data.frame` with one row per unit.
#' @param id Name of the unit id column.
#' @param by Names of the grouping columns (parent id and stratum). `NULL`
#'   means one group.
#' @param total Name of the column with the group total \eqn{T}. Needed
#'   unless `method` is `"none"` or `"prob"`.
#' @param value Name of the column with the value \eqn{v}, or `NULL` for 1
#'   (counting units). For `method = "prob"`, the inclusion probability.
#' @param method `"ratio"` (default, covers count, aux and external), `"prob"`
#'   or `"none"`.
#' @param sampled Optional name of a logical column: `TRUE` for units that
#'   were sampled. Default: all units are sampled.
#' @param numSamp Optional name of a column with the reported number of
#'   sampled units of the group, compared with the observed number.
#' @param step Label used in the log. Default `"computeStageFactors"`.
#' @param verbose Logical. Print log entries as messages. Default `FALSE`.
#'
#' @return A `data.table` with one row per sampled unit: `id`, the `by`
#'   columns, `n` (sampled units in the group), `total`, `sumValue` and
#'   `factor`. The log is attached as attribute `"ratioLog"`.
#' @seealso [raisingScheme()], [getRatioLog()]
#' @export
#'
#' @examples
#' hauls <- data.frame(FOid = 1:5, FTid = c(1, 1, 1, 2, 2),
#'                     FOnumTotal = c(6, 6, 6, 4, 4))
#' computeStageFactors(hauls, id = "FOid", by = "FTid", total = "FOnumTotal")
computeStageFactors <- function(units, id, by = NULL, total = NULL,
                                value = NULL,
                                method = c("ratio", "prob", "none"),
                                sampled = NULL, numSamp = NULL,
                                step = "computeStageFactors",
                                verbose = FALSE) {
  method <- match.arg(method)
  log <- newRatioLog()
  u <- data.table::as.data.table(units)
  needed <- c(id, by, if (method == "ratio") total, value, sampled, numSamp)
  missingCols <- setdiff(needed, names(u))
  if (length(missingCols) > 0) {
    stop(step, ": column(s) not found: ", paste(missingCols, collapse = ", "),
         call. = FALSE)
  }
  if (anyDuplicated(u[[id]]) > 0) {
    stop(step, ": duplicated unit ids in '", id, "'.", call. = FALSE)
  }
  # Columns are taken out with [[ ]] so that argument names cannot clash with
  # column names inside data.table expressions
  sampVec <- if (is.null(sampled)) {
    rep(TRUE, nrow(u))
  } else {
    as.logical(u[[sampled]])
  }
  u[, .samp := sampVec]
  u[is.na(.samp), .samp := FALSE]
  vVec <- if (is.null(value)) rep(1, nrow(u)) else as.numeric(u[[value]])
  u[, .v := vVec]
  if (is.null(by)) {
    u[, .grp := 1L]
    by <- character(0)
  }
  grpCols <- if (length(by) > 0) by else ".grp"
  groupLabel <- function(d) {
    if (length(by) == 0) return("all units")
    apply(d[, by, with = FALSE], 1, function(r) {
      paste(paste0(by, "=", trimws(r)), collapse = ", ")
    })
  }

  s <- u[.samp == TRUE]
  if (nrow(s) == 0) stop(step, ": no sampled units.", call. = FALSE)

  if (method == "none") {
    s[, `:=`(n = .N, .T = NA_real_, sumValue = NA_real_, factor = 1),
      by = grpCols]
  }

  if (method == "prob") {
    bad <- s[is.na(.v) | .v <= 0 | .v > 1]
    if (nrow(bad) > 0) {
      stop(step, ": inclusion probability missing or outside (0, 1] for ",
           nrow(bad), " unit(s): ", paste(utils::head(bad[[id]], 10),
                                          collapse = ", "), call. = FALSE)
    }
    s[, `:=`(n = .N, .T = NA_real_, sumValue = NA_real_, factor = 1 / .v),
      by = grpCols]
  }

  if (method == "ratio") {
    tVec <- as.numeric(u[[total]])
    u[, .T := tVec]
    # The total must be one value per group
    tg <- u[, .(nT = data.table::uniqueN(.T), hasNA = anyNA(.T)),
            by = grpCols]
    if (any(tg$hasNA)) {
      stop(step, ": '", total, "' is missing in: ",
           paste(groupLabel(tg[hasNA == TRUE]), collapse = " | "),
           call. = FALSE)
    }
    if (any(tg$nT > 1)) {
      stop(step, ": '", total, "' differs within a group, so the totals are ",
           "not reported per these strata: ",
           paste(groupLabel(tg[nT > 1]), collapse = " | "), call. = FALSE)
    }
    badV <- s[is.na(.v)]
    if (nrow(badV) > 0) {
      stop(step, ": value '", value, "' is missing for ", nrow(badV),
           " sampled unit(s): ", paste(utils::head(badV[[id]], 10),
                                       collapse = ", "), call. = FALSE)
    }
    g <- u[, .(.T = .T[1], n = sum(.samp), sumValue = sum(.v[.samp])),
           by = grpCols]
    empty <- g[.T > 0 & (n == 0 | sumValue <= 0)]
    if (nrow(empty) > 0) {
      stop(step, ": positive total but no sampled units (or a sum of values ",
           "of 0) in: ", paste(groupLabel(empty), collapse = " | "),
           call. = FALSE)
    }
    small <- g[n > 0 & .T < sumValue]
    if (nrow(small) > 0) {
      log <- addToRatioLog(log, step, "warning",
        paste0("'", total, "' is smaller than the sum of the sampled ",
               "values: factor below 1"), nrow(small), verbose)
    }
    g[, factor := data.table::fifelse(sumValue > 0, .T / sumValue, 0)]
    s <- merge(s[, setdiff(names(s), c(".T")), with = FALSE],
               g, by = grpCols)
  }

  if (!is.null(numSamp)) {
    nsVec <- as.numeric(u[[numSamp]])
    u[, .ns := nsVec]
    ns <- u[.samp == TRUE, .(n = .N, reported = .ns[1]),
            by = grpCols]
    off <- ns[!is.na(reported) & reported != n]
    if (nrow(off) > 0) {
      log <- addToRatioLog(log, step, "info",
        paste0("Number of sampled units differs from '", numSamp,
               "': the observed number is used"), nrow(off), verbose)
    }
  }

  out <- s[, c(id, by, "n", ".T", "sumValue", "factor"), with = FALSE]
  data.table::setnames(out, ".T", "total")
  data.table::setorderv(out, c(by, id))
  attr(out, "ratioLog") <- log
  out[]
}
