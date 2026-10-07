#' Ratio estimation to known totals
#'
#' Combined ratio estimator for totals known from outside the sampling
#' design, e.g. landings from the CL table. Within each domain d
#' \deqn{\hat{t}_{y,d} = X_d \hat{t}_{y,d} / \hat{t}_{x,d}}
#' where \eqn{\hat{t}_{y,d}} and \eqn{\hat{t}_{x,d}} are design-based
#' estimates from the same sample and \eqn{X_d} is the known total. The
#' variance is estimated by linearisation: the residuals
#' \eqn{e = y - \hat{R}_d x} are formed for the units where `xVar` is
#' measured and estimated through the same design, and
#' \eqn{\hat{V} = (X_d / \hat{t}_{x,d})^2 \hat{V}(\hat{t}_e)}.
#'
#' With `X = 1` the result is the estimated ratio \eqn{\hat{R}_d} itself and
#' its standard error, e.g. a mean per unit when `xVar` counts the units.
#'
#' @param estimates The output of [doEstimationOnDesignTree()]; it must
#' contain the variables `yVars` and `xVar`.
#' @param yVars Names of the variables to estimate, e.g. "N|Age=3".
#' @param xVar Name of the auxiliary variable, e.g. "SAsampWtMes". It must
#' have no variance where it is measured.
#' @param X Named numeric vector of the known totals per domain, in the
#' units of `xVar`. If `domainOf` is NULL a single value.
#' @param domainOf NULL for a single domain, or a function returning the
#' domain of each unit where `xVar` is measured (a character vector the
#' length of its argument `nodes`), e.g. built with [getAncestorValue()].
#'
#' @return A data.table with, for each variable and domain, the known total
#' `X`, the design-based estimates `t_y` and `t_x`, the ratio `R`, the ratio
#' estimate `est`, its variance `var_est` and standard error `se`.
#' @export
#'
#' @examples
#' # sprat numbers at age in Q1 raised to the Q1 landings in CL
#' x <- H8ExampleEE1
#' q1 <- c("January", "February", "March")
#' tree <- createDesignTree(x, "BV", bvAssess = "Age",
#'                          ratio = c(SA = "weight"),
#'                          strictSampleSize = FALSE, varianceAsWR = "TE")
#' leaves <- rbind(getLeafValuesBV(tree, x, classMeas = "Age"),
#'                 getLeafValuesSA(tree, x, "SAsampWtMes"))
#' res <- doEstimationOnDesignTree(tree, leaves)
#' quarter <- function(nodes) {
#'   ifelse(getAncestorValue(tree, nodes, x, "TEstratumName") %in% q1,
#'          "Q1", "Q4")
#' }
#' landings <- x$CL[CLmetier6 == "OTM_SPF_16-31_0_0" & CLquar %in% c(1, 4),
#'                  .(X = sum(CLoffWeight) * 1000), by = CLquar]
#' doRatioEstimationToTotal(res, c("N|Age=2", "N|Age=3"), "SAsampWtMes",
#'                          X = c(Q1 = landings[CLquar == 1, X],
#'                                Q4 = landings[CLquar == 4, X]),
#'                          domainOf = quarter)
doRatioEstimationToTotal <- function(estimates, yVars, xVar, X,
                                     domainOf = NULL) {

  if (is.null(estimates$tree) || is.null(estimates$nodes)) {
    stop("estimates must be the output of doEstimationOnDesignTree()")
  }
  tree <- copy(estimates$tree)
  nodes <- copy(estimates$nodes)
  missingVars <- setdiff(c(yVars, xVar), nodes$var)
  if (length(missingVars) > 0) {
    stop("Variables not in the estimates: ", paste(missingVars, collapse = ", "))
  }

  # the units where x is measured, and the design above them
  xNodes <- nodes[var == xVar]
  xTable <- xNodes[which.max(depth), table]
  injectNodes <- tree[table == xTable &
                        !node %in% tree[table == xTable, parent], node]
  below <- character(0)
  current <- injectNodes
  repeat {
    current <- tree[parent %in% current, node]
    if (length(current) == 0) break
    below <- c(below, current)
  }
  treeUpper <- tree[!node %in% below]
  zero <- estimates$zeroIfNoChildren

  if (is.null(domainOf)) {
    if (length(X) != 1) stop("X must be a single value when domainOf is NULL")
    domain <- rep("all", length(injectNodes))
    X <- c(all = unname(X))
  } else {
    domain <- as.character(domainOf(injectNodes))
    if (length(domain) != length(injectNodes) || anyNA(domain)) {
      stop("domainOf must return a domain for every unit")
    }
    if (!all(unique(domain) %in% names(X))) {
      stop("No known total X for the domains: ",
           paste(setdiff(unique(domain), names(X)), collapse = ", "))
    }
  }

  valuesAt <- function(variable) {
    r <- nodes[var == variable]
    i <- match(injectNodes, r$node)
    data.table(node = injectNodes, t = fifelse(is.na(i), 0, r$t[i]),
               v = fifelse(is.na(i), 0, r$v[i]))
  }
  xv <- valuesAt(xVar)
  if (any(xv$v != 0, na.rm = TRUE) || anyNA(xv$v)) {
    stop(xVar, " has a variance where it is measured: the covariance with ",
         "the y variables would be needed")
  }
  label <- function(variable) paste0(variable, "#", domain)

  # pass 1: domain totals of y and x
  pass1 <- unique(rbindlist(c(
    list(xv[, .(node, var = label(xVar), t, v)]),
    lapply(yVars, function(y) valuesAt(y)[, .(node, var = label(y), t, v)]))))
  # keep units with an NA variance: dropping them would set it to zero
  keep <- function(d) d[t != 0 | is.na(v) | v != 0]
  est1 <- doEstimationOnDesignTree(treeUpper, keep(pass1),
                                   zeroIfNoChildren = zero)
  root <- est1$nodes[node == "ROOT"]
  total <- function(variable) {
    r <- root$t[match(variable, root$var)]
    fifelse(is.na(r), 0, r)
  }
  zeroX <- unique(domain)[total(paste0(xVar, "#", unique(domain))) == 0]
  if (length(zeroX) > 0) {
    stop("The estimated total of ", xVar, " is zero in domains: ",
         paste(zeroX, collapse = ", "))
  }

  # pass 2: linearised residuals e = y - R x
  out <- list()
  for (y in yVars) {
    yv <- valuesAt(y)
    R <- total(label(y)) / total(label(xVar))
    resid <- data.table(node = injectNodes, var = paste0("e:", label(y)),
                        t = yv$t - R * xv$t, v = yv$v)
    est2 <- doEstimationOnDesignTree(treeUpper, keep(resid),
                                     zeroIfNoChildren = zero)
    for (d in unique(domain)) {
      tx <- total(paste0(xVar, "#", d))
      ty <- total(paste0(y, "#", d))
      ve <- est2$nodes[node == "ROOT" & var == paste0("e:", y, "#", d), v]
      if (length(ve) == 0) ve <- 0
      out[[paste(y, d)]] <- data.table(var = y, domain = d, X = X[[d]],
                                       t_y = ty, t_x = tx, R = ty / tx,
                                       est = X[[d]] * ty / tx,
                                       var_est = (X[[d]] / tx)^2 * ve)
    }
  }
  res <- rbindlist(out)
  res[, se := sqrt(var_est)]
  res[]
}
