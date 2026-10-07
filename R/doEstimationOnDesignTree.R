#' Design-based estimation on a design tree
#'
#' Estimates totals and their variances for every sampling unit and stratum
#' of a design tree by folding the leaf values up the tree, from the lowest
#' sampling stage to the top. For each stratum h of the child units of a
#' parent unit the estimated total is the generalised Horvitz-Thompson
#' (multiple count) estimator \eqn{\hat{t}_h = \sum_k w_k \hat{t}_k} with
#' \eqn{w_k = 1/E(n_k)}, or, for ratio stages,
#' \eqn{\hat{t}_h = X_h \sum_k w_k \hat{t}_k / \sum_k w_k x_k}.
#' The total of a parent is the sum over its child strata.
#'
#' The variance of a stratum estimate is the variance between the sampled
#' units plus the contribution of the lower stages,
#' \eqn{\hat{V}_h = \hat{V}_{between} + \sum_k c_k \hat{V}_k}, where
#' \eqn{c_k = w_k} for designs without replacement and \eqn{c_k = 0} for
#' designs with replacement (Sarndal et al. 1992, Results 4.3.1 and 4.5.1).
#' The between variance is
#' \eqn{fpc \cdot n/(n-1) \sum_k (w_k u_k - \sum_l w_l u_l / n)^2} with
#' \eqn{u_k = \hat{t}_k} (\eqn{fpc = 1 - n/N} for SRSWOR, 1 for SRSWR and
#' UPSWR, 0 for CENSUS), which equals the Sen-Yates-Grundy and
#' Hansen-Hurwitz estimators used in [estimMC()]. For ratio stages
#' \eqn{u_k = \hat{t}_k - \hat{R} x_k} and the variance is multiplied by
#' \eqn{(X_h / \sum_k w_k x_k)^2} (linearisation). The variance is NA when it
#' cannot be estimated, e.g. with one sampled unit in a stratum of a without
#' replacement design, or UPSWOR without second order inclusion
#' probabilities (see `varianceAsWR` in [createDesignTree()]).
#'
#' Estimates are stored sparsely: a unit without a row for a variable has an
#' estimated total of zero and a variance of zero.
#'
#' @param designTree A design tree from [createDesignTree()].
#' @param leafValues A data.table with the columns `node`, `var`, `t` and `v`
#' giving the values (`t`) of each variable (`var`) for the lowest units, and
#' their variance (`v`, normally 0 for measured values). See
#' [getLeafValuesSA()], [getLeafValuesBV()] and [getLeafValuesFM()].
#' @param zeroIfNoChildren Optional tables whose units without any child
#' units have a total of zero, e.g. "SS" when a species was looked for but
#' not found and no SA row was recorded. By default (NULL) such units stop
#' the estimation, because a unit without data below it is missing, not
#' zero. Prefer adding the zeros to the data, e.g. with
#' [generateZerosUsingSL()].
#'
#' @return A list with
#' \describe{
#'   \item{nodes}{estimated total `t`, variance `v` and standard error `se`
#'   for every unit (`node`) and variable}
#'   \item{strata}{the estimates for every parent unit and child stratum:
#'   `n_g` sampled units, `t`, the between (`vb`) and within (`vw`)
#'   variance components, `v` and `se`}
#'   \item{tree}{the design tree}
#'   \item{zeroIfNoChildren}{the argument of the same name}
#' }
#' @export
#'
#' @references Sarndal, C.-E., Swensson, B. and Wretman, J. (1992).
#' Model Assisted Survey Sampling. Springer-Verlag.
#'
#' @examples
#' tree <- createDesignTree(H8ExampleEE1, "BV", bvAssess = "Age",
#'                          ratio = c(SA = "weight"),
#'                          strictSampleSize = FALSE)
#' leaves <- getLeafValuesBV(tree, H8ExampleEE1, classMeas = "Age")
#' res <- doEstimationOnDesignTree(tree, leaves)
#' res$nodes[node == "ROOT"]
doEstimationOnDesignTree <- function(designTree, leafValues,
                                     zeroIfNoChildren = NULL) {

  needed <- c("node", "parent", "table", "stratum", "depth", "w", "cw", "fpc",
              "x", "X", "ratio")
  if (!all(needed %in% names(designTree))) {
    stop("designTree must be created with createDesignTree()")
  }
  # work on a copy so that the caller's tree is not changed (e.g. indices)
  tree <- copy(designTree)
  if (!all(c("node", "var", "t", "v") %in% names(leafValues))) {
    stop("leafValues must have the columns node, var, t and v")
  }
  leaves <- as.data.table(leafValues)[, .(node, var, t, v)]
  if (anyNA(leaves$t)) {
    stop("NA leaf values - decide how to treat them before estimation")
  }
  if (anyDuplicated(leaves[, .(node, var)])) {
    stop("More than one leaf value per unit and variable")
  }
  missingNodes <- setdiff(unique(leaves$node), tree$node)
  if (length(missingNodes) > 0) {
    stop("Leaf units not in the design tree, e.g. ", missingNodes[1])
  }

  leaves[, leafTable := tree$table[match(leaves$node, tree$node)]]
  leafTables <- unique(leaves[, .(var, leafTable)])
  leaves[, leafTable := NULL]
  if (anyDuplicated(leafTables$var)) {
    stop("A variable has leaf values in more than one table")
  }

  # Tables each variable passes through on its way up. Every unit of these
  # tables must lead down to at least one unit of the leaf table, otherwise
  # its value is missing, not zero (create zeros explicitly, e.g. with
  # generateZerosUsingSL()).
  unknownZero <- setdiff(zeroIfNoChildren, unique(tree$table))
  if (length(unknownZero) > 0) {
    stop("zeroIfNoChildren tables not in the tree: ",
         paste(unknownZero, collapse = ", "))
  }
  zeroLeaves <- tree[table %in% zeroIfNoChildren & !node %in% tree$parent, node]
  varTables <- list()
  for (lt in unique(leafTables$leafTable)) {
    reach <- designTreeAncestors(tree, c(tree[table == lt, node], zeroLeaves))
    upTables <- setdiff(unique(tree[node %in% reach, table]), lt)
    deadNodes <- tree[table %in% upTables & !node %in% reach, node]
    if (length(deadNodes) > 0) {
      stop(length(deadNodes), " units have no child units down to table ", lt,
           ", e.g. ", deadNodes[1])
    }
    # leaf values must sit on the deepest unit of a sub-sampling chain;
    # children in lower tables (e.g. BV under SA for an SA variable) are
    # irrelevant for the variable
    subSampled <- intersect(leaves[var %in% leafTables[leafTable == lt, var],
                                   node],
                            tree[table == lt, parent])
    if (length(subSampled) > 0) {
      stop("Leaf values given for units that are sub-sampled further, e.g. ",
           subSampled[1])
    }
    for (v in leafTables[leafTable == lt, var]) varTables[[v]] <- c(upTables, lt)
  }

  # design sums per stratum over ALL its units (zeros are implicit)
  groups <- tree[, .(n_g = .N, fpc = fpc[1], ratio = ratio[1], X = X[1],
                     Swx = sum(w * x), Swx2 = sum((w * x)^2)),
                 by = .(parent, table, stratum, depth)]

  est <- copy(leaves)
  strataOut <- list()
  for (d in max(tree$depth):1) {
    kids <- tree[depth == d, .(node, parent, table, stratum, w, cw, x)]
    e <- est[kids, on = "node", nomatch = NULL]
    if (nrow(e) == 0) next
    s <- e[, .(Swt = sum(w * t), Swt2 = sum((w * t)^2),
               Swtwx = sum((w * t) * (w * x)),
               # with replacement stages (cw = 0) do not need the lower
               # stage variances, which may be NA
               within = sum(fifelse(cw == 0, 0, cw * v))),
           by = .(parent, table, stratum, var)]
    s <- groups[depth == d][s, on = .(parent, table, stratum)]
    s[, `:=`(t = Swt, ssq = Swt2 - Swt^2 / n_g, g = 1)]
    s[ratio == TRUE, `:=`(R = Swt / Swx, g = X / Swx)]
    s[ratio == TRUE, `:=`(t = X * R, ssq = Swt2 - 2 * R * Swtwx + R^2 * Swx2)]
    # ssq is a sum of squares: negative values within rounding error are 0
    s[, ssqScale := Swt2]
    s[ratio == TRUE, ssqScale := pmax(Swt2, R^2 * Swx2)]
    if (any(s$ssq < -1e-8 * s$ssqScale, na.rm = TRUE)) {
      stop("Negative sum of squares in the variance (numerical problem)")
    }
    s[ssq < 0, ssq := 0]
    s[, vb := fifelse(fpc == 0, 0, fpc * n_g / (n_g - 1) * ssq)]
    s[n_g < 2 & (is.na(fpc) | fpc != 0), vb := NA_real_]
    s[, `:=`(vb = g^2 * vb, vw = g^2 * within)]
    s[, v := vb + vw]
    strataOut[[as.character(d)]] <- s[, .(parent, table, stratum, depth, var,
                                          n_g, ratio, t, vb, vw, v)]
    est <- rbind(est, s[, .(t = sum(t), v = sum(v)), by = .(node = parent, var)])
  }

  # Whether a variance can be estimated depends on the design only, not on
  # the values: a stratum with a single unit (without replacement) has no
  # variance estimate even if its value happens to be zero and is therefore
  # absent from the sparse estimates.
  okStrata <- list()
  okNodes <- list()
  for (lt in unique(leafTables$leafTable)) {
    relevant <- varTables[[leafTables[leafTable == lt, var][1]]]
    ok <- designTreeVarianceEstimable(tree, groups, relevant)
    for (v in leafTables[leafTable == lt, var]) {
      okStrata[[v]] <- ok$strata[, .(parent, table, stratum, var = v,
                                     okBetween, okWithin)]
      okNodes[[v]] <- ok$nodes[, .(node, var = v, okNode)]
    }
  }
  okStrata <- rbindlist(okStrata)
  okNodes <- rbindlist(okNodes)

  # complete the strata estimates with explicit zeros
  strata <- rbindlist(c(list(data.table(
    parent = character(0), table = character(0), stratum = character(0),
    depth = integer(0), var = character(0), n_g = integer(0),
    ratio = logical(0), t = numeric(0), vb = numeric(0), vw = numeric(0),
    v = numeric(0))), strataOut))
  allStrata <- rbindlist(lapply(names(varTables), function(v) {
    groups[table %in% varTables[[v]], .(parent, table, stratum, depth,
                                        var = v, n_g, ratio)]
  }))
  if (nrow(allStrata) > 0) {
    strata <- strata[allStrata, on = names(allStrata)]
    strata[is.na(t), `:=`(t = 0, vb = 0, vw = 0, v = 0)]
    strata <- okStrata[strata, on = .(parent, table, stratum, var)]
    strata[okBetween == FALSE, vb := NA_real_]
    strata[okWithin == FALSE, vw := NA_real_]
    strata[, v := vb + vw]
    strata[, c("okBetween", "okWithin") := NULL]
  }
  strata[, se := sqrt(v)]
  setcolorder(strata, c("parent", "table", "stratum", "depth", "var", "n_g",
                        "ratio", "t", "vb", "vw", "v", "se"))
  setorder(strata, depth, parent, table, stratum, var)

  nodes <- tree[, .(node, table, stratum, depth)][est, on = "node"]
  nodes[node == "ROOT", `:=`(table = "ROOT", depth = 0L)]
  if (nrow(okNodes) > 0) {
    nodes <- okNodes[nodes, on = .(node, var)]
    nodes[okNode == FALSE, v := NA_real_]
    nodes[, okNode := NULL]
    setcolorder(nodes, c("node", "table", "stratum", "depth", "var", "t", "v"))
  }
  nodes[, se := sqrt(v)]
  list(nodes = nodes[], strata = strata[], tree = tree,
       zeroIfNoChildren = zeroIfNoChildren)
}


#' Can the variances be estimated, from the design alone (internal)
#'
#' A stratum has an estimable between-unit variance when its fpc is 0
#' (census) or it has at least 2 units and a known fpc. Its within-unit
#' variance is estimable when all its units that contribute one (cw > 0)
#' are estimable. A unit is estimable when all its child strata are; units
#' without children are.
#'
#' @param tree The design tree.
#' @param groups The strata of the tree (parent, table, stratum, depth, n_g,
#'   fpc).
#' @param relevantTables The tables a variable passes through.
#' @return list(strata = okBetween and okWithin per stratum,
#'   nodes = okNode per unit)
#' @noRd
designTreeVarianceEstimable <- function(tree, groups, relevantTables) {
  okNode <- stats::setNames(rep(TRUE, nrow(tree) + 1), c(tree$node, "ROOT"))
  out <- list()
  kidsAll <- tree[table %in% relevantTables]
  for (d in sort(unique(kidsAll$depth), decreasing = TRUE)) {
    kids <- kidsAll[depth == d]
    g <- kids[, .(okWithin = all(cw == 0 | okNode[node])),
              by = .(parent, table, stratum)]
    g <- groups[depth == d, .(parent, table, stratum, n_g, fpc)][
      g, on = .(parent, table, stratum)]
    g[, okBetween := !is.na(fpc) & (fpc == 0 | n_g >= 2)]
    up <- g[, .(ok = all(okBetween & okWithin)), by = parent]
    okNode[up$parent] <- okNode[up$parent] & up$ok
    out[[as.character(d)]] <- g[, .(parent, table, stratum, okBetween, okWithin)]
  }
  list(strata = rbindlist(out),
       nodes = data.table(node = names(okNode), okNode = unname(okNode)))
}


#' All ancestors of a set of units, the units included (internal)
#' @param tree The design tree.
#' @param nodes The units.
#' @return character vector of units
#' @noRd
designTreeAncestors <- function(tree, nodes) {
  parentOf <- stats::setNames(tree$parent, tree$node)
  out <- nodes
  current <- nodes
  repeat {
    current <- unique(parentOf[current])
    current <- current[!is.na(current) & current != "ROOT"]
    if (length(current) == 0) break
    out <- c(out, current)
  }
  unique(out)
}


#' Get a field of an ancestor table for units of a design tree
#'
#' Useful to define domains for units of the lower tables, e.g. the area
#' (`LEarea`) or month (`TEstratumName`) of each fish.
#'
#' @param designTree A design tree from [createDesignTree()].
#' @param nodes The units (values of `designTree$node`).
#' @param RDBESDataObject The RDBESDataObject the tree was made from.
#' @param field The field, e.g. "TEstratumName". Its table is given by the
#' two letter prefix and must be an ancestor of all `nodes`.
#'
#' @return A vector with the value of `field` for each unit.
#' @export
#'
#' @examples
#' tree <- createDesignTree(H8ExampleEE1, "SA", strictSampleSize = FALSE)
#' saNodes <- tree[table == "SA", node]
#' getAncestorValue(tree, saNodes, H8ExampleEE1, "TEstratumName")
getAncestorValue <- function(designTree, nodes, RDBESDataObject, field) {
  tableName <- substr(field, 1, 2)
  src <- RDBESDataObject[[tableName]]
  if (is.null(src) || !field %in% names(src)) {
    stop("Field ", field, " not found in table ", tableName)
  }
  parentOf <- stats::setNames(designTree$parent, designTree$node)
  tableOf <- stats::setNames(designTree$table, designTree$node)
  current <- nodes
  for (i in seq_len(max(designTree$depth))) {
    notYet <- !is.na(current) & tableOf[current] != tableName
    if (!any(notYet, na.rm = TRUE)) break
    current[which(notYet)] <- parentOf[current[which(notYet)]]
  }
  if (anyNA(current) || any(tableOf[current] != tableName)) {
    stop("Table ", tableName, " is not an ancestor of all units")
  }
  ids <- sub("^[A-Z]{2}:", "", current)
  src[[field]][match(ids, as.character(src[[paste0(tableName, "id")]]))]
}
