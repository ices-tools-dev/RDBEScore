# Prototype: recursive (bottom-up) design-based estimation on an
# RDBESDataObject, with optional ratio estimators at any sampling stage.
#
# Idea (cf. issue #15 and "A Generalized Horvitz-Thompson Estimator v3",
# section 5):
#   t_hat(unit) = sum over child strata h of  stage_h( t_hat(children in h) )
# where stage_h is the generalised HT (multiple count) estimator or a ratio
# estimator, and everything stage_h needs (selection method, numTotal,
# numSamp, selProb, incProb, auxiliary values) is stored on the CHILD rows.
#
# Instead of writing a recursive function over table names, every table is
# flattened into ONE tree of sampling units (node, parent, design). The
# recursion is then a loop over tree depth, deepest first. SA sub-sampling,
# BV under FM (LH A) or under SA (LH C), optional tables etc. all become
# "a node has a parent" and need no special cases in the estimator.
#
# Each node carries, for every target variable, (t, v):
#   t = estimated total of the variable for the population the unit
#       represents, v = estimated variance of t.
# Variance recursion (Sarndal et al. 1992, Result 4.3.1 / 4.5.1):
#   V(parent stratum) = V_between(t_k) + sum_k c_k * V_k
#   c_k = 1/pi_k for WOR designs (and CENSUS), 0 for WR designs.
# Ratio stage (linearisation):
#   t_R = X * sum(w t) / sum(w x),   e_k = t_k - R x_k,
#   V(t_R) = g^2 * [ V_between(e_k) + sum_k c_k V_k ],   g = X / sum(w x)
#
# Only sums of w*t, (w*t)^2, (w*t)*(w*x) are needed, so variables can be
# stored sparsely (long format, absent = 0) as long as the number of
# sampled units per stratum comes from the design tree.

library(data.table)

implementedMethods <- c("CENSUS", "SRSWOR", "SRSWR", "UPSWOR", "UPSWR")

#' Build the sampling-unit tree of an RDBESDataObject
#'
#' @param rdbes RDBESDataObject with one upper hierarchy
#' @param lowestTable lowest table to include: "SA", "FM" or "BV"
#' @param bvAssess BVtypeAssess whose selection design defines the BV units
#'   (fish); required if lowestTable = "BV"
#' @param ratio named character vector; names = tables whose stage uses a
#'   ratio estimator, values = "weight" (SA only: x = SAsampWtMes,
#'   X = SAtotalWtMes) or "aux" (x = <T>auxVarValue, X = <T>auxVarTot)
#' @param methodMap optional named character vector mapping selection
#'   methods that are not implemented (e.g. "NPEJ") to an implemented one.
#'   This is an analyst decision and is recorded in the output.
#' @param nonResponse "stop" (default) or "respondents": drop units with
#'   <T>samp == "N" and treat the respondents as the sample (n := number of
#'   responding units). The latter assumes missing at random within stratum.
#' @param strictSampleSize if TRUE (default) stop when the number of sampled
#'   rows in a stratum differs from <T>numSamp; if FALSE only warn
#' @param varianceAsWR tables whose stage variance is computed as if the
#'   units had been selected with replacement ("ultimate cluster"): the fpc
#'   is dropped and the variances of the lower stages are not needed. Point
#'   estimates are unchanged. Conservative (over-estimates) for WOR stages.
#' @return data.table with one row per sampling unit
designTree <- function(rdbes, lowestTable = "SA", bvAssess = NULL,
                       ratio = character(0), methodMap = NULL,
                       nonResponse = "stop", strictSampleSize = TRUE,
                       varianceAsWR = character(0)) {

  hier <- unique(rdbes$DE$DEhierarchy)
  if (length(hier) != 1) stop("Exactly one DEhierarchy required, found: ",
                              paste(hier, collapse = ", "))
  upper <- RDBEScore::getTablesInRDBESHierarchy(
    hier, includeOptTables = FALSE, includeLowHierTables = FALSE,
    includeTablesNotInSampHier = FALSE)
  lower <- switch(lowestTable, SA = character(0), FM = "FM", BV = c("FM", "BV"),
                  stop("lowestTable must be SA, FM or BV"))
  if ("BV" %in% lower && is.null(bvAssess))
    stop("bvAssess must be given when lowestTable = 'BV'")
  badRatio <- setdiff(names(ratio), c(upper, "SA", "BV"))
  if (length(badRatio) > 0) stop("Ratio requested for unknown tables: ",
                                 paste(badRatio, collapse = ", "))
  if (any(ratio[names(ratio) != "SA"] == "weight"))
    stop("ratio = 'weight' is only defined for SA")

  num <- function(x) as.numeric(as.character(x))
  col <- function(tbl, name) {
    if (name %in% names(tbl)) tbl[[name]] else rep(NA, nrow(tbl))
  }

  unitRows <- function(tbl, tb, parentNode) {
    data.table(
      node = paste0(tb, ":", tbl[[paste0(tb, "id")]]),
      parent = parentNode,
      table = tb,
      stratum = as.character(col(tbl, paste0(tb, "stratumName"))),
      method = as.character(col(tbl, paste0(tb, "selectMeth"))),
      N = num(col(tbl, paste0(tb, "numTotal"))),
      n = num(col(tbl, paste0(tb, "numSamp"))),
      selProb = num(col(tbl, paste0(tb, "selProb"))),
      incProb = num(col(tbl, paste0(tb, "incProb"))),
      samp = as.character(col(tbl, paste0(tb, "samp"))),
      clustering = as.character(col(tbl, paste0(tb, "clustering"))),
      x = NA_real_, X = NA_real_, ratio = FALSE
    )
  }

  addRatio <- function(u, tbl, tb) {
    if (!tb %in% names(ratio)) return(u)
    if (ratio[[tb]] == "weight") {
      u[, `:=`(x = num(tbl$SAsampWtMes), X = num(tbl$SAtotalWtMes))]
    } else {
      u[, `:=`(x = num(col(tbl, paste0(tb, "auxVarValue"))),
               X = num(col(tbl, paste0(tb, "auxVarTot"))))]
    }
    u[, ratio := TRUE]
    u
  }

  units <- list()

  # DE and SD are not sampled: they act as census pseudo-stages
  de <- rdbes$DE
  units$DE <- data.table(node = paste0("DE:", de$DEid), parent = "ROOT",
                         table = "DE",
                         stratum = paste0(de$DEyear, "-", de$DEstratumName),
                         method = "CENSUS")
  sd <- rdbes$SD
  units$SD <- data.table(node = paste0("SD:", sd$SDid),
                         parent = paste0("DE:", sd$DEid), table = "SD",
                         stratum = paste0(sd$SDctry, "-", sd$SDinst),
                         method = "CENSUS")

  suTables <- setdiff(upper, c("DE", "SD", "SA"))
  prev <- "SD"
  for (tb in suTables) {
    tbl <- rdbes[[tb]]
    if (is.null(tbl) || nrow(tbl) == 0) stop("Table ", tb, " is empty")
    u <- unitRows(tbl, tb, paste0(prev, ":", tbl[[paste0(prev, "id")]]))
    units[[tb]] <- addRatio(u, tbl, tb)
    prev <- tb
  }

  # SA: parent is SS, or another SA when sub-sampled
  sa <- rdbes$SA
  saParent <- paste0("SS:", sa$SSid)
  if (any(!is.na(sa$SAparSequNum))) {
    key <- sa[, .(SSid, SAseqNum, parSAid = SAid)]
    m <- key[sa[, .(SSid, SAparSequNum)], on = .(SSid, SAseqNum = SAparSequNum),
             mult = "all"]
    if (nrow(m) != nrow(sa))
      stop("SAparSequNum does not identify a unique parent SA within SSid")
    isSub <- !is.na(sa$SAparSequNum)
    if (any(isSub & is.na(m$parSAid)))
      stop("Sub-sample SA rows without a parent SA: ",
           paste(sa$SAid[isSub & is.na(m$parSAid)], collapse = ", "))
    saParent[isSub] <- paste0("SA:", m$parSAid[isSub])
  }
  units$SA <- addRatio(unitRows(sa, "SA", saParent), sa, "SA")

  if ("FM" %in% lower && !is.null(rdbes$FM) && nrow(rdbes$FM) > 0) {
    fm <- rdbes$FM
    # FM is a complete tally of the SA sample: each class is its own
    # census stratum
    units$FM <- data.table(node = paste0("FM:", fm$FMid),
                           parent = paste0("SA:", fm$SAid), table = "FM",
                           stratum = paste0("FM:", fm$FMid), method = "CENSUS")
  }

  if ("BV" %in% lower) {
    bv <- rdbes$BV[BVtypeAssess == bvAssess]
    if (nrow(bv) == 0) stop("No BV rows with BVtypeAssess == ", bvAssess)
    bvParent <- ifelse(is.na(bv$FMid), paste0("SA:", bv$SAid),
                       paste0("FM:", bv$FMid))
    bv[, parentNode := bvParent]
    # one unit per fish and parent; its design must be unique
    fish <- unique(bv[, .(parentNode, BVfishId, BVstratumName, BVselectMeth,
                          BVnumTotal, BVnumSamp, BVselProb, BVincProb)])
    dup <- fish[, .N, by = .(parentNode, BVfishId)][N > 1]
    if (nrow(dup) > 0)
      stop("Fish with conflicting BV design rows for assessment ", bvAssess,
           ", e.g. BVfishId ", dup$BVfishId[1])
    fish[, BVid := paste0(parentNode, "/", BVfishId)]
    u <-data.table(node = paste0("BV:", fish$BVid), parent = fish$parentNode,
                    table = "BV", stratum = as.character(fish$BVstratumName),
                    method = as.character(fish$BVselectMeth),
                    N = num(fish$BVnumTotal), n = num(fish$BVnumSamp),
                    selProb = num(fish$BVselProb), incProb = num(fish$BVincProb),
                    samp = "Y", clustering = "N",
                    x = NA_real_, X = NA_real_, ratio = FALSE,
                    BVfishId = fish$BVfishId)
    units$BV <- u
  }

  tree <- rbindlist(units, fill = TRUE)
  if (anyDuplicated(tree$node)) stop("Duplicated node ids in the tree")
  tree[is.na(ratio), ratio := FALSE]

  # ---- selection method checks ------------------------------------------------
  tree[, methodOriginal := method]
  if (!is.null(methodMap)) {
    hit <- tree$method %in% names(methodMap)
    tree[hit, method := methodMap[method]]
  }
  bad <- tree[!method %in% implementedMethods, .N, by = .(table, method)]
  if (nrow(bad) > 0)
    stop("Selection methods without an implemented estimator (use methodMap ",
         "only if you accept the assumption):\n",
         paste(capture.output(print(bad)), collapse = "\n"))

  clust <- tree[!is.na(clustering) & clustering != "N", .N, by = table]
  if (nrow(clust) > 0)
    stop("Clustering present in tables: ", paste(clust$table, collapse = ", "))

  # ---- non-response -----------------------------------------------------------
  nr <- tree[!is.na(samp) & samp == "N"]
  if (nrow(nr) > 0) {
    if (nonResponse == "stop")
      stop(nrow(nr), " selected units were not sampled (samp == 'N'), e.g. ",
           nr$node[1], ". Decide how to treat non-response (nonResponse = ",
           "'respondents' assumes missing at random within stratum).")
    if (nonResponse != "respondents") stop("Unknown nonResponse option")
    if (any(tree$parent %in% nr$node))
      stop("Non-responding units have child records")
    tree <- tree[!node %in% nr$node]
    tree[, nResp := .N, by = .(parent, table, stratum)]
    tree[method %in% c("SRSWOR", "SRSWR"), n := nResp]
    tree[, nResp := NULL]
  }

  # ---- per stratum checks and weights ----------------------------------------
  tree[, nRows := .N, by = .(parent, table, stratum)]
  srs <- tree[method %in% c("SRSWOR", "SRSWR")]
  if (nrow(srs) > 0) {
    if (anyNA(srs$N) || anyNA(srs$n)) stop("SRS units with NA numTotal/numSamp")
    inc <- srs[, .(uN = uniqueN(N), un = uniqueN(n), n = n[1], rows = .N),
               by = .(parent, table, stratum)]
    if (any(inc$uN > 1 | inc$un > 1))
      stop("numTotal/numSamp not constant within a stratum, e.g. ",
           inc[uN > 1 | un > 1][1, paste(parent, table, stratum)])
    mism <- inc[rows != n]
    if (nrow(mism) > 0) {
      msg <- paste0(nrow(mism), " strata where rows != numSamp (tables ",
                    paste(unique(mism$table), collapse = ", "), ")")
      if (strictSampleSize) stop(msg) else warning(msg)
    }
  }
  if (any(tree$method == "UPSWOR" & is.na(tree$incProb)))
    stop("UPSWOR units without incProb")
  if (any(tree$method == "UPSWR" & is.na(tree$selProb)))
    stop("UPSWR units without selProb")
  if (any(tree$method == "UPSWOR" & tree$incProb <= 0, na.rm = TRUE) ||
      any(tree$method == "UPSWR" & tree$selProb <= 0, na.rm = TRUE))
    stop("Selected units with selection/inclusion probability <= 0")

  tree[, w := fcase(method == "CENSUS", 1,
                    method %in% c("SRSWOR", "SRSWR"), N / n,
                    method == "UPSWOR", 1 / incProb,
                    method == "UPSWR", 1 / (n * selProb))]
  # finite population correction of the between-unit variance; NA when the
  # design needs second order probabilities that the RDBES does not hold
  tree[, fpc := fcase(method == "CENSUS", 0,
                      method == "SRSWOR", 1 - n / N,
                      method %in% c("SRSWR", "UPSWR"), 1,
                      method == "UPSWOR", NA_real_)]
  # multiplier of the within-unit variance (Sarndal 4.3.1 vs 4.5.1)
  tree[, cw := fifelse(method %in% c("SRSWR", "UPSWR"), 0, w)]
  # with-replacement approximation of the variance, an explicit choice
  tree[, varianceAsWR := table %in% varianceAsWR & method != "CENSUS"]
  tree[varianceAsWR == TRUE, `:=`(fpc = 1, cw = 0)]

  # ---- ratio checks -------------------------------------------------------------
  rt <- tree[ratio == TRUE]
  if (nrow(rt) > 0) {
    if (anyNA(rt$x) || anyNA(rt$X))
      stop("Ratio stage with NA auxiliary values in tables: ",
           paste(unique(rt[is.na(x) | is.na(X), table]), collapse = ", "))
    rx <- rt[, uniqueN(X), by = .(parent, table, stratum)][V1 > 1]
    if (nrow(rx) > 0)
      stop("Auxiliary total (X) not constant within ", nrow(rx), " strata")
  }

  # ---- depth ---------------------------------------------------------------------
  tree[, depth := NA_integer_]
  tree[parent == "ROOT", depth := 1L]
  d <- 1L
  while (anyNA(tree$depth)) {
    nodesAtD <- tree[depth == d, node]
    if (length(nodesAtD) == 0) {
      orphan <- tree[is.na(depth)]
      stop(nrow(orphan), " units whose parent is not in the tree, e.g. ",
           orphan$node[1], " -> ", orphan$parent[1])
    }
    tree[is.na(depth) & parent %in% nodesAtD, depth := d + 1L]
    d <- d + 1L
  }

  attr(tree, "methodMap") <- methodMap
  tree[]
}


#' Fold leaf values up the design tree
#'
#' @param tree output of designTree()
#' @param leaves data.table(node, var, t, v): leaf values (v = their
#'   variance, normally 0 for measured values)
#' @return list(nodes = node level estimates, strata = estimates for each
#'   parent x child-stratum, tree = tree)
foldEstimate <- function(tree, leaves) {
  stopifnot(all(c("node", "var", "t", "v") %in% names(leaves)))
  leaves <- as.data.table(leaves)[, .(node, var, t, v)]
  if (anyNA(leaves$t)) stop("NA leaf values - decide how to treat them first")
  if (anyDuplicated(leaves[, .(node, var)])) stop("Duplicated leaf values")
  miss <- setdiff(unique(leaves$node), tree$node)
  if (length(miss) > 0) stop("Leaf nodes not in tree, e.g. ", miss[1])

  # dead branches: per variable, every node down to that variable's leaf
  # level must have at least one child, otherwise its value is missing,
  # not zero (create zeros explicitly, e.g. with generateZerosUsingSL)
  leaves[, leafTable := tree$table[match(leaves$node, tree$node)]]
  leafTables <- unique(leaves[, .(var, leafTable)])
  leaves[, leafTable := NULL]
  if (any(duplicated(leafTables$var)))
    stop("A variable has leaves in more than one table")
  for (lt in unique(leafTables$leafTable)) {
    leafNodes <- tree[table == lt, node]
    reach <- ancestorsOf(tree, leafNodes)
    # tables a variable at level lt passes through on its way up; every unit
    # of these tables must lead down to at least one lt unit
    upTables <- setdiff(unique(tree[node %in% reach, table]), lt)
    deadNodes <- tree[table %in% upTables & !node %in% reach, node]
    if (length(deadNodes) > 0)
      stop(length(deadNodes), " units have no child units down to table ", lt,
           ", e.g. ", deadNodes[1])
    # children in tables below lt (e.g. BV under SA for an SA variable) are
    # ignored for that variable; children in lt itself (SA sub-samples) are
    # not: leaf values must sit on the deepest unit of the chain
    nonLeaf <- intersect(leaves[var %in% leafTables[leafTable == lt, var], node],
                         tree[table == lt, parent])
    if (length(nonLeaf) > 0)
      stop("Leaf values given for units that are sub-sampled further, e.g. ",
           nonLeaf[1])
    # all units of the leaf table under the tree must have leaf values or
    # be legitimately zero - zeros are implicit, nothing to check here
  }

  # design aggregates per child stratum, over ALL units in it
  grp <- tree[, .(n_g = .N, fpc = fpc[1], ratio = ratio[1], X = X[1],
                  Swx = sum(w * x), Swx2 = sum((w * x)^2)),
              by = .(parent, table, stratum, depth)]

  est <- copy(leaves)
  strataOut <- list()
  for (d in max(tree$depth):1) {
    kids <- tree[depth == d, .(node, parent, table, stratum, w, cw, x)]
    e <- est[kids, on = "node", nomatch = NULL]
    if (nrow(e) == 0) next
    s <- e[, .(Swt = sum(w * t), Swt2 = sum((w * t)^2),
               # WR stages (cw = 0) do not need the lower stage variances,
               # which may be NA (e.g. one SA sample per stratum)
               Swtwx = sum((w * t) * (w * x)),
               within = sum(fifelse(cw == 0, 0, cw * v))),
           by = .(parent, table, stratum, var)]
    s <- grp[depth == d][s, on = .(parent, table, stratum)]
    s[, `:=`(t = Swt, ssq = Swt2 - Swt^2 / n_g, g = 1)]
    s[ratio == TRUE, `:=`(R = Swt / Swx, g = X / Swx)]
    s[ratio == TRUE, `:=`(t = X * R, ssq = Swt2 - 2 * R * Swtwx + R^2 * Swx2)]
    s[, vb := fifelse(fpc == 0, 0, fpc * n_g / (n_g - 1) * ssq)]
    s[n_g < 2 & fpc != 0, vb := NA_real_]
    s[, `:=`(vb = g^2 * vb, vw = g^2 * within)]
    s[, v := vb + vw]
    strataOut[[as.character(d)]] <- s[, .(parent, table, stratum, var, n_g,
                                          ratio, t, vb, vw, v)]
    up <- s[, .(t = sum(t), v = sum(v)), by = .(node = parent, var)]
    est <- rbind(est, up)
  }
  nodes <- tree[, .(node, table, stratum, depth)][est, on = "node"]
  nodes[node == "ROOT", `:=`(table = "ROOT", depth = 0L)]
  list(nodes = nodes[], strata = rbindlist(strataOut), tree = tree)
}


#' All ancestors (including the nodes themselves) of a set of nodes
ancestorsOf <- function(tree, nodes) {
  out <- nodes
  cur <- nodes
  par <- setNames(tree$parent, tree$node)
  repeat {
    cur <- unique(par[cur])
    cur <- cur[!is.na(cur) & cur != "ROOT"]
    if (length(cur) == 0) break
    out <- c(out, cur)
  }
  unique(out)
}


#' Value of an ancestor's column for each node, e.g. LEarea for BV fish
#'
#' @param tree design tree
#' @param nodes nodes to look up
#' @param rdbes RDBESDataObject
#' @param column column name; its table is taken from the 2-letter prefix
ancestorValue <- function(tree, nodes, rdbes, column) {
  tb <- substr(column, 1, 2)
  par <- setNames(tree$parent, tree$node)
  tab <- setNames(tree$table, tree$node)
  cur <- nodes
  for (i in seq_len(max(tree$depth))) {
    notYet <- !is.na(cur) & tab[cur] != tb
    if (!any(notYet)) break
    cur[notYet] <- par[cur[notYet]]
  }
  if (anyNA(cur) || any(tab[cur] != tb))
    stop("Table ", tb, " is not an ancestor of all nodes")
  id <- sub("^..:", "", cur)
  src <- rdbes[[tb]]
  src[[column]][match(id, as.character(src[[paste0(tb, "id")]]))]
}


# ---- leaf value helpers -------------------------------------------------------

#' Leaf values from an SA column (e.g. SAsampWtMes)
#' @param na "stop", or "zero" = treat units with NA as outside the domain
#'   (this is what survey::svytotal(na.rm = TRUE) does)
leavesSA <- function(tree, rdbes, column, na = "stop") {
  # deepest SA of each sub-sampling chain (FM/BV children do not matter)
  saNodes <- tree[table == "SA" & !node %in% tree[table == "SA", parent], node]
  sa <- rdbes$SA
  val <- as.numeric(sa[[column]][match(sub("^SA:", "", saNodes),
                                       as.character(sa$SAid))])
  if (anyNA(val)) {
    if (na == "stop") stop(sum(is.na(val)), " NA values in ", column)
    val[is.na(val)] <- 0
  }
  data.table(node = saNodes, var = column, t = val, v = 0)[t != 0]
}

#' Leaf values from BV: numbers (or sums of valueMeas) per class
#'
#' @param classMeas BVtypeMeas used for the classes (e.g. "Age")
#' @param breaks NULL to use the values themselves, or class breaks
#'   (left-closed) for continuous measurements
#' @param sumMeas NULL to count fish, or BVtypeMeas to sum (e.g. weight)
leavesBV <- function(tree, rdbes, classMeas, breaks = NULL, sumMeas = NULL) {
  fishNodes <- tree[table == "BV", .(node, parent, BVfishId)]
  bv <- copy(rdbes$BV)
  bv[, parentNode :=ifelse(is.na(FMid), paste0("SA:", SAid), paste0("FM:", FMid))]
  meas <- bv[BVtypeMeas %in% c(classMeas, sumMeas),
             .(parentNode, BVfishId, BVtypeMeas, BVvalueMeas)]
  if (anyDuplicated(meas[, .(parentNode, BVfishId, BVtypeMeas)]))
    stop("More than one value per fish and BVtypeMeas")
  wide <- dcast(meas, parentNode + BVfishId ~ BVtypeMeas, value.var = "BVvalueMeas")
  f <- wide[fishNodes, on = .(parentNode = parent, BVfishId)]
  cls <- f[[classMeas]]
  if (anyNA(cls)) stop(sum(is.na(cls)), " selected fish without ", classMeas)
  if (!is.null(breaks)) {
    cls <- as.character(cut(as.numeric(cls), breaks, right = FALSE))
    if (anyNA(cls)) stop("Measurements outside the class breaks")
  }
  val <- if (is.null(sumMeas)) 1 else as.numeric(f[[sumMeas]])
  if (anyNA(val)) stop("Selected fish without ", sumMeas)
  data.table(node = f$node,
             var = paste0(if (is.null(sumMeas)) "N" else sumMeas,
                          "|", classMeas, "=", cls),
             t = val, v = 0)
}


#' Ratio estimator to known totals from outside the design (e.g. CL landings)
#'
#' Combined ratio within each domain d:  t_y,d = X_d * t_hat_y,d / t_hat_x,d.
#' Variance by linearisation: the residual e = y - R_d x is formed at the
#' level where both y and x exist (the leaves of treeUpper, e.g. SA units),
#' and folded up the same design (second pass):
#'   V(t_y,d) = (X_d / t_hat_x,d)^2 * V(t_hat_e,d)
#'
#' @param fit foldEstimate() result containing the y and x variables
#' @param treeUpper designTree() pruned at the injection level (e.g.
#'   lowestTable = "SA"), with the same stage options as for fit
#' @param yVars names of the y variables, e.g. "N|Age=3"
#' @param xVar name of the x variable, e.g. "SAsampWtMes"
#' @param domainOf function(nodes) returning the domain label of each
#'   injection-level node (e.g. quarter from TEstratumName)
#' @param X named numeric vector of known totals per domain, in the units
#'   of xVar
#' @return data.table with one row per y variable and domain
ratioToTotal <- function(fit, treeUpper, yVars, xVar, domainOf, X) {
  inj <- treeUpper[!node %in% treeUpper$parent, node]
  if (uniqueN(treeUpper[node %in% inj, table]) != 1)
    stop("treeUpper leaves must all be in one table")
  get1 <- function(variable) {
    r <- fit$nodes[var == variable]
    i <- match(inj, r$node)
    data.table(node = inj, t = fifelse(is.na(i), 0, r$t[i]),
               v = fifelse(is.na(i), 0, r$v[i]))
  }
  dom <- domainOf(inj)
  if (anyNA(dom)) stop("Domain missing for some units")
  if (!all(unique(dom) %in% names(X))) stop("No known total for domains: ",
                                            paste(setdiff(unique(dom), names(X)), collapse = ", "))
  xv <- get1(xVar)
  if (any(xv$v != 0))
    stop("x has a variance at the injection level: the y-x covariance would ",
         "be needed. Inject at a level where x is known.")

  # pass 1: domain totals of y and x through the upper design
  lab <- function(v) paste0(v, "#", dom)
  l1 <- rbindlist(c(list(xv[, .(node, var = lab(xVar), t, v)]),
                    lapply(yVars, function(y) get1(y)[, .(node, var = lab(y), t, v)])))
  f1 <- foldEstimate(treeUpper, unique(l1)[t != 0 | v != 0])
  root <- f1$nodes[node == "ROOT", .(var, t)]
  tot <- function(v) { r <- root$t[match(v, root$var)]; fifelse(is.na(r), 0, r) }

  # pass 2: linearised residuals e = y - R x
  out <- list()
  for (y in yVars) {
    yv <- get1(y)
    R <- tot(lab(y)) / tot(lab(xVar))
    e <- data.table(node = inj, var = paste0("e:", lab(y)),
                    t = yv$t - R * xv$t, v = yv$v)
    f2 <- foldEstimate(treeUpper, e[t != 0 | v != 0])
    for (d in unique(dom)) {
      tx <- tot(paste0(xVar, "#", d))
      ty <- tot(paste0(y, "#", d))
      ve <- f2$nodes[node == "ROOT" & var == paste0("e:", y, "#", d), v]
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
