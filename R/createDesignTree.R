#' Create the design tree of an RDBESDataObject
#'
#' Flattens the sampling units of all tables of an RDBESDataObject into one
#' table with one row per sampling unit. Each row points to its parent unit
#' and holds the design variables that describe how the unit was selected
#' from its parent (stratum, selection method, numbers total and sampled,
#' selection and inclusion probabilities, auxiliary variables). The design
#' tree is the input for [doEstimationOnDesignTree()].
#'
#' DE and SD are census pseudo-stages. FM rows are treated as a complete
#' tally of their SA sample (each class is its own census stratum). BV units
#' are fish: for the assessment type `bvAssess` each fish is one unit whose
#' parent is its FM row (lower hierarchy A) or SA row (lower hierarchy C).
#' Sub-sampled SA rows (`SAparSequNum`) get the parent SA as their parent.
#' Clustering (`<T>clustering` "1C" or "2C") adds a cluster stage between a
#' unit and its parent: clusters are selected with the `<T>...Cluster`
#' design variables, the units within each cluster with the ordinary ones.
#'
#' The function stops when the design information is incomplete or
#' inconsistent, instead of silently making assumptions. Each argument that
#' relaxes a check is an explicit analytical choice.
#'
#' @param RDBESDataObject An RDBESDataObject with a single upper hierarchy.
#' @param lowestTable The lowest table to include: "SA", "FM" or "BV".
#' @param bvAssess The BVtypeAssess whose selection design defines the BV
#' units (fish). Required if `lowestTable = "BV"`.
#' @param ratio Optional named character vector. Names are the tables whose
#' stage uses a ratio estimator, values the auxiliary variable: "weight"
#' (SA only; x = SAsampWtMes, X = SAtotalWtMes) or "aux"
#' (x = `<T>auxVarValue`, X = `<T>auxVarTot`).
#' @param methodMap Optional named character vector mapping selection methods
#' without an implemented estimator (e.g. `c(NPQSRSWOR = "SRSWOR")`) to an
#' implemented one. The original method is kept in `methodOriginal`.
#' @param nonResponse "stop" (default) stops when selected units were not
#' sampled (`<T>samp == "N"`). "respondents" drops them and treats the
#' respondents as the sample (n = number of responding units), which assumes
#' that the non-response is random within the stratum.
#' @param strictSampleSize If TRUE (default) stop when the number of sampled
#' units in a stratum differs from `<T>numSamp`; if FALSE only warn.
#' @param varianceAsWR Optional tables whose stage variance is computed as if
#' the units had been selected with replacement (no finite population
#' correction, lower stage variances not needed). Point estimates are
#' unchanged; for without replacement designs the variance is overestimated.
#' Cluster stages are named `<T>_cluster`, e.g. "VS_cluster".
#' @param verbose (Optional) Set to TRUE to print informative text. The
#' default is FALSE.
#'
#' @return A data.table with one row per sampling unit and the columns
#' `node`, `parent`, `table`, `stratum`, `method`, `N`, `n`, `selProb`,
#' `incProb`, the expansion weight `w`, the finite population correction
#' `fpc`, the within-unit variance multiplier `cw`, the auxiliary values `x`
#' and `X`, `ratio` and the `depth` of the unit in the tree.
#' @export
#'
#' @seealso [doEstimationOnDesignTree()], [getLeafValuesSA()],
#' [getLeafValuesBV()], [getLeafValuesFM()]
#'
#' @examples
#' tree <- createDesignTree(H8ExampleEE1, "BV", bvAssess = "Age",
#'                          ratio = c(SA = "weight"),
#'                          strictSampleSize = FALSE)
#' tree[, .N, by = .(table, method)]
createDesignTree <- function(RDBESDataObject,
                             lowestTable = "SA",
                             bvAssess = NULL,
                             ratio = NULL,
                             methodMap = NULL,
                             nonResponse = "stop",
                             strictSampleSize = TRUE,
                             varianceAsWR = NULL,
                             verbose = FALSE) {

  if (!inherits(RDBESDataObject, "RDBESDataObject")) {
    stop("RDBESDataObject must be of class RDBESDataObject")
  }
  validateRDBESDataObject(RDBESDataObject, verbose = verbose)
  rdbes <- RDBESDataObject

  hierarchy <- unique(rdbes$DE$DEhierarchy)
  if (length(hierarchy) != 1) {
    stop("Exactly one DEhierarchy is required, found: ",
         paste(hierarchy, collapse = ", "))
  }
  upperTables <- getTablesInRDBESHierarchy(hierarchy,
                                           includeOptTables = FALSE,
                                           includeLowHierTables = FALSE,
                                           includeTablesNotInSampHier = FALSE)
  lowerTables <- switch(lowestTable,
                        SA = character(0),
                        FM = "FM",
                        BV = c("FM", "BV"),
                        stop("lowestTable must be 'SA', 'FM' or 'BV'"))
  if ("BV" %in% lowerTables && is.null(bvAssess)) {
    stop("bvAssess must be given when lowestTable = 'BV'")
  }
  if (!nonResponse %in% c("stop", "respondents")) {
    stop("nonResponse must be 'stop' or 'respondents'")
  }
  if (length(ratio) > 0) {
    if (is.null(names(ratio)) || any(names(ratio) == "")) {
      stop("ratio must be a named character vector, e.g. c(SA = 'weight')")
    }
    unknown <- setdiff(names(ratio), c(upperTables, "BV"))
    if (length(unknown) > 0) {
      stop("Ratio requested for tables not in the hierarchy: ",
           paste(unknown, collapse = ", "))
    }
    if (!all(ratio %in% c("weight", "aux"))) {
      stop("ratio values must be 'weight' or 'aux'")
    }
    if (any(ratio == "weight" & names(ratio) != "SA")) {
      stop("ratio = 'weight' is only defined for SA")
    }
  }

  units <- list()
  units$DE <- data.table(node = paste0("DE:", rdbes$DE$DEid),
                         parent = "ROOT", table = "DE",
                         stratum = paste0(rdbes$DE$DEyear, "-",
                                          rdbes$DE$DEstratumName),
                         method = "CENSUS")
  units$SD <- data.table(node = paste0("SD:", rdbes$SD$SDid),
                         parent = paste0("DE:", rdbes$SD$DEid), table = "SD",
                         stratum = paste0(rdbes$SD$SDctry, "-",
                                          rdbes$SD$SDinst),
                         method = "CENSUS")

  previousTable <- "SD"
  for (tableName in setdiff(upperTables, c("DE", "SD", "SA"))) {
    tbl <- rdbes[[tableName]]
    if (is.null(tbl) || nrow(tbl) == 0) {
      stop("Table ", tableName, " is empty")
    }
    parentIds <- tbl[[paste0(previousTable, "id")]]
    if (is.null(parentIds) || anyNA(parentIds)) {
      stop("Table ", tableName, " has rows without ", previousTable, "id")
    }
    units[[tableName]] <- designTreeUnits(tbl, tableName,
                                          paste0(previousTable, ":", parentIds),
                                          ratio)
    previousTable <- tableName
  }

  sa <- rdbes$SA
  if (is.null(sa) || nrow(sa) == 0) stop("Table SA is empty")
  units$SA <- designTreeUnits(sa, "SA", designTreeSAParents(sa), ratio)

  if ("FM" %in% lowerTables && !is.null(rdbes$FM) && nrow(rdbes$FM) > 0) {
    units$FM <- data.table(node = paste0("FM:", rdbes$FM$FMid),
                           parent = paste0("SA:", rdbes$FM$SAid),
                           table = "FM",
                           stratum = paste0("FM:", rdbes$FM$FMid),
                           method = "CENSUS")
  }
  if ("BV" %in% lowerTables) {
    units$BV <- designTreeBVUnits(rdbes$BV, bvAssess, ratio)
  }

  tree <- rbindlist(units, fill = TRUE)
  tree[is.na(ratio), ratio := FALSE]
  tree <- designTreeAddClusters(tree)
  if (anyDuplicated(tree$node)) stop("Duplicated sampling units in the tree")

  tree <- designTreeCheckAndWeight(tree, methodMap, nonResponse,
                                   strictSampleSize, varianceAsWR)

  # depth of each unit, the root being at depth 0
  tree[, depth := NA_integer_]
  tree[parent == "ROOT", depth := 1L]
  d <- 1L
  while (anyNA(tree$depth)) {
    nodesAtDepth <- tree[depth == d, node]
    if (length(nodesAtDepth) == 0) {
      orphans <- tree[is.na(depth)]
      stop(nrow(orphans), " units whose parent is not in the tree, e.g. ",
           orphans$node[1], " -> ", orphans$parent[1])
    }
    tree[is.na(depth) & parent %in% nodesAtDepth, depth := d + 1L]
    d <- d + 1L
  }

  if (verbose) {
    print(tree[, .N, by = .(table, method)])
  }
  tree[]
}


#' Design tree rows of one sampling unit table (internal)
#'
#' @param tbl The table.
#' @param tableName Its two letter name.
#' @param parentNodes The parent node of each row.
#' @param ratio The ratio argument of createDesignTree().
#' @return data.table of units
#' @noRd
designTreeUnits <- function(tbl, tableName, parentNodes, ratio) {
  col <- function(field) {
    name <- paste0(tableName, field)
    if (name %in% names(tbl)) tbl[[name]] else rep(NA, nrow(tbl))
  }
  num <- function(x) as.numeric(as.character(x))
  u <- data.table(
    node = paste0(tableName, ":", tbl[[paste0(tableName, "id")]]),
    parent = parentNodes,
    table = tableName,
    stratum = as.character(col("stratumName")),
    method = as.character(col("selectMeth")),
    N = num(col("numTotal")),
    n = num(col("numSamp")),
    selProb = num(col("selProb")),
    incProb = num(col("incProb")),
    samp = as.character(col("samp")),
    clustering = as.character(col("clustering")),
    clusterName = as.character(col("clusterName")),
    methodCluster = as.character(col("selectMethCluster")),
    NCluster = num(col("numTotalClusters")),
    nCluster = num(col("numSampClusters")),
    selProbCluster = num(col("selProbCluster")),
    incProbCluster = num(col("incProbCluster")),
    x = NA_real_, X = NA_real_, ratio = FALSE
  )
  if (tableName %in% names(ratio)) {
    if (ratio[[tableName]] == "weight") {
      u[, `:=`(x = num(tbl$SAsampWtMes), X = num(tbl$SAtotalWtMes))]
    } else {
      u[, `:=`(x = num(col("auxVarValue")), X = num(col("auxVarTot")))]
    }
    u[, ratio := TRUE]
  }
  u
}


#' Parent node of each SA row: its SS, or its parent SA if sub-sampled
#' @param sa The SA table.
#' @return character vector of parent nodes
#' @noRd
designTreeSAParents <- function(sa) {
  parents <- paste0("SS:", sa$SSid)
  isSub <- !is.na(sa$SAparSequNum)
  if (any(isSub)) {
    key <- sa[, .(SSid, SAseqNum, parSAid = SAid)]
    m <- key[sa[, .(SSid, SAparSequNum)],
             on = .(SSid, SAseqNum = SAparSequNum), mult = "all"]
    if (nrow(m) != nrow(sa)) {
      stop("SAparSequNum does not identify a unique parent SA within SSid")
    }
    if (any(isSub & is.na(m$parSAid))) {
      stop("Sub-sampled SA rows without a parent SA: ",
           paste(sa$SAid[isSub & is.na(m$parSAid)], collapse = ", "))
    }
    parents[isSub] <- paste0("SA:", m$parSAid[isSub])
  }
  parents
}


#' BV units (fish) selected for one assessment type
#' @param bv The BV table.
#' @param bvAssess The BVtypeAssess.
#' @param ratio The ratio argument of createDesignTree().
#' @return data.table of units
#' @noRd
designTreeBVUnits <- function(bv, bvAssess, ratio) {
  if (is.null(bv) || nrow(bv) == 0) stop("Table BV is empty")
  if ("BV" %in% names(ratio)) stop("Ratio estimation is not defined for BV")
  # a logical vector, not BVtypeAssess == bvAssess, so that data.table does
  # not add an index to the input table
  bv <- bv[bv$BVtypeAssess %in% bvAssess]
  if (nrow(bv) == 0) stop("No BV rows with BVtypeAssess == ", bvAssess)
  bv[, parentNode := fifelse(is.na(FMid), paste0("SA:", SAid),
                             paste0("FM:", FMid))]
  fish <- unique(bv[, .(parentNode, BVfishId, BVstratumName, BVselectMeth,
                        BVnumTotal, BVnumSamp, BVselProb, BVincProb)])
  dup <- fish[, .N, by = .(parentNode, BVfishId)][N > 1]
  if (nrow(dup) > 0) {
    stop("Fish with conflicting BV design values for assessment ", bvAssess,
         ", e.g. BVfishId ", dup$BVfishId[1])
  }
  num <- function(x) as.numeric(as.character(x))
  data.table(node = paste0("BV:", fish$parentNode, "/", fish$BVfishId),
             parent = fish$parentNode, table = "BV",
             stratum = as.character(fish$BVstratumName),
             method = as.character(fish$BVselectMeth),
             N = num(fish$BVnumTotal), n = num(fish$BVnumSamp),
             selProb = num(fish$BVselProb), incProb = num(fish$BVincProb),
             samp = "Y", BVfishId = fish$BVfishId,
             x = NA_real_, X = NA_real_, ratio = FALSE)
}


#' Insert cluster stages for clustered units
#' @param tree The design tree.
#' @return The design tree with cluster nodes
#' @noRd
designTreeAddClusters <- function(tree) {
  isClustered <- !is.na(tree$clustering) & tree$clustering != "N"
  if (!any(isClustered)) return(tree)
  cl <- tree[isClustered]
  if (any(cl$ratio)) stop("Ratio estimation with clustering is not implemented")
  if (anyNA(cl$clusterName)) stop("Clustered units without clusterName")
  cl[, clusterNode := paste0(table, "_cluster:", parent, "/", stratum, "/",
                             clusterName)]
  clusters <- unique(cl[, .(node = clusterNode, parent,
                            table = paste0(table, "_cluster"), stratum,
                            method = methodCluster, N = NCluster,
                            n = nCluster, selProb = selProbCluster,
                            incProb = incProbCluster, samp = "Y",
                            ratio = FALSE)])
  if (anyDuplicated(clusters$node)) {
    stop("Cluster design variables differ within a cluster, e.g. ",
         clusters$node[duplicated(clusters$node)][1])
  }
  tree[isClustered, parent := cl$clusterNode]
  rbind(tree, clusters, fill = TRUE)
}


#' Check the design of each stratum and compute the weights
#' @param tree The design tree.
#' @param methodMap,nonResponse,strictSampleSize,varianceAsWR See
#'   createDesignTree().
#' @return The design tree with w, fpc and cw
#' @noRd
designTreeCheckAndWeight <- function(tree, methodMap, nonResponse,
                                     strictSampleSize, varianceAsWR) {
  implemented <- c("CENSUS", "SRSWOR", "SRSWR", "UPSWOR", "UPSWR")

  tree[, methodOriginal := method]
  if (length(methodMap) > 0) {
    mapped <- tree$method %in% names(methodMap)
    tree[mapped, method := methodMap[method]]
  }
  bad <- tree[!method %in% implemented | is.na(method), .N,
              by = .(table, method = methodOriginal)]
  if (nrow(bad) > 0) {
    stop("Selection methods without an implemented estimator (methodMap ",
         "can map them if the assumption is acceptable):\n",
         paste(utils::capture.output(print(bad)), collapse = "\n"))
  }

  notSampled <- tree[!is.na(samp) & samp == "N"]
  if (nrow(notSampled) > 0) {
    if (nonResponse == "stop") {
      stop(nrow(notSampled), " selected units were not sampled (samp == ",
           "'N'), e.g. ", notSampled$node[1], ". Decide how to treat the ",
           "non-response (nonResponse = 'respondents' assumes it is random ",
           "within the stratum).")
    }
    if (any(tree$parent %in% notSampled$node)) {
      stop("Units with samp == 'N' have child records")
    }
    if (any(notSampled$method %in% c("UPSWOR", "UPSWR"))) {
      stop("nonResponse = 'respondents' is not implemented for UPS methods")
    }
    tree <- tree[!node %in% notSampled$node]
    tree[, nResp := .N, by = .(parent, table, stratum)]
    tree[method %in% c("SRSWOR", "SRSWR"), n := nResp]
    tree[, nResp := NULL]
  }

  srs <- tree[method %in% c("SRSWOR", "SRSWR")]
  if (nrow(srs) > 0) {
    if (anyNA(srs$N) || anyNA(srs$n)) {
      stop("SRS units without numTotal or numSamp in tables: ",
           paste(unique(srs[is.na(N) | is.na(n), table]), collapse = ", "))
    }
    perStratum <- srs[, .(uN = uniqueN(N), un = uniqueN(n), n = n[1],
                          units = .N), by = .(parent, table, stratum)]
    if (any(perStratum$uN > 1 | perStratum$un > 1)) {
      stop("numTotal or numSamp differ within a stratum, e.g. ",
           perStratum[uN > 1 | un > 1][1, paste(parent, table, stratum)])
    }
    mismatch <- perStratum[units != n]
    if (nrow(mismatch) > 0) {
      msg <- paste0(nrow(mismatch), " strata where the number of sampled ",
                    "units differs from numSamp (tables ",
                    paste(unique(mismatch$table), collapse = ", "), ")")
      if (strictSampleSize) stop(msg) else warning(msg)
    }
  }
  if (any(tree$method == "UPSWOR" & is.na(tree$incProb))) {
    stop("UPSWOR units without incProb")
  }
  if (any(tree$method == "UPSWR" & (is.na(tree$selProb) | is.na(tree$n)))) {
    stop("UPSWR units without selProb or numSamp")
  }
  if (any(tree$method == "UPSWOR" & tree$incProb <= 0, na.rm = TRUE) ||
      any(tree$method == "UPSWR" & tree$selProb <= 0, na.rm = TRUE)) {
    stop("Selected units with a selection or inclusion probability <= 0")
  }

  tree[, w := fcase(method == "CENSUS", 1,
                    method %in% c("SRSWOR", "SRSWR"), N / n,
                    method == "UPSWOR", 1 / incProb,
                    method == "UPSWR", 1 / (n * selProb))]
  # the RDBES has no second order inclusion probabilities, so the UPSWOR
  # variance is NA unless varianceAsWR is used
  tree[, fpc := fcase(method == "CENSUS", 0,
                      method == "SRSWOR", 1 - n / N,
                      method %in% c("SRSWR", "UPSWR"), 1,
                      method == "UPSWOR", NA_real_)]
  tree[, cw := fifelse(method %in% c("SRSWR", "UPSWR"), 0, w)]
  unknownWR <- setdiff(varianceAsWR, unique(tree$table))
  if (length(unknownWR) > 0) {
    stop("varianceAsWR tables not in the tree: ",
         paste(unknownWR, collapse = ", "))
  }
  tree[, varianceAsWR := table %in% varianceAsWR & method != "CENSUS"]
  tree[varianceAsWR == TRUE, `:=`(fpc = 1, cw = 0)]

  ratioUnits <- tree[ratio == TRUE]
  if (nrow(ratioUnits) > 0) {
    if (anyNA(ratioUnits$x) || anyNA(ratioUnits$X)) {
      stop("Ratio stage with missing auxiliary values in tables: ",
           paste(unique(ratioUnits[is.na(x) | is.na(X), table]),
                 collapse = ", "))
    }
    varyingX <- ratioUnits[, uniqueN(X), by = .(parent, table, stratum)]
    if (any(varyingX$V1 > 1)) {
      stop("The auxiliary total differs within ", sum(varyingX$V1 > 1),
           " strata")
    }
  }
  tree
}
