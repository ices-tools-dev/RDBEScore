#' Leaf values from SA fields for design-based estimation
#'
#' Takes the values of one or more SA fields of the lowest SA units (the
#' deepest unit of each sub-sampling chain) of a design tree.
#'
#' @param designTree A design tree from [createDesignTree()].
#' @param RDBESDataObject The RDBESDataObject the tree was made from.
#' @param fields SA fields to use, e.g. "SAsampWtMes".
#' @param na "stop" (default) stops when values are missing. "zero" sets
#' them to zero, i.e. treats those units as outside the domain of interest;
#' this is what `survey::svytotal(..., na.rm = TRUE)` does.
#'
#' @return A data.table with the columns `node`, `var`, `t` and `v`, for use
#' in [doEstimationOnDesignTree()]. Zero values are left out.
#' @export
#'
#' @examples
#' tree <- createDesignTree(H8ExampleEE1, "SA", strictSampleSize = FALSE)
#' getLeafValuesSA(tree, H8ExampleEE1, "SAsampWtMes")
getLeafValuesSA <- function(designTree, RDBESDataObject, fields,
                            na = "stop") {
  if (!na %in% c("stop", "zero")) stop("na must be 'stop' or 'zero'")
  sa <- RDBESDataObject$SA
  missingFields <- setdiff(fields, names(sa))
  if (length(missingFields) > 0) {
    stop("Fields not in SA: ", paste(missingFields, collapse = ", "))
  }
  isSA <- designTree$table == "SA"
  saNodes <- designTree$node[isSA & !designTree$node %in% designTree$parent[isSA]]
  rows <- match(sub("^SA:", "", saNodes), as.character(sa$SAid))
  rbindlist(lapply(fields, function(field) {
    value <- as.numeric(as.character(sa[[field]][rows]))
    if (anyNA(value)) {
      if (na == "stop") {
        stop(sum(is.na(value)), " missing values in ", field,
             " - decide how to treat them (na = 'zero' excludes those units",
             " from the domain)")
      }
      value[is.na(value)] <- 0
    }
    data.table(node = saNodes, var = field, t = value, v = 0)[t != 0]
  }))
}


#' Leaf values from BV for design-based estimation
#'
#' Numbers of fish (or the sum of a measurement, e.g. weight) by class of a
#' measurement (e.g. age or length class) for the fish units of a design
#' tree created with `lowestTable = "BV"`.
#'
#' @param designTree A design tree from [createDesignTree()] with BV units.
#' @param RDBESDataObject The RDBESDataObject the tree was made from.
#' @param classMeas The BVtypeMeas defining the classes, e.g. "Age".
#' @param breaks NULL to use the measured values as classes, or class
#' breaks (left closed) for continuous measurements such as length.
#' @param sumMeas NULL to count fish, or the BVtypeMeas to sum by class,
#' e.g. "WeightMeasured".
#'
#' @return A data.table with the columns `node`, `var`, `t` and `v`. The
#' variables are named `N|<classMeas>=<class>` (or `<sumMeas>|...`).
#' @export
#'
#' @examples
#' tree <- createDesignTree(H8ExampleEE1, "BV", bvAssess = "Age",
#'                          strictSampleSize = FALSE)
#' head(getLeafValuesBV(tree, H8ExampleEE1, classMeas = "Age"))
getLeafValuesBV <- function(designTree, RDBESDataObject, classMeas,
                            breaks = NULL, sumMeas = NULL) {
  fish <- designTree[designTree$table == "BV", .(node, parent, BVfishId)]
  if (nrow(fish) == 0) stop("The design tree has no BV units")
  bv <- copy(RDBESDataObject$BV)
  bv[, parentNode := fifelse(is.na(FMid), paste0("SA:", SAid),
                             paste0("FM:", FMid))]
  meas <- bv[BVtypeMeas %in% c(classMeas, sumMeas),
             .(parentNode, BVfishId, BVtypeMeas, BVvalueMeas)]
  if (anyDuplicated(meas[, .(parentNode, BVfishId, BVtypeMeas)])) {
    stop("More than one value per fish and BVtypeMeas")
  }
  wide <- dcast(meas, parentNode + BVfishId ~ BVtypeMeas,
                value.var = "BVvalueMeas")
  f <- wide[fish, on = .(parentNode = parent, BVfishId)]
  if (!classMeas %in% names(f)) stop("No BV measurements of ", classMeas)
  cls <- f[[classMeas]]
  if (anyNA(cls)) {
    stop(sum(is.na(cls)), " selected fish without a ", classMeas, " value")
  }
  if (!is.null(breaks)) {
    cls <- as.character(cut(as.numeric(cls), breaks, right = FALSE))
    if (anyNA(cls)) stop("Measurements outside the class breaks")
  }
  if (is.null(sumMeas)) {
    value <- 1
  } else {
    if (!sumMeas %in% names(f)) stop("No BV measurements of ", sumMeas)
    value <- as.numeric(f[[sumMeas]])
    if (anyNA(value)) stop("Selected fish without a ", sumMeas, " value")
  }
  data.table(node = f$node,
             var = paste0(if (is.null(sumMeas)) "N" else sumMeas, "|",
                          classMeas, "=", cls),
             t = value, v = 0)
}


#' Leaf values from FM for design-based estimation
#'
#' Numbers of fish by length (or other) class from the FM tally, for a
#' design tree created with `lowestTable = "FM"`.
#'
#' @param designTree A design tree from [createDesignTree()] with FM units.
#' @param RDBESDataObject The RDBESDataObject the tree was made from.
#' @param breaks NULL to use FMclassMeas as classes, or class breaks (left
#' closed).
#'
#' @return A data.table with the columns `node`, `var`, `t` and `v`. The
#' variables are named `N|FMclassMeas=<class>`.
#' @export
#'
#' @examples
#' x <- filterRDBESDataObject(H1Example, "DEstratumName", "DE_stratum1_H1",
#'                            killOrphans = TRUE)
#' tree <- createDesignTree(x, "FM")
#' head(getLeafValuesFM(tree, x))
getLeafValuesFM <- function(designTree, RDBESDataObject, breaks = NULL) {
  fmNodes <- designTree$node[designTree$table == "FM"]
  if (length(fmNodes) == 0) stop("The design tree has no FM units")
  fm <- RDBESDataObject$FM
  rows <- match(sub("^FM:", "", fmNodes), as.character(fm$FMid))
  cls <- fm$FMclassMeas[rows]
  if (!is.null(breaks)) {
    cls <- as.character(cut(as.numeric(cls), breaks, right = FALSE))
    if (anyNA(cls)) stop("FMclassMeas outside the class breaks")
  }
  value <- as.numeric(fm$FMnumAtUnit[rows])
  if (anyNA(value) || anyNA(cls)) stop("FM rows with missing values")
  out <- data.table(node = fmNodes, var = paste0("N|FMclassMeas=", cls),
                    t = value, v = 0)
  out[t != 0]
}
