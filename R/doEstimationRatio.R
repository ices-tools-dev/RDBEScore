#' Estimate Numbers and Mean Values by Length or Age Class
#'
#' @param RDBESDataObj A validated RDBESDataObject containing hierarchical
#'   sampling and biological data. Must include relevant tables (SA, FM,
#'   and/or BV) depending on the lower hierarchy.
#' @param targetValue A character string specifying the type of composition to
#'   estimate. Options are \code{"LengthComp"} or \code{"AgeComp"}.
#' @param raiseVar The raising variable used to construct the ratio estimator.
#'   Options are \code{"Weight"} (SAtotalWtMes / SAsampWtMes),
#'   \code{"Count"} (SAnumTotal / SAnumSamp), or any other value which will
#'   use \code{SAauxVarValue} directly as the raise factor.
#' @param classUnits Units of the length class intervals, e.g. \code{"mm"} or
#'   \code{"cm"}. Used only for \code{targetValue = "LengthComp"}: in LH A
#'   and B, checks consistency with FM data; in LH C, used to interpret raw BV
#'   lengths before class assignment.
#'   Codes: \url{https://vocab.ices.dk/?ref=1608}
#' @param classBreaks A numeric vector of three values:
#'   \code{c(min, max, width)}. Defines the length class intervals. Used only
#'   for \code{targetValue = "LengthComp"}.
#' @param LWparam A numeric vector of length two \code{c(a, b)} specifying the
#'   weight-length relationship (W = a * L^b). Used in LH A and B when
#'   individual weights are absent from BV but lengths are available.
#' @param verbose Logical; if \code{TRUE}, informational messages are printed
#'   during processing.
#'
#' @return A \code{data.table} with estimated numbers at length or age and
#'   associated mean values. Key output columns:
#'   \itemize{
#'     \item \code{LengthClass} or \code{Age} — the grouping variable
#'     \item \code{NumbersAtLength} or \code{NumbersAtAge} — raised estimates
#'     \item \code{MeanWeightAtLength} or \code{BVMeanWeight} — mean weight
#'     \item \code{MeanLengthAtAge} — mean length at age (AgeComp only)
#'     \item \code{raiseFactor} — the SA-level raise factor applied
#'   }
#'
#' @details
#' The three lower hierarchies differ in which tables are present:
#' \itemize{
#'   \item \strong{LH A}: \code{SA -> FM -> BV}. FM holds the length-frequency;
#'     BV holds individual biological measurements for a subsample of
#'     fish within each FM length class. Supports both LengthComp and AgeComp.
#'   \item \strong{LH B}: \code{SA -> FM} only. FM holds the length-frequency;
#'     no individual biological measurements (no BV). Supports
#'     LengthComp only; AgeComp is not possible without BV.
#'   \item \strong{LH C}: \code{SA -> BV} only. Individual biological
#'     measurements link directly to SA; no FM length-frequency tally.
#'     Supports both LengthComp and AgeComp.
#' }
#'
#' @importFrom utils tail
doEstimationRatio <- function(RDBESDataObj,
                              targetValue  = "LengthComp",
                              raiseVar     = "Weight",
                              classUnits   = "mm",
                              classBreaks  = c(100, 300, 10),
                              LWparam      = NULL,
                              verbose      = FALSE) {


  # ---------------------------------------------------------------------------
  # Checks
  # ---------------------------------------------------------------------------

  RDBEScore::validateRDBESDataObject(RDBESDataObj, verbose = FALSE)

  if (length(unique(RDBESDataObj$DE$DEhierarchy)) > 1)
    stop("Multiple upper hierarchies not implemented")

  if (length(unique(RDBESDataObj$SA$SAlowHierarchy)) > 1)
    stop("Multiple lower hierarchies not allowed")

  RDBESEstRatioObj <- Filter(Negate(is.null), RDBESDataObj)

  lh <- unique(RDBESEstRatioObj$SA$SAlowHierarchy)

  # Warn if length-class arguments are passed but irrelevant
  if (targetValue == "AgeComp") {
    if (!missing(classUnits))
      warning("'classUnits' is ignored when targetValue = 'AgeComp'.")
    if (!missing(classBreaks))
      warning("'classBreaks' is ignored when targetValue = 'AgeComp'.")
  }

  # AgeComp is not possible without individual fish records
  if (targetValue == "AgeComp" && lh == "B")
    stop("AgeComp is not possible with lower hierarchy B: LH B has no individual ",
         "fish records (no BV table). Use LH A or LH C for age composition.")

  # ---------------------------------------------------------------------------
  # Get the appropriate weight for raising
  # ---------------------------------------------------------------------------

  wcol <- NULL

  bv_present <- !is.null(RDBESEstRatioObj$BV)

  if (bv_present) {

    weightVar <- grep("(?i)weight", unique(RDBESEstRatioObj$BV$BVtypeMeas),
                      value = TRUE, perl = TRUE)

    if (lh == "A" && (length(weightVar) == 0 || all(is.na(weightVar))))
      stop("No individual weight measured in BV (required for lower hierarchy A)")

    if (length(weightVar) > 0) {
      if (interactive() && length(unique(weightVar)) > 1) {
        idx <- utils::menu(weightVar, title = "Select the BV weight type to use:")
        if (idx == 0L) stop("Selection cancelled.")
        wcol <- weightVar[idx]
      } else {
        if (length(unique(weightVar)) > 1)
          message("Multiple weight types found; using first: ", weightVar[1L])
        else if (verbose)
          message("Only one weight type present. Using: ", weightVar[1L])
        wcol <- weightVar[1L]
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Helpers - SHould be moved to utils?
  # ---------------------------------------------------------------------------

  # Check FM length-class consistency (LH A and B)
  checkLC <- function(fm, classUnits, classBreaks) {
    vocabUnits <- c("mm", "25mm", "cm", "5cm", "scm", "smm")
    fmUnits    <- unique(fm$FMaccuracy)

    if (!classUnits %in% vocabUnits)
      stop(paste("Invalid classUnits:", classUnits,
                 "\nMust be one of:", paste(vocabUnits, collapse = ", ")))
    if (length(fmUnits) > 1)
      stop("Multiple class units found in FM data: ", paste(fmUnits, collapse = ", "))
    if (!classUnits %in% fmUnits)
      stop(paste("Mismatch: user classUnits (", classUnits,
                 ") vs FM data units (", paste(fmUnits, collapse = ", "), ")."))

    fmBreaks  <- range(fm$FMclassMeas, na.rm = TRUE)
    userRange <- range(seq(classBreaks[1], classBreaks[2], classBreaks[3]))
    if (any(userRange != fmBreaks))
      stop("classBreaks (", paste(userRange, collapse = "-"),
           ") differ from FM data range (", paste(fmBreaks, collapse = "-"), ").")

    message("Length class checks passed: units and breaks are consistent.")
  }

  brks <- seq(classBreaks[1], classBreaks[2], by = classBreaks[3])

  assignLengthClass <- function(lengths) {
    cut(lengths,
        breaks         = brks,
        right          = FALSE,
        include.lowest = TRUE,
        labels         = head(brks, -1))
  }

  # Unit conversion for individual BV lengths (LH C) - Should ICES vocab used directly here?
  convertLength <- function(x, fromUnit, toUnit) {
    toMM <- switch(fromUnit,
      "mm"   = 1, "cm"   = 10, "25mm" = 25, "5cm"  = 50,
      stop("Unknown fromUnit: ", fromUnit))
    fromMM <- switch(toUnit,
      "mm"   = 1, "cm"   = 10, "25mm" = 25, "5cm"  = 50,
      stop("Unknown toUnit: ", toUnit))
    x * toMM / fromMM
  }

  # Derive individual BV weights from wcol or LW relationship
  addBVweight <- function(bv_wide, lengthVar = NULL) {
    if (!is.null(wcol) && wcol %in% names(bv_wide)) {
      bv_wide[, BVweight := as.numeric(bv_wide[[wcol]])]
    } else if (!is.null(LWparam) && !is.null(lengthVar) && lengthVar %in% names(bv_wide)) {
      bv_wide[, BVweight := LWparam[1] * as.numeric(bv_wide[[lengthVar]]) ^ LWparam[2]]
    } else {
      bv_wide[, BVweight := NA_real_]
    }
    bv_wide
  }

  # Apply SA-level raise factor
  applySARaise <- function(su, numbersInCol, numbersOutCol) {
    if (raiseVar == "Weight") {
      su[, raiseFactor := SAtotalWtMes / SAsampWtMes]
    } else if (raiseVar == "Count") {
      su[, raiseFactor := SAnumTotal / SAnumSamp]
    } else {
      su[, raiseFactor := as.numeric(SAauxVarValue)]
    }
    su[, (numbersOutCol) := get(numbersInCol) * raiseFactor]
    su
  }

  # SA columns used across branches
  saCols <- c("SAid", "SAlowHierarchy", "SAtotalWtMes", "SAsampWtMes",
              "SAnumTotal", "SAnumSamp", "SAauxVarValue", "SAauxVarUnit")


  # ===========================================================================
  # LENGTH COMPOSITION
  # ===========================================================================

  if (targetValue == "LengthComp") {

    # -------------------------------------------------------------------------
    # LH A: SA -> FM -> BV
    # FM holds the length-frequency. BV (if present) provides individual
    # weights for MeanWeightAtLength; otherwise LWparam is used.
    # -------------------------------------------------------------------------

    if (lh == "A") {

      warning("Lower hierarchy A: only the FM table is used to calculate numbers at length.")

      fm <- data.table::setDT(RDBESEstRatioObj$FM)
      sa <- data.table::setDT(RDBESEstRatioObj$SA)

      checkLC(fm = fm, classUnits = classUnits, classBreaks = classBreaks)

      fm <- fm[, unique(.SD), .SDcols = c("SAid", "FMid", "FMclassMeas",
                                           "FMnumAtUnit", "FMaccuracy", "FMtypeAssess")]
      sa <- sa[, unique(.SD), .SDcols = saCols]
      sa[, SAauxVarValue := as.numeric(SAauxVarValue)]

      fm[, LengthClass := assignLengthClass(FMclassMeas)]

      fm1 <- fm[
        , .(FMNumbersAtLength = sum(FMnumAtUnit, na.rm = TRUE)),
        by = .(SAid, LengthClass)
      ][, FMTotCount := sum(FMNumbersAtLength, na.rm = TRUE), by = SAid]

      su <- merge(fm1, sa, by = "SAid")
      su <- applySARaise(su,
                         numbersInCol  = "FMNumbersAtLength",
                         numbersOutCol = "NumbersAtLength")

      # Mean weight at length: from BV individual weights or LW relationship
      if (bv_present && !is.null(wcol)) {
        bv    <- data.table::setDT(RDBESEstRatioObj$BV)
        bv_w  <- bv[BVtypeMeas == wcol,
                    .(FMid, BVweight = as.numeric(BVvalueMeas))]
        bv_lc <- merge(bv_w, fm[, .(FMid, SAid, LengthClass)], by = "FMid")
        mean_w <- bv_lc[, .(MeanWeightAtLength = mean(BVweight, na.rm = TRUE)),
                        by = .(SAid, LengthClass)]
        su <- merge(su, mean_w, by = c("SAid", "LengthClass"), all.x = TRUE)
      } else if (!is.null(LWparam)) {
        fm_lw <- fm[, .(LengthClass, FMclassMeas)][, .SD[1], by = LengthClass]
        fm_lw[, MeanWeightAtLength := LWparam[1] * FMclassMeas ^ LWparam[2]]
        su <- merge(su, fm_lw[, .(LengthClass, MeanWeightAtLength)],
                    by = "LengthClass", all.x = TRUE)
      } else {
        if (verbose)
          message("No BV weights or LWparam supplied: MeanWeightAtLength set to NA.")
        su[, MeanWeightAtLength := NA_real_]
      }

      return(su)
    }

    # -------------------------------------------------------------------------
    # LH B: SA -> FM only
    # FM holds the length-frequency; no BV table exists.
    # MeanWeightAtLength can only be derived from LWparam.
    # -------------------------------------------------------------------------

    if (lh == "B") {

      fm <- data.table::setDT(RDBESEstRatioObj$FM)
      sa <- data.table::setDT(RDBESEstRatioObj$SA)

      checkLC(fm = fm, classUnits = classUnits, classBreaks = classBreaks)

      fm <- fm[, unique(.SD), .SDcols = c("SAid", "FMid", "FMclassMeas",
                                           "FMnumAtUnit", "FMaccuracy", "FMtypeAssess")]
      sa <- sa[, unique(.SD), .SDcols = saCols]
      sa[, SAauxVarValue := as.numeric(SAauxVarValue)]

      fm[, LengthClass := assignLengthClass(FMclassMeas)]

      fm1 <- fm[
        , .(FMNumbersAtLength = sum(FMnumAtUnit, na.rm = TRUE)),
        by = .(SAid, LengthClass)
      ][, FMTotCount := sum(FMNumbersAtLength, na.rm = TRUE), by = SAid]

      su <- merge(fm1, sa, by = "SAid")
      su <- applySARaise(su,
                         numbersInCol  = "FMNumbersAtLength",
                         numbersOutCol = "NumbersAtLength")

      # Mean weight at length: LWparam only (no BV in LH B)
      if (!is.null(LWparam)) {
        fm_lw <- fm[, .(LengthClass, FMclassMeas)][, .SD[1], by = LengthClass]
        fm_lw[, MeanWeightAtLength := LWparam[1] * FMclassMeas ^ LWparam[2]]
        su <- merge(su, fm_lw[, .(LengthClass, MeanWeightAtLength)],
                    by = "LengthClass", all.x = TRUE)
      } else {
        if (verbose)
          message("No LWparam supplied: MeanWeightAtLength set to NA.")
        su[, MeanWeightAtLength := NA_real_]
      }

      return(su)
    }

    # -------------------------------------------------------------------------
    # LH C: SA -> BV only
    # No FM table. Individual fish measurements link directly to SA.
    # -------------------------------------------------------------------------

    if (lh == "C") {

      if (!bv_present)
        stop("Lower hierarchy C requires a BV table.")

      bv <- data.table::setDT(RDBESEstRatioObj$BV)
      sa <- data.table::setDT(RDBESEstRatioObj$SA)
      sa <- sa[, unique(.SD), .SDcols = saCols]
      sa[, SAauxVarValue := as.numeric(SAauxVarValue)]

      lengthVar <- grep("(?i)length", unique(bv$BVtypeMeas), value = TRUE, perl = TRUE)
      if (length(unique(lengthVar)) > 1)
        stop("Multiple length types in BV; filter to one before calling.")

      bv <- bv[, unique(.SD), .SDcols = c("SAid", "BVfishId", "BVtypeMeas",
                                           "BVvalueMeas", "BVtypeAssess")]

      measTypes <- if (!is.null(wcol)) c(lengthVar, wcol) else lengthVar
      bv_wide <- bv[BVtypeMeas %in% measTypes,
                    data.table::dcast(.SD,
                                      SAid + BVfishId ~ BVtypeMeas,
                                      value.var = "BVvalueMeas", drop = TRUE)]

      bv_wide[, convertedLength := as.numeric(bv_wide[[lengthVar]])]
      bv_wide[, LengthClass     := assignLengthClass(convertedLength)]
      bv_wide <- addBVweight(bv_wide, lengthVar)

      bv1 <- bv_wide[
        , .(BVNumbersAtLength  = .N,
            MeanWeightAtLength = mean(BVweight, na.rm = TRUE)),
        by = .(SAid, LengthClass)
      ][
        , BVTotCount := sum(BVNumbersAtLength), by = SAid
      ][
        bv_wide[, .(BVTotWeight = sum(BVweight, na.rm = TRUE)), by = SAid],
        on = "SAid"
      ]

      su <- merge(bv1, sa, by = "SAid")
      su <- applySARaise(su,
                         numbersInCol  = "BVNumbersAtLength",
                         numbersOutCol = "NumbersAtLength")

      return(su)
    }
  }


  # ===========================================================================
  # AGE COMPOSITION
  # ===========================================================================

  if (targetValue == "AgeComp") {

    # LH B is caught earlier with a clear error message.

    # -------------------------------------------------------------------------
    # LH A: SA -> FM -> BV
    # FM length-frequency; BV individual ages stratified under FM
    # length classes. Ages are first raised within each FM length class
    # (BV subsample -> FM count), then the SA-level raise is applied.
    # -------------------------------------------------------------------------

    if (lh == "A") {

      if (!bv_present)
        stop("Lower hierarchy A age composition requires a BV table.")
      if (is.null(wcol))
        stop("A weight column in BV is required for age composition in lower hierarchy A.")

      bv <- data.table::setDT(RDBESEstRatioObj$BV)
      fm <- data.table::setDT(RDBESEstRatioObj$FM)
      sa <- data.table::setDT(RDBESEstRatioObj$SA)

      bv <- bv[, unique(.SD), .SDcols = c("FMid", "BVfishId", "BVtypeMeas", "BVvalueMeas")]
      bv <- data.table::dcast(bv, ... ~ BVtypeMeas, value.var = "BVvalueMeas", drop = TRUE)
      bv[, BVweight := as.numeric(bv[[wcol]])]

      if (!"Age" %in% names(bv))
        stop("No 'Age' measurement found in BV. Check BVtypeMeas values.")

      fm <- fm[, unique(.SD), .SDcols = c("SAid", "FMid", "FMclassMeas", "FMnumAtUnit")]
      sa <- sa[, unique(.SD), .SDcols = saCols]
      sa[, SAauxVarValue := as.numeric(SAauxVarValue)]

      fm_tot <- fm[, .(FMnumAtUnit = sum(FMnumAtUnit, na.rm = TRUE),
                       SAid        = SAid[1]),
                   by = FMid]

      bv1 <- bv[
        , .(BVMeanWeight   = mean(BVweight, na.rm = TRUE),
            BVNumbersAtAge = .N),
        by = .(FMid, Age)
      ][, BVTotCount := sum(BVNumbersAtAge), by = FMid]

      # Raise within FM length class: BV subsample -> FM tally
      bv2 <- merge(bv1, fm_tot, by = "FMid")
      bv2[, num_raise   := fifelse(BVTotCount > 0, FMnumAtUnit / BVTotCount, NA_real_)]
      bv2[, N_at_age_FM := BVNumbersAtAge * num_raise]

      # Mean length at age
      lengthVar <- grep("(?i)length", names(bv), value = TRUE, perl = TRUE)
      if (length(lengthVar) > 0) {
        lv1 <- lengthVar[1]
        mean_len <- bv[, .(MeanLengthAtAge = mean(as.numeric(bv[[lv1]]),
                                               na.rm = TRUE)),
                       by = .(FMid, Age)]
        bv2 <- merge(bv2, mean_len, by = c("FMid", "Age"), all.x = TRUE)
      } else {
        bv2[, MeanLengthAtAge := NA_real_]
      }

      # Aggregate from FM level to SA level
      bv3 <- bv2[
        , .(N_at_age_SA     = sum(N_at_age_FM, na.rm = TRUE),
            BVMeanWeight    = weighted.mean(BVMeanWeight,    BVNumbersAtAge, na.rm = TRUE),
            MeanLengthAtAge = weighted.mean(MeanLengthAtAge, BVNumbersAtAge, na.rm = TRUE)),
        by = .(SAid, Age)
      ]

      su <- merge(bv3, sa, by = "SAid")
      su <- applySARaise(su,
                         numbersInCol  = "N_at_age_SA",
                         numbersOutCol = "NumbersAtAge")

      return(su)
    }

    # -------------------------------------------------------------------------
    # LH C: SA -> BV only
    # No FM table. Individual fish measurements link directly to SA.
    # -------------------------------------------------------------------------

    if (lh == "C") {

      bv <- data.table::setDT(RDBESEstRatioObj$BV)
      sa <- data.table::setDT(RDBESEstRatioObj$SA)
      sa <- sa[, unique(.SD), .SDcols = saCols]
      sa[, SAauxVarValue := as.numeric(SAauxVarValue)]

      bv <- bv[, unique(.SD), .SDcols = c("SAid", "BVfishId", "BVtypeMeas", "BVvalueMeas")]
      bv <- data.table::dcast(bv, ... ~ BVtypeMeas, value.var = "BVvalueMeas", drop = TRUE)

      if (!"Age" %in% names(bv))
        stop("No 'Age' measurement found in BV. Check BVtypeMeas values.")

      # Resolve weight outside data.table to avoid get() scoping issues in j
      if (!is.null(wcol) && wcol %in% names(bv)) {
        bv[, BVweight := as.numeric(bv[[wcol]])]
      } else if (!is.null(LWparam)) {
        lengthVar_age <- grep("(?i)length", names(bv), value = TRUE, perl = TRUE)
        if (length(lengthVar_age) == 0)
          stop("LWparam supplied but no length column found in BV.")
        bv[, BVweight := LWparam[1] * as.numeric(bv[[lengthVar_age[1]]]) ^ LWparam[2]]
      } else {
        if (verbose) message("No weight column or LWparam supplied: BVweight set to NA.")
        bv[, BVweight := NA_real_]
      }

      bv1 <- bv[
        , .(BVMeanWeight   = mean(BVweight, na.rm = TRUE),
            BVNumbersAtAge = .N),
        by = .(SAid, Age)
      ][
        , BVTotCount := sum(BVNumbersAtAge), by = SAid
      ][
        bv[, .(BVTotWeight = sum(BVweight, na.rm = TRUE)), by = SAid],
        on = "SAid"
      ]

      # Mean length at age
      lengthVar <- grep("(?i)length", names(bv), value = TRUE, perl = TRUE)
      if (length(lengthVar) > 0) {
        lv1 <- lengthVar[1]
        mean_len <- bv[, .(MeanLengthAtAge = mean(as.numeric(bv[[lv1]]),
                                               na.rm = TRUE)),
                       by = .(SAid, Age)]
        bv1 <- merge(bv1, mean_len, by = c("SAid", "Age"), all.x = TRUE)
      } else {
        bv1[, MeanLengthAtAge := NA_real_]
      }

      su <- merge(bv1, sa, by = "SAid")
      su <- applySARaise(su,
                         numbersInCol  = "BVNumbersAtAge",
                         numbersOutCol = "NumbersAtAge")

      return(su)
    }
  }

  stop("Unrecognised combination of targetValue ('", targetValue,
       "') and lower hierarchy ('", lh, "').")
}
