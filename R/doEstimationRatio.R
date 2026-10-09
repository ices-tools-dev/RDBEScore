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
#'   \strong{Ignored when \code{marketProp} is supplied} (see Details), in which
#'   case the market-category ratio estimator is used instead.
#' @param marketProp Optional. The market-category (commercial size category)
#'   composition of the landings, used to raise the age/length composition the
#'   same way as the TYPE-Ia procedure (\code{RaiseNumbers}). This is the
#'   \code{prop_MC} vector in that procedure. Supply EITHER:
#'   \itemize{
#'     \item a named numeric vector, names = market categories matching
#'       \code{SAcommCat}, values = proportion of the landed weight in each
#'       category (e.g. \code{c("1"=0.20, "2"=0.34, "3"=0.34, "4"=0.12)}); or
#'     \item a two-column \code{data.frame}/\code{data.table} with a category
#'       column (\code{marketPropCatCol}) and a proportion column
#'       (\code{marketPropValCol}).
#'   }
#'   Proportions are used together with
#'   \code{totalLandings} to obtain the landed weight per category as
#'   \eqn{W_c = totalLandings \times prop_c} (exactly \code{Rtot_landing *
#'   prop_MC}). When \code{NULL} (default) the function falls back to the
#'   per-sample SA raise factor controlled by \code{raiseVar}.
#' @param totalLandings Optional. A single number: the TOTAL landed weight of
#'   the species in this estimation stratum (area * species * season * metier),
#'   e.g. the summed CL official weight (\code{CLoffWeight}) for that stratum,
#'   or InterCatch CATON. Required when \code{marketProp} is supplied (the
#'   proportions are multiplied by it to give the per-category weights).
#' @param landingsWtUnit Unit of \code{totalLandings}: \code{"kg"} or
#'   \code{"g"}. BV individual weights are assumed to be grams (RDBES
#'   \code{Weightg}); the landed weight is converted to grams internally so
#'   that \code{numbers = W_c / meanWeight_c} is dimensionally consistent.
#'   Default \code{"kg"}.
#' @param marketPropCatCol,marketPropValCol Column names used only when
#'   \code{marketProp} is supplied as a data.frame. Defaults
#'   \code{"commSizeCat"} and \code{"prop"}.
#' @param plusGroup Optional integer age. Ages greater than or equal to this
#'   value are collapsed into a single plus group (labelled as the plus-group
#'   age) before raising. Used only for \code{targetValue = "AgeComp"}.
#'   \code{NULL} (default) keeps ages as reported.
#' @param classUnits Units of the length class intervals, e.g. \code{"mm"} or
#'   \code{"cm"}. Used only for \code{targetValue = "LengthComp"}.
#'   Codes: \url{https://vocab.ices.dk/?ref=1608}
#' @param classBreaks A numeric vector of three values:
#'   \code{c(min, max, width)}. Defines the length class intervals. Used only
#'   for \code{targetValue = "LengthComp"}.
#' @param LWparam A numeric vector of length two \code{c(a, b)} specifying the
#'   weight-length relationship (W = a * L^b). Used when individual weights are
#'   absent from BV but lengths are available.
#' @param aggregate Logical. If \code{TRUE} (default), the per-sample raised
#'   numbers are summed over samples to give one estimate per class for the
#'   whole (filtered) data set, and the mean weight/length per class is the
#'   numbers-weighted mean across samples. If \code{FALSE}, the function returns
#'   the un-aggregated per-sample table (one row per SAid * class), which is the
#'   original behaviour and is useful for diagnostics. Ignored in the
#'   market-category ratio mode (\code{marketProp} supplied), which always
#'   aggregates across categories.
#' @param verbose Logical; if \code{TRUE}, informational messages are printed
#'   during processing.
#'
#' @return A \code{data.table} with estimated numbers at length or age and
#'   associated mean values.
#'   With \code{aggregate = TRUE} (or in market-category mode) there is one row
#'   per class:
#'   \itemize{
#'     \item \code{LengthClass} or \code{Age} — the grouping variable
#'     \item \code{NumbersAtLength} or \code{NumbersAtAge} — raised estimates,
#'       summed over samples
#'     \item \code{MeanWeightAtLength} or \code{MeanWeightAtAge} — numbers-
#'       weighted mean weight
#'     \item \code{MeanLengthAtAge} — numbers-weighted mean length at age
#'       (AgeComp only, when a length is available)
#'   }
#'   With \code{aggregate = FALSE} the result is one row per \code{SAid} * class
#'   and additionally carries \code{SAid}, the per-sample raised numbers, and
#'   \code{raiseFactor} (the SA-level raise factor applied).
#'
#' @details
#' \strong{Two raising modes.}
#' \enumerate{
#'   \item \emph{Per-sample SA raise} (\code{marketProp = NULL}): each SA record
#'     is raised by \code{raiseVar} (\code{SAtotalWtMes/SAsampWtMes},
#'     \code{SAnumTotal/SAnumSamp}, or \code{SAauxVarValue}). This is the
#'     original RDBEScore behaviour and is self-contained within the sampled
#'     fraction.
#'   \item \emph{Market-category ratio estimator} (\code{marketProp} +
#'     \code{totalLandings} supplied): reproduces the IMARES TYPE-Ia raising
#'     (\code{RaiseNumbers}). The landed weight per category is first obtained
#'     as \eqn{W_c = totalLandings \times prop_c} (the \code{Rtot_landing *
#'     prop_MC} step). Sampled fish are stratified by commercial size category
#'     \code{SAcommCat}. For each category \eqn{c}:
#'     \deqn{N_c = W_c / \bar{w}_c}
#'     where \eqn{\bar{w}_c} is the mean individual fish weight in that category
#'     (from BV). The age proportions within a category
#'     \eqn{P_{a,c} = n_{a,c}/n_c} are then scaled to the population:
#'     \deqn{N_a = \sum_c P_{a,c}\, N_c.}
#'     Mean weight at age is the across-category, numbers-weighted mean
#'     (\code{tab_PB_age_c} in the original code). This matches the existing
#'     database-driven function and is the recommended mode for the NL auction
#'     (market-category) sampling design.
#' }
#'
#' \strong{Expected inputs} (one stratum at a time; filter the object to a
#' single quarter * area * metier first, exactly as the TYPE-Ia loop does):
#' \preformatted{
#'   marketProp    = c("1" = 0.20, "2" = 0.34, "3" = 0.34, "4" = 0.12)
#'   totalLandings = 62650          # kg of PLE landed in this stratum
#' }
#' which is equivalent to passing the per-category weights
#' \code{totalLandings * marketProp} directly.
#'
#' \strong{Weights are used as-is}: no gutted->whole or presentation/state
#' conversion is applied. Ensure the BV weights and the \code{totalLandings}
#' refer to the same presentation/state of processing before calling.
#'
#' The three lower hierarchies differ in which tables are present:
#' \itemize{
#'   \item \strong{LH A}: \code{SA -> FM -> BV}.
#'   \item \strong{LH B}: \code{SA -> FM} only (no BV; AgeComp not possible).
#'   \item \strong{LH C}: \code{SA -> BV} only.
#' }
#'
#' @importFrom utils tail head menu
#' @export
doEstimationRatio <- function(RDBESDataObj,
                              targetValue      = "LengthComp",
                              raiseVar         = "Weight",
                              marketProp       = NULL,
                              totalLandings    = NULL,
                              landingsWtUnit   = "kg",
                              marketPropCatCol = "commSizeCat",
                              marketPropValCol = "prop",
                              plusGroup        = NULL,
                              classUnits       = "mm",
                              classBreaks      = c(100, 300, 10),
                              LWparam          = NULL,
                              aggregate        = TRUE,
                              verbose          = FALSE) {

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

  useLandings <- !is.null(marketProp)

  Wc <- NULL  # per-category landed weight in grams, built below

  if (useLandings) {

    if (!landingsWtUnit %in% c("kg", "g"))
      stop("landingsWtUnit must be 'kg' or 'g'.")

    # Normalise marketProp to a data.table: commSizeCat (char), prop (numeric)
    if (is.data.frame(marketProp)) {
      marketProp <- data.table::as.data.table(marketProp)
      if (!marketPropCatCol %in% names(marketProp))
        stop("marketProp is missing the category column '", marketPropCatCol, "'.")
      if (!marketPropValCol %in% names(marketProp))
        stop("marketProp is missing the proportion column '", marketPropValCol, "'.")
      mp <- data.table::data.table(
        commSizeCat = as.character(marketProp[[marketPropCatCol]]),
        prop        = as.numeric(marketProp[[marketPropValCol]]))
    } else if (is.numeric(marketProp)) {
      if (is.null(names(marketProp)))
        stop("When marketProp is a numeric vector it must be named by market ",
             "category (e.g. c(\"1\" = 0.2, \"2\" = 0.34, ...)).")
      mp <- data.table::data.table(
        commSizeCat = as.character(names(marketProp)),
        prop        = as.numeric(marketProp))
    } else {
      stop("marketProp must be a named numeric vector or a data.frame.")
    }

    # marketProp holds the market-category PROPORTIONS (supplied externally),
    # totalLandings is the per-stratum total landed weight (e.g. summed
    # CLoffWeight over area * species * season * metier). The per-category
    # landed weight is W_c = totalLandings * prop_c (IMARES Rtot_landing * prop_MC).
    if (is.null(totalLandings) || length(totalLandings) != 1 ||
        !is.finite(totalLandings))
      stop("totalLandings must be a single finite number giving the total ",
           "landed weight of the species in this stratum (e.g. summed CL ",
           "official weight for the area * species * season * metier).")

    sp <- sum(mp$prop, na.rm = TRUE)
    if (abs(sp - 1) > 0.01)
      warning("marketProp values sum to ", round(sp, 4),
              " (expected ~1). Used as supplied: W_c = totalLandings * marketProp.")

    totG <- if (landingsWtUnit == "kg") totalLandings * 1000 else totalLandings
    mp[, W_c_g := totG * prop]
    Wc <- mp[, .(commSizeCat, W_c_g)]

    if (verbose)
      message("Market-category ratio estimator: W_c = totalLandings * marketProp ",
              "per size category (raiseVar is ignored).")
  }

  # Warn if length-class arguments are passed but irrelevant
  if (targetValue == "AgeComp") {
    if (!missing(classUnits))
      warning("'classUnits' is ignored when targetValue = 'AgeComp'.")
    if (!missing(classBreaks))
      warning("'classBreaks' is ignored when targetValue = 'AgeComp'.")
  }

  if (targetValue == "AgeComp" && lh == "B")
    stop("AgeComp is not possible with lower hierarchy B: LH B has no individual ",
         "fish records (no BV table). Use LH A or LH C for age composition.")

  # ---------------------------------------------------------------------------
  # Get the appropriate weight for raising (BV individual weight)
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
  # Helpers
  # ---------------------------------------------------------------------------

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
    cut(lengths, breaks = brks, right = FALSE,
        include.lowest = TRUE, labels = utils::head(brks, -1))
  }

  convertLength <- function(x, fromUnit, toUnit) {
    toMM <- switch(fromUnit, "mm" = 1, "cm" = 10, "25mm" = 25, "5cm" = 50,
                   stop("Unknown fromUnit: ", fromUnit))
    fromMM <- switch(toUnit, "mm" = 1, "cm" = 10, "25mm" = 25, "5cm" = 50,
                     stop("Unknown toUnit: ", toUnit))
    x * toMM / fromMM
  }

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

  # Per-sample SA raise factor (original RDBEScore behaviour)
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

  # Aggregate per-sample raised results to the (filtered) data set:
  #   numbers     -> summed over samples
  #   mean weight -> numbers-weighted mean over samples
  #   mean length -> numbers-weighted mean over samples (AgeComp)
  # `su` is one row per SAid * class. `classCol` is "LengthClass" or "Age";
  # `numbersCol` is "NumbersAtLength" or "NumbersAtAge".
  aggregateOverSamples <- function(su, classCol, numbersCol,
                                   meanWeightCol = NULL, meanLengthCol = NULL) {
    if (!aggregate) return(su[])
    su <- data.table::copy(su[!is.na(get(classCol))])
    su[, .wts__ := get(numbersCol)]
    out <- su[, .(NumbersTmp = sum(get(numbersCol), na.rm = TRUE)),
              by = c(classCol)]
    data.table::setnames(out, "NumbersTmp", numbersCol)
    addWeightedMean <- function(out, valueCol) {
      if (is.null(valueCol) || !valueCol %in% names(su)) return(out)
      wm <- su[, .(.v = stats::weighted.mean(get(valueCol), .wts__, na.rm = TRUE)),
               by = c(classCol)]
      data.table::setnames(wm, ".v", valueCol)
      merge(out, wm, by = classCol, all.x = TRUE)
    }
    out <- addWeightedMean(out, meanWeightCol)
    out <- addWeightedMean(out, meanLengthCol)
    data.table::setorderv(out, classCol)
    out[]
  }

  # Commercial size category column. The authoritative RDBES R name (per the
  # RDBES CS data model) is SAcommCat (scale: SAcommCatScl). Older/other
  # versions have used SAcommSizeCat. We resolve to whichever is present and
  # copy it to a canonical SAcommCat column used downstream.
  saNames <- names(RDBESEstRatioObj$SA)
  if (!"SAcommCat" %in% saNames) {
    catAliases <- c("SAcommCat", "SAcommSizeCat", "SAcommSizeCategory",
                    "SAcommercialSizeCategory")
    hit <- intersect(catAliases, saNames)
    if (length(hit) == 0) {
      cand <- grep("commcat|comm.*siz.*cat|commercialsizecategory",
                   saNames, ignore.case = TRUE, value = TRUE)
      hit  <- cand[!grepl("scale|scl", cand, ignore.case = TRUE)]
    }
    if (useLandings && length(hit) == 0)
      stop("Could not find a commercial size category column in SA. Looked for ",
           paste(catAliases, collapse = ", "),
           ". Available SA columns: ", paste(saNames, collapse = ", "))
    if (length(hit) >= 1)
      data.table::setnames(RDBESEstRatioObj$SA, hit[1], "SAcommCat")
  }

  # SA columns used across branches; keep only those that actually exist so
  # version differences in optional columns don't break the .SDcols selection.
  saColsWanted <- c("SAid", "SAlowHierarchy", "SAtotalWtMes", "SAsampWtMes",
                    "SAnumTotal", "SAnumSamp", "SAauxVarValue", "SAauxVarUnit",
                    "SAcommCat")
  saCols <- intersect(saColsWanted, names(RDBESEstRatioObj$SA))

  # ===========================================================================
  # MARKET-CATEGORY RATIO ESTIMATOR (TYPE-Ia / RaiseNumbers)
  # Used whenever `landings` is supplied.
  # ===========================================================================

  if (useLandings) {

    sa <- data.table::setDT(RDBESEstRatioObj$SA)
    sa <- sa[, unique(.SD), .SDcols = saCols]
    sa[, SAcommCat := as.character(SAcommCat)]

    if (targetValue == "AgeComp") {

      if (!bv_present)
        stop("AgeComp with landings requires a BV table (LH A or LH C).")

      bv <- data.table::setDT(RDBESEstRatioObj$BV)
      bv <- bv[, unique(.SD), .SDcols = c("SAid", "BVfishId", "BVtypeMeas", "BVvalueMeas")]
      bv <- data.table::dcast(bv, SAid + BVfishId ~ BVtypeMeas,
                              value.var = "BVvalueMeas", drop = TRUE)

      if (!"Age" %in% names(bv))
        stop("No 'Age' measurement found in BV. Check BVtypeMeas values.")

      fish <- merge(bv, sa[, .(SAid, commSizeCat = SAcommCat)], by = "SAid")
      fish[, age := as.numeric(Age)]

      # individual weight (used as-is, no presentation conversion)
      if (!is.null(wcol) && wcol %in% names(fish)) {
        fish[, BVweight := as.numeric(fish[[wcol]])]
      } else if (!is.null(LWparam)) {
        lengthVar <- grep("(?i)length", names(fish), value = TRUE, perl = TRUE)
        if (length(lengthVar) == 0)
          stop("LWparam supplied but no length column found in BV.")
        fish[, BVweight := LWparam[1] * as.numeric(fish[[lengthVar[1]]]) ^ LWparam[2]]
      } else {
        stop("AgeComp ratio raising needs an individual weight: a BV weight ",
             "type or LWparam. None found.")
      }

      classCol <- "age"

    } else {  # LengthComp ratio estimator

      if (lh == "C") {
        bv <- data.table::setDT(RDBESEstRatioObj$BV)
        lengthVar <- grep("(?i)length", unique(bv$BVtypeMeas), value = TRUE, perl = TRUE)
        if (length(lengthVar) == 0)
          stop("LengthComp with landings (LH C) needs a length in BV.")
        if (length(unique(lengthVar)) > 1)
          stop("Multiple length types in BV; filter to one before calling.")
        bv <- bv[, unique(.SD), .SDcols = c("SAid", "BVfishId", "BVtypeMeas", "BVvalueMeas")]
        bv <- data.table::dcast(bv, SAid + BVfishId ~ BVtypeMeas,
                                value.var = "BVvalueMeas", drop = TRUE)
        bv[, convLen := convertLength(as.numeric(bv[[lengthVar]]),
                                      fromUnit = classUnits, toUnit = "mm")]
        bv[, LengthClass := assignLengthClass(convLen)]
        fish <- merge(bv, sa[, .(SAid, commSizeCat = SAcommCat)], by = "SAid")
        if (!is.null(wcol) && wcol %in% names(fish))
          fish[, BVweight := as.numeric(fish[[wcol]])]
        else
          fish[, BVweight := NA_real_]
      } else {
        # LH A / B: FM length-frequency raised per category by W_c / mean wt
        fm <- data.table::setDT(RDBESEstRatioObj$FM)
        checkLC(fm = fm, classUnits = classUnits, classBreaks = classBreaks)
        fm <- fm[, unique(.SD), .SDcols = c("SAid", "FMid", "FMclassMeas", "FMnumAtUnit")]
        fm[, LengthClass := assignLengthClass(FMclassMeas)]
        fm <- merge(fm, sa[, .(SAid, commSizeCat = SAcommCat)], by = "SAid")
        fish <- fm[, .(commSizeCat, LengthClass, n = FMnumAtUnit)]
        if (!is.null(LWparam))
          fish[, BVweight := LWparam[1] * (as.numeric(as.character(LengthClass))) ^ LWparam[2]]
        else
          fish[, BVweight := NA_real_]
      }
      classCol <- "LengthClass"
    }

    # ----- Generic market-category ratio estimator on `fish` -----------------
    if (!"n" %in% names(fish)) fish[, n := 1]

    # mean individual weight per category  (\bar{w}_c) and sampled n
    wbar_c <- fish[, .(meanWeight_c = stats::weighted.mean(BVweight, n, na.rm = TRUE),
                       nFish_c = sum(n)),
                   by = commSizeCat]

    # total numbers per category  N_c = W_c / \bar{w}_c
    Nc <- merge(wbar_c, Wc, by = "commSizeCat", all.x = TRUE)
    missingW <- Nc[is.na(W_c_g), unique(commSizeCat)]
    if (length(missingW) > 0)
      warning("No landed weight supplied for size categor(y/ies): ",
              paste(missingW, collapse = ", "),
              ". These categories contribute 0 to the raised numbers.")
    Nc[, Numb_c := W_c_g / meanWeight_c]

    # within-category counts, proportions and raised numbers by class
    nac <- fish[, .(n_ac = sum(n)), by = c("commSizeCat", classCol)]
    nac <- merge(nac, Nc[, .(commSizeCat, nFish_c, Numb_c)], by = "commSizeCat")
    nac[, P_ac := n_ac / nFish_c]
    nac[, N_ac := P_ac * Numb_c]

    # numbers at class (summed across categories)
    numbers <- nac[, .(Numbers = sum(N_ac, na.rm = TRUE)), by = classCol]

    # population proportions per category, for numbers-weighted means
    nac[, PB_ac := N_ac / sum(N_ac, na.rm = TRUE), by = classCol]

    if (targetValue == "AgeComp") {

      mwac <- fish[, .(meanW_ac = mean(BVweight, na.rm = TRUE)),
                   by = c("commSizeCat", classCol)]
      mwac <- merge(mwac, nac[, c("commSizeCat", classCol, "PB_ac"), with = FALSE],
                    by = c("commSizeCat", classCol))
      meanW <- mwac[, .(MeanWeightAtAge = sum(meanW_ac * PB_ac, na.rm = TRUE)),
                    by = classCol]

      out <- merge(numbers, meanW, by = classCol, all.x = TRUE)
      data.table::setnames(out, c(classCol, "Numbers"), c("Age", "NumbersAtAge"))

      # mean length at age, if a length exists in BV
      bvlen <- grep("(?i)length", names(fish), value = TRUE, perl = TRUE)
      if (length(bvlen) > 0) {
        lv1 <- bvlen[1]
        mlac <- fish[, .(meanL_ac = mean(as.numeric(fish[[lv1]]), na.rm = TRUE)),
                     by = c("commSizeCat", "age")]
        mlac <- merge(mlac, nac[, .(commSizeCat, age, PB_ac)],
                      by = c("commSizeCat", "age"))
        meanL <- mlac[, .(MeanLengthAtAge = sum(meanL_ac * PB_ac, na.rm = TRUE)),
                      by = age]
        data.table::setnames(meanL, "age", "Age")
        out <- merge(out, meanL, by = "Age", all.x = TRUE)
      }

      # optional plus group (collapse after raising, numbers-weighted means)
      if (!is.null(plusGroup)) {
        out[, Age := pmin(Age, plusGroup)]
        aggCols <- list(NumbersAtAge    = quote(sum(NumbersAtAge, na.rm = TRUE)),
                        MeanWeightAtAge = quote(stats::weighted.mean(MeanWeightAtAge,
                                                            NumbersAtAge, na.rm = TRUE)))
        if ("MeanLengthAtAge" %in% names(out))
          out <- out[, .(NumbersAtAge    = sum(NumbersAtAge, na.rm = TRUE),
                         MeanWeightAtAge = stats::weighted.mean(MeanWeightAtAge,
                                                       NumbersAtAge, na.rm = TRUE),
                         MeanLengthAtAge = stats::weighted.mean(MeanLengthAtAge,
                                                       NumbersAtAge, na.rm = TRUE)),
                     by = Age]
        else
          out <- out[, .(NumbersAtAge    = sum(NumbersAtAge, na.rm = TRUE),
                         MeanWeightAtAge = stats::weighted.mean(MeanWeightAtAge,
                                                       NumbersAtAge, na.rm = TRUE)),
                     by = Age]
      }

      data.table::setorder(out, Age)
      return(out[])

    } else {  # LengthComp
      mwac <- fish[, .(meanW_ac = mean(BVweight, na.rm = TRUE)),
                   by = c("commSizeCat", classCol)]
      mwac <- merge(mwac, nac[, c("commSizeCat", classCol, "PB_ac"), with = FALSE],
                    by = c("commSizeCat", classCol))
      meanW <- mwac[, .(MeanWeightAtLength = sum(meanW_ac * PB_ac, na.rm = TRUE)),
                    by = classCol]
      out <- merge(numbers, meanW, by = classCol, all.x = TRUE)
      data.table::setnames(out, c(classCol, "Numbers"),
                           c("LengthClass", "NumbersAtLength"))
      return(out[])
    }
  }

  # ===========================================================================
  # ORIGINAL PER-SAMPLE SA RAISE  (landings = NULL)
  # ===========================================================================

  # ---------------------------------------------------------------------------
  # LENGTH COMPOSITION
  # ---------------------------------------------------------------------------

  if (targetValue == "LengthComp") {

    if (lh == "A") {

      warning("Lower hierarchy A: only the FM table is used to calculate numbers at length.")

      fm <- data.table::setDT(RDBESEstRatioObj$FM)
      sa <- data.table::setDT(RDBESEstRatioObj$SA)
      checkLC(fm = fm, classUnits = classUnits, classBreaks = classBreaks)
      fm <- fm[, unique(.SD), .SDcols = c("SAid", "FMid", "FMclassMeas",
                                           "FMnumAtUnit", "FMaccuracy", "FMtypeAssess")]
      sa <- sa[, unique(.SD), .SDcols = saCols]
      if ("SAauxVarValue" %in% names(sa)) sa[, SAauxVarValue := as.numeric(SAauxVarValue)]
      fm[, LengthClass := assignLengthClass(FMclassMeas)]
      fm1 <- fm[, .(FMNumbersAtLength = sum(FMnumAtUnit, na.rm = TRUE)),
                by = .(SAid, LengthClass)
               ][, FMTotCount := sum(FMNumbersAtLength, na.rm = TRUE), by = SAid]
      su <- merge(fm1, sa, by = "SAid")
      su <- applySARaise(su, "FMNumbersAtLength", "NumbersAtLength")

      if (bv_present && !is.null(wcol)) {
        bv    <- data.table::setDT(RDBESEstRatioObj$BV)
        bv_w  <- bv[BVtypeMeas == wcol, .(FMid, BVweight = as.numeric(BVvalueMeas))]
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
      return(aggregateOverSamples(su, "LengthClass", "NumbersAtLength",
                                   meanWeightCol = "MeanWeightAtLength"))
    }

    if (lh == "B") {

      fm <- data.table::setDT(RDBESEstRatioObj$FM)
      sa <- data.table::setDT(RDBESEstRatioObj$SA)
      checkLC(fm = fm, classUnits = classUnits, classBreaks = classBreaks)
      fm <- fm[, unique(.SD), .SDcols = c("SAid", "FMid", "FMclassMeas",
                                           "FMnumAtUnit", "FMaccuracy", "FMtypeAssess")]
      sa <- sa[, unique(.SD), .SDcols = saCols]
      if ("SAauxVarValue" %in% names(sa)) sa[, SAauxVarValue := as.numeric(SAauxVarValue)]
      fm[, LengthClass := assignLengthClass(FMclassMeas)]
      fm1 <- fm[, .(FMNumbersAtLength = sum(FMnumAtUnit, na.rm = TRUE)),
                by = .(SAid, LengthClass)
               ][, FMTotCount := sum(FMNumbersAtLength, na.rm = TRUE), by = SAid]
      su <- merge(fm1, sa, by = "SAid")
      su <- applySARaise(su, "FMNumbersAtLength", "NumbersAtLength")

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
      return(aggregateOverSamples(su, "LengthClass", "NumbersAtLength",
                                   meanWeightCol = "MeanWeightAtLength"))
    }

    if (lh == "C") {

      if (!bv_present)
        stop("Lower hierarchy C requires a BV table.")

      bv <- data.table::setDT(RDBESEstRatioObj$BV)
      sa <- data.table::setDT(RDBESEstRatioObj$SA)
      sa <- sa[, unique(.SD), .SDcols = saCols]
      if ("SAauxVarValue" %in% names(sa)) sa[, SAauxVarValue := as.numeric(SAauxVarValue)]

      lengthVar <- grep("(?i)length", unique(bv$BVtypeMeas), value = TRUE, perl = TRUE)
      if (length(unique(lengthVar)) > 1)
        stop("Multiple length types in BV; filter to one before calling.")

      bv <- bv[, unique(.SD), .SDcols = c("SAid", "BVfishId", "BVtypeMeas",
                                           "BVvalueMeas", "BVtypeAssess")]
      measTypes <- if (!is.null(wcol)) c(lengthVar, wcol) else lengthVar
      bv_wide <- bv[BVtypeMeas %in% measTypes,
                    data.table::dcast(.SD, SAid + BVfishId ~ BVtypeMeas,
                                      value.var = "BVvalueMeas", drop = TRUE)]
      bv_wide[, convertedLength := as.numeric(bv_wide[[lengthVar]])]
      bv_wide[, LengthClass     := assignLengthClass(convertedLength)]
      bv_wide <- addBVweight(bv_wide, lengthVar)

      bv1 <- bv_wide[, .(BVNumbersAtLength  = .N,
                         MeanWeightAtLength = mean(BVweight, na.rm = TRUE)),
                     by = .(SAid, LengthClass)
                    ][, BVTotCount := sum(BVNumbersAtLength), by = SAid
                    ][ bv_wide[, .(BVTotWeight = sum(BVweight, na.rm = TRUE)),
                               by = SAid], on = "SAid" ]
      su <- merge(bv1, sa, by = "SAid")
      su <- applySARaise(su, "BVNumbersAtLength", "NumbersAtLength")
      return(aggregateOverSamples(su, "LengthClass", "NumbersAtLength",
                                   meanWeightCol = "MeanWeightAtLength"))
    }
  }

  # ---------------------------------------------------------------------------
  # AGE COMPOSITION
  # ---------------------------------------------------------------------------

  if (targetValue == "AgeComp") {

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
      if ("SAauxVarValue" %in% names(sa)) sa[, SAauxVarValue := as.numeric(SAauxVarValue)]
      fm_tot <- fm[, .(FMnumAtUnit = sum(FMnumAtUnit, na.rm = TRUE),
                       SAid = SAid[1]), by = FMid]

      bv1 <- bv[, .(BVMeanWeight = mean(BVweight, na.rm = TRUE),
                    BVNumbersAtAge = .N), by = .(FMid, Age)
               ][, BVTotCount := sum(BVNumbersAtAge), by = FMid]
      bv2 <- merge(bv1, fm_tot, by = "FMid")
      bv2[, num_raise   := data.table::fifelse(BVTotCount > 0, FMnumAtUnit / BVTotCount, NA_real_)]
      bv2[, N_at_age_FM := BVNumbersAtAge * num_raise]

      lengthVar <- grep("(?i)length", names(bv), value = TRUE, perl = TRUE)
      if (length(lengthVar) > 0) {
        lv1 <- lengthVar[1]
        mean_len <- bv[, .(MeanLengthAtAge = mean(as.numeric(bv[[lv1]]), na.rm = TRUE)),
                       by = .(FMid, Age)]
        bv2 <- merge(bv2, mean_len, by = c("FMid", "Age"), all.x = TRUE)
      } else {
        bv2[, MeanLengthAtAge := NA_real_]
      }

      bv3 <- bv2[, .(N_at_age_SA = sum(N_at_age_FM, na.rm = TRUE),
                     BVMeanWeight = stats::weighted.mean(BVMeanWeight, BVNumbersAtAge, na.rm = TRUE),
                     MeanLengthAtAge = stats::weighted.mean(MeanLengthAtAge, BVNumbersAtAge, na.rm = TRUE)),
                 by = .(SAid, Age)]
      su <- merge(bv3, sa, by = "SAid")
      su <- applySARaise(su, "N_at_age_SA", "NumbersAtAge")
      data.table::setnames(su, "BVMeanWeight", "MeanWeightAtAge")
      su[, Age := as.numeric(Age)]
      if (!is.null(plusGroup)) {
        su[, Age := pmin(Age, plusGroup)]
        # collapse ages merged into the plus group within each sample so the
        # un-aggregated (aggregate = FALSE) table has unique SAid * Age rows
        if (!aggregate)
          su <- su[, .(NumbersAtAge    = sum(NumbersAtAge, na.rm = TRUE),
                       MeanWeightAtAge = stats::weighted.mean(MeanWeightAtAge,
                                            NumbersAtAge, na.rm = TRUE),
                       MeanLengthAtAge = stats::weighted.mean(MeanLengthAtAge,
                                            NumbersAtAge, na.rm = TRUE)),
                   by = .(SAid, Age)]
      }
      return(aggregateOverSamples(su, "Age", "NumbersAtAge",
                                   meanWeightCol = "MeanWeightAtAge",
                                   meanLengthCol = "MeanLengthAtAge"))
    }

    if (lh == "C") {

      bv <- data.table::setDT(RDBESEstRatioObj$BV)
      sa <- data.table::setDT(RDBESEstRatioObj$SA)
      sa <- sa[, unique(.SD), .SDcols = saCols]
      if ("SAauxVarValue" %in% names(sa)) sa[, SAauxVarValue := as.numeric(SAauxVarValue)]

      bv <- bv[, unique(.SD), .SDcols = c("SAid", "BVfishId", "BVtypeMeas", "BVvalueMeas")]
      bv <- data.table::dcast(bv, ... ~ BVtypeMeas, value.var = "BVvalueMeas", drop = TRUE)
      if (!"Age" %in% names(bv))
        stop("No 'Age' measurement found in BV. Check BVtypeMeas values.")

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

      bv1 <- bv[, .(BVMeanWeight = mean(BVweight, na.rm = TRUE),
                    BVNumbersAtAge = .N), by = .(SAid, Age)
               ][, BVTotCount := sum(BVNumbersAtAge), by = SAid
               ][ bv[, .(BVTotWeight = sum(BVweight, na.rm = TRUE)), by = SAid],
                  on = "SAid" ]

      lengthVar <- grep("(?i)length", names(bv), value = TRUE, perl = TRUE)
      if (length(lengthVar) > 0) {
        lv1 <- lengthVar[1]
        mean_len <- bv[, .(MeanLengthAtAge = mean(as.numeric(bv[[lv1]]), na.rm = TRUE)),
                       by = .(SAid, Age)]
        bv1 <- merge(bv1, mean_len, by = c("SAid", "Age"), all.x = TRUE)
      } else {
        bv1[, MeanLengthAtAge := NA_real_]
      }

      su <- merge(bv1, sa, by = "SAid")
      su <- applySARaise(su, "BVNumbersAtAge", "NumbersAtAge")
      data.table::setnames(su, "BVMeanWeight", "MeanWeightAtAge")
      su[, Age := as.numeric(Age)]
      if (!is.null(plusGroup)) {
        su[, Age := pmin(Age, plusGroup)]
        # collapse ages merged into the plus group within each sample so the
        # un-aggregated (aggregate = FALSE) table has unique SAid * Age rows
        if (!aggregate)
          su <- su[, .(NumbersAtAge    = sum(NumbersAtAge, na.rm = TRUE),
                       MeanWeightAtAge = stats::weighted.mean(MeanWeightAtAge,
                                            NumbersAtAge, na.rm = TRUE),
                       MeanLengthAtAge = stats::weighted.mean(MeanLengthAtAge,
                                            NumbersAtAge, na.rm = TRUE)),
                   by = .(SAid, Age)]
      }
      return(aggregateOverSamples(su, "Age", "NumbersAtAge",
                                   meanWeightCol = "MeanWeightAtAge",
                                   meanLengthCol = "MeanLengthAtAge"))
    }
  }

  stop("Unrecognised combination of targetValue ('", targetValue,
       "') and lower hierarchy ('", lh, "').")
}
