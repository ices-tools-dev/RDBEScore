#' Expand the measurements of each sample to the catch fraction it represents
#'
#' Second step of the ratio estimation workflow (see [doRatioEstimation()]).
#' For every included lowest-level SA from [getSampleUnits()] the length or age
#' composition measured in FM and BV is expanded to the catch fraction of the
#' top-level SA, using the sample's `fSample`. Nothing is raised to the
#' population yet.
#'
#' **By lower hierarchy**
#' * **A** (FM + length-stratified BV). Length: FM counts. Age: the sample's
#'   own age-length key (ALK), \eqn{y_a = f \sum_l n_l m_{la} / m_l}. Length
#'   classes without aged fish are lost and logged, unless
#'   `ageRoute = "sampleFillPooled"`, which fills them from an ALK pooled over
#'   all included samples. In the pooled key each aged fish is weighted by the
#'   number of fish it represents (\eqn{f n_l / m_l}), not equally.
#' * **B** (FM only). Length: FM counts. Age: only with `externalALK`,
#'   otherwise the samples are excluded and logged.
#' * **C** (BV, all fish). Length and age directly from BV. Fish missing the
#'   target measurement are assumed missing at random within their sample.
#'
#' **Weights.** Individual live weight is taken from BV `WeightLive`, else
#' `WeightMeasured * SAconFacMeasLive`, else `WeightGutted * gutToLive`. BV
#' weights only gutted and no `gutToLive` stops the function, unless
#' `useWeights = FALSE`.
#'
#' **Length classes.** FM lengths are in mm (`FMclassMeas`) with class width
#' from `FMaccuracy`. `classWidth` (mm) must be a multiple of every FM
#' accuracy. Class labels are the lower bound in mm.
#'
#' @param RDBESDataObj The `RDBESDataObject` used in [getSampleUnits()].
#' @param sampleUnits Output of [getSampleUnits()].
#' @param target `"Age"` or `"Length"`.
#' @param lengthType Length measurement type (ICES `BiologicalMeasurementType`)
#'   used in FM and BV. Default `"LengthTotal"`.
#' @param classWidth Length class width in mm. Default `NULL` uses the widest
#'   FM accuracy, or 10 mm when there is no FM.
#' @param ageRoute `"sample"` (default) or `"sampleFillPooled"`, see Details.
#' @param externalALK Optional `data.frame` for lower hierarchy B with columns
#'   `lengthClass` (mm, lower bound on the `classWidth` grid), `age`, `prop`
#'   (proportion at age within the length class) and optionally `meanWeight`
#'   (g, live) and `meanLength` (mm).
#' @param gutToLive Optional numeric factor converting BV `WeightGutted` to
#'   live weight.
#' @param useWeights Logical. If `FALSE` individual weights are ignored and
#'   mean weights are not estimated. Default `TRUE`.
#' @param verbose Logical. Print log entries as messages. Default `FALSE`.
#'
#' @return A `data.table` with one row per lowest-level SA (`SAid`) and class:
#'   `class` (age in years, or length class lower bound in mm), `yN` (numbers),
#'   `yNW` (numbers with a weight), `yW_g` (live weight, g), `yNL` (numbers
#'   with a length), `yL_mm` (sum of lengths, mm), all expanded to the catch
#'   fraction, and `nFish` (fish actually measured or aged in that class).
#'   Attributes: `"target"`, `"classWidth"` and the log `"ratioLog"`.
#' @seealso [getSampleUnits()], [getStageWeights()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
#' @export
expandWithinSample <- function(RDBESDataObj,
                               sampleUnits,
                               target = c("Age", "Length"),
                               lengthType = "LengthTotal",
                               classWidth = NULL,
                               ageRoute = c("sample", "sampleFillPooled"),
                               externalALK = NULL,
                               gutToLive = NULL,
                               useWeights = TRUE,
                               verbose = FALSE) {
  target <- match.arg(target)
  ageRoute <- match.arg(ageRoute)
  step <- "expandWithinSample"
  log <- newRatioLog()

  su <- sampleUnits[include == TRUE,
                    .(SAid, SAlowHierarchy, fSample, SApres,
                      conv = as.numeric(SAconFacMeasLive))]
  su[is.na(conv), conv := 1]
  if (nrow(su) == 0) stop("No included sample units.", call. = FALSE)

  # ---------------------------------------------------------------------------
  # Frequency measures (FM)
  # ---------------------------------------------------------------------------
  if (is.null(RDBESDataObj$FM) || nrow(RDBESDataObj$FM) == 0) {
    fm <- data.table::data.table(SAid = numeric(0))
  } else {
    fm <- getLinkedDataFromLevel("SAid", su$SAid, RDBESDataObj, "FM")
  }
  if (nrow(fm) > 0) {
    otherTypes <- setdiff(unique(fm$FMtypeMeas), lengthType)
    if (length(otherTypes) > 0) {
      log <- addToRatioLog(log, step, "info",
        paste0("FM records of type ", paste(otherTypes, collapse = ", "),
               " ignored (lengthType = ", lengthType, ")"),
        fm[FMtypeMeas != lengthType, .N], verbose)
    }
    fm <- fm[FMtypeMeas == lengthType]
    accMap <- accuracyCodeToMM()
    fm[, accMM := unname(accMap[FMaccuracy])]
    badAcc <- unique(fm[is.na(accMM), FMaccuracy])
    if (length(badAcc) > 0) {
      stop("FMaccuracy value(s) not supported for length classes: ",
           paste(badAcc, collapse = ", "), ". Supported: ",
           paste(names(accMap), collapse = ", "), call. = FALSE)
    }
  }

  accAll <- if (nrow(fm) > 0) sort(unique(fm$accMM)) else numeric(0)
  if (is.null(classWidth)) {
    classWidth <- if (length(accAll) > 0) max(accAll) else 10
    log <- addToRatioLog(log, step, "info",
      paste0("classWidth set to ", classWidth, " mm"), NA, verbose)
  }
  if (length(accAll) > 0) {
    ratio <- classWidth / accAll
    if (any(abs(ratio - round(ratio)) > 1e-9 | ratio < 1)) {
      stop("classWidth (", classWidth, " mm) must be a multiple of every FM ",
           "accuracy in the data (", paste(accAll, collapse = ", "), " mm).",
           call. = FALSE)
    }
  }
  toClass <- function(len) floor(len / classWidth) * classWidth
  if (nrow(fm) > 0) {
    fm[, lengthClass := toClass(as.numeric(FMclassMeas))]
    fm[, midLength := as.numeric(FMclassMeas) + accMM / 2]
    fm <- merge(fm, su[, .(SAid, fSample)], by = "SAid")
  }

  # ---------------------------------------------------------------------------
  # Biological variables (BV), one row per fish
  # ---------------------------------------------------------------------------
  fish <- NULL
  bv <- RDBESDataObj$BV
  if (!is.null(bv) && nrow(bv) > 0) {
    bv <- bv[(SAid %in% su$SAid) | (FMid %in% fm$FMid)]
  }
  if (!is.null(bv) && nrow(bv) > 0) {
    bv <- data.table::copy(bv)
    bv[, SAidFish := data.table::fifelse(is.na(SAid),
                                         as.numeric(fm$SAid[match(FMid, fm$FMid)]),
                                         as.numeric(SAid))]
    bv[, FMidFish := data.table::fifelse(is.na(FMid), -1L, as.integer(FMid))]
    dup <- bv[, .N, by = .(SAidFish, FMidFish, BVfishId, BVtypeMeas)][N > 1, .N]
    if (dup > 0) {
      log <- addToRatioLog(log, step, "warning",
        "Fish with the same measurement type recorded twice, first value used",
        dup, verbose)
    }
    fish <- data.table::dcast(bv, SAidFish + FMidFish + BVfishId ~ BVtypeMeas,
                              value.var = "BVvalueMeas",
                              fun.aggregate = function(v) v[1], fill = NA)
    pres <- bv[, .(BVpres = BVpres[1]), by = .(SAidFish, FMidFish, BVfishId)]
    fish <- merge(fish, pres, by = c("SAidFish", "FMidFish", "BVfishId"))
    data.table::setnames(fish, "SAidFish", "SAid")
    fish <- merge(fish, su[, .(SAid, SAlowHierarchy, fSample, SApres,
                               conv)], by = "SAid")
    numCol <- function(col) {
      if (col %in% names(fish)) suppressWarnings(as.numeric(fish[[col]]))
      else rep(NA_real_, nrow(fish))
    }
    fish[, age := numCol("Age")]
    fish[, len := numCol(lengthType)]

    wLive <- numCol("WeightLive")
    wMeas <- numCol("WeightMeasured") * fish$conv
    wGut <- numCol("WeightGutted")
    if (useWeights && any(!is.na(wGut) & is.na(wLive) & is.na(wMeas))) {
      if (is.null(gutToLive)) {
        stop("Some fish only have WeightGutted. Supply gutToLive (gutted to ",
             "live conversion factor) or set useWeights = FALSE.",
             call. = FALSE)
      }
      log <- addToRatioLog(log, step, "assumption",
        paste0("WeightGutted converted to live weight with gutToLive = ",
               gutToLive),
        sum(!is.na(wGut) & is.na(wLive) & is.na(wMeas)), verbose)
    }
    if (is.null(gutToLive)) gutToLive <- NA_real_
    wGut <- wGut * gutToLive
    presDiff <- sum(!is.na(wMeas) & is.na(wLive) & !isBlank(fish$BVpres) &
                      !isBlank(fish$SApres) & fish$BVpres != fish$SApres)
    if (presDiff > 0) {
      log <- addToRatioLog(log, step, "warning",
        "BVpres differs from SApres but SAconFacMeasLive was applied to WeightMeasured",
        presDiff, verbose)
    }
    w <- wLive
    w[is.na(w)] <- wMeas[is.na(w)]
    w[is.na(w)] <- wGut[is.na(w)]
    if (!useWeights) w <- rep(NA_real_, length(w))
    fish[, wt := w]

    # Length class of each fish: FM class in LH A, measured length otherwise
    fish[, lengthClass := toClass(len)]
    if (nrow(fm) > 0) {
      fmCls <- fm[match(fish$FMidFish, fm$FMid)]
      inA <- fish$FMidFish > 0 & !is.na(fmCls$FMid)
      fish[inA, lengthClass := fmCls$lengthClass[inA]]
      fillLen <- inA & is.na(fish$len)
      fish[fillLen, len := fmCls$midLength[fillLen]]
    }
  }
  if (is.null(fish)) {
    fish <- data.table::data.table(SAid = numeric(0), FMidFish = integer(0),
                                   SAlowHierarchy = character(0),
                                   fSample = numeric(0), age = numeric(0),
                                   len = numeric(0), wt = numeric(0),
                                   lengthClass = numeric(0))
  }

  # Summaries of a set of fish rows within a sample and class
  fishSums <- function(d, by) {
    d[, .(n = .N, nW = sum(!is.na(wt)), sW = sum(wt, na.rm = TRUE),
          nL = sum(!is.na(len)), sL = sum(len, na.rm = TRUE)), by = by]
  }
  emptyOut <- data.table::data.table(SAid = numeric(0), class = numeric(0),
                                     yN = numeric(0), yNW = numeric(0),
                                     yW_g = numeric(0), yNL = numeric(0),
                                     yL_mm = numeric(0), nFish = numeric(0))
  pieces <- list(emptyOut)

  if (target == "Length") {
    # ---- LH A and B from FM --------------------------------------------------
    fmAB <- fm[SAid %in% su[SAlowHierarchy %in% c("A", "B"), SAid]]
    if (nrow(fmAB) > 0) {
      lf <- fmAB[, .(n = sum(FMnumAtUnit), sMid = sum(FMnumAtUnit * midLength),
                     f = fSample[1]), by = .(SAid, lengthClass)]
      mw <- fishSums(fish[SAlowHierarchy == "A" & !is.na(lengthClass)],
                     c("SAid", "lengthClass"))
      lf <- merge(lf, mw[, .(SAid, lengthClass, nW, sW)],
                  by = c("SAid", "lengthClass"), all.x = TRUE)
      lf[, meanW := data.table::fifelse(!is.na(nW) & nW > 0, sW / nW,
                                        NA_real_)]
      pieces[[length(pieces) + 1L]] <- lf[, .(
        SAid, class = lengthClass, yN = f * n,
        yNW = data.table::fifelse(is.na(meanW), 0, f * n),
        yW_g = data.table::fifelse(is.na(meanW), 0, f * n * meanW),
        yNL = f * n, yL_mm = f * sMid, nFish = n)]
      noW <- lf[is.na(meanW), sum(n)]
      if (useWeights && noW > 0) {
        log <- addToRatioLog(log, step, "info",
          "Measured fish in length classes without individual weights (no mean weight at length)",
          noW, verbose)
      }
    }
    # ---- LH C from BV --------------------------------------------------------
    fc <- fish[SAlowHierarchy == "C"]
    if (nrow(fc) > 0) {
      tot <- fc[, .(nAll = .N, nHas = sum(!is.na(lengthClass))), by = SAid]
      missingLen <- tot[, sum(nAll - nHas)]
      if (missingLen > 0) {
        log <- addToRatioLog(log, step, "assumption",
          "LH C fish without length: assumed missing at random within their sample",
          missingLen, verbose)
      }
      s <- fishSums(fc[!is.na(lengthClass)], c("SAid", "lengthClass"))
      s <- merge(s, tot, by = "SAid")
      s <- merge(s, su[, .(SAid, f = fSample)], by = "SAid")
      s[, k := f * nAll / nHas]
      pieces[[length(pieces) + 1L]] <- s[, .(
        SAid, class = lengthClass, yN = k * n, yNW = k * nW, yW_g = k * sW,
        yNL = k * nL, yL_mm = k * sL, nFish = n)]
    }
  } else {
    # ---- LH C: ages directly -------------------------------------------------
    fc <- fish[SAlowHierarchy == "C"]
    if (nrow(fc) > 0) {
      tot <- fc[, .(nAll = .N, nAged = sum(!is.na(age))), by = SAid]
      missingAge <- tot[, sum(nAll - nAged)]
      if (missingAge > 0) {
        log <- addToRatioLog(log, step, "assumption",
          "LH C fish without age: assumed missing at random within their sample",
          missingAge, verbose)
      }
      s <- fishSums(fc[!is.na(age)], c("SAid", "age"))
      s <- merge(s, tot[nAged > 0], by = "SAid")
      s <- merge(s, su[, .(SAid, f = fSample)], by = "SAid")
      s[, k := f * nAll / nAged]
      pieces[[length(pieces) + 1L]] <- s[, .(
        SAid, class = age, yN = k * n, yNW = k * nW, yW_g = k * sW,
        yNL = k * nL, yL_mm = k * sL, nFish = n)]
    }

    # ---- LH A: sample ALK ----------------------------------------------------
    suA <- su[SAlowHierarchy == "A", SAid]
    if (length(suA) > 0 && nrow(fm) > 0) {
      lfA <- fm[SAid %in% suA, .(n = sum(FMnumAtUnit), f = fSample[1]),
                by = .(SAid, lengthClass)]
      aged <- fish[SAlowHierarchy == "A" & !is.na(age) & !is.na(lengthClass)]
      alk <- fishSums(aged, c("SAid", "lengthClass", "age"))
      alk[, mTot := sum(n), by = .(SAid, lengthClass)]
      data.table::setnames(alk, "n", "m")
      own <- merge(lfA, alk, by = c("SAid", "lengthClass"))
      pieces[[length(pieces) + 1L]] <- own[, .(
        SAid, class = age, yN = f * n * m / mTot, yNW = f * n * nW / mTot,
        yW_g = f * n * sW / mTot, yNL = f * n * nL / mTot,
        yL_mm = f * n * sL / mTot, nFish = m)]

      gaps <- lfA[!alk, on = .(SAid, lengthClass)]
      if (nrow(gaps) > 0 && ageRoute == "sampleFillPooled") {
        # pooled ALK: aged fish weighted by the fish they represent
        wA <- merge(aged, lfA, by = c("SAid", "lengthClass"))
        wA[, mL := .N, by = .(SAid, lengthClass)]
        wA[, rep := f * n / mL]
        wC <- fish[SAlowHierarchy == "C" & !is.na(age) & !is.na(lengthClass)]
        if (nrow(wC) > 0) wC[, rep := fSample]
        pool <- data.table::rbindlist(list(
          wA[, .(lengthClass, age, wt, len, rep)],
          if (nrow(wC) > 0) wC[, .(lengthClass, age, wt, len, rep)]))
        pk <- pool[, .(r = sum(rep),
                       rW = sum(rep[!is.na(wt)]),
                       sW = sum(rep * wt, na.rm = TRUE),
                       rL = sum(rep[!is.na(len)]),
                       sL = sum(rep * len, na.rm = TRUE)),
                   by = .(lengthClass, age)]
        pk[, rTot := sum(r), by = lengthClass]
        filled <- merge(gaps, pk, by = "lengthClass", allow.cartesian = TRUE)
        pieces[[length(pieces) + 1L]] <- filled[, .(
          SAid, class = age, yN = f * n * r / rTot,
          yNW = f * n * rW / rTot, yW_g = f * n * sW / rTot,
          yNL = f * n * rL / rTot, yL_mm = f * n * sL / rTot, nFish = 0)]
        log <- addToRatioLog(log, step, "assumption",
          "LH A length classes without aged fish filled from the pooled ALK",
          nrow(gaps[lengthClass %in% pk$lengthClass]), verbose)
        gaps <- gaps[!lengthClass %in% pk$lengthClass]
      }
      if (nrow(gaps) > 0) {
        log <- addToRatioLog(log, step, "warning",
          "LH A fish in length classes without any aged fish are lost from numbers at age (raised count)",
          as.integer(round(gaps[, sum(f * n)])), verbose)
      }
    }

    # ---- LH B: external ALK only ---------------------------------------------
    suB <- su[SAlowHierarchy == "B", SAid]
    if (length(suB) > 0) {
      if (is.null(externalALK)) {
        log <- addToRatioLog(log, step, "exclusion",
          "LH B samples have no ages and no externalALK was given: excluded from numerator and auxiliary total",
          length(suB), verbose)
      } else {
        ext <- data.table::as.data.table(externalALK)
        if (!all(c("lengthClass", "age", "prop") %in% names(ext))) {
          stop("externalALK needs columns lengthClass, age and prop.",
               call. = FALSE)
        }
        if (!"meanWeight" %in% names(ext)) ext[, meanWeight := NA_real_]
        if (!"meanLength" %in% names(ext)) {
          ext[, meanLength := lengthClass + classWidth / 2]
        }
        lfB <- fm[SAid %in% suB, .(n = sum(FMnumAtUnit), f = fSample[1]),
                  by = .(SAid, lengthClass)]
        e <- merge(lfB, ext, by = "lengthClass", allow.cartesian = TRUE)
        pieces[[length(pieces) + 1L]] <- e[, .(
          SAid, class = age, yN = f * n * prop,
          yNW = data.table::fifelse(is.na(meanWeight), 0, f * n * prop),
          yW_g = data.table::fifelse(is.na(meanWeight), 0,
                                   f * n * prop * meanWeight),
          yNL = f * n * prop, yL_mm = f * n * prop * meanLength, nFish = 0)]
        lost <- lfB[!lengthClass %in% ext$lengthClass, sum(f * n)]
        log <- addToRatioLog(log, step, "assumption",
          "LH B ages from externalALK: assumed to represent the same population",
          length(suB), verbose)
        if (lost > 0) {
          log <- addToRatioLog(log, step, "warning",
            "LH B fish in length classes missing from externalALK are lost (raised count)",
            as.integer(round(lost)), verbose)
        }
      }
    }
  }

  out <- data.table::rbindlist(pieces, use.names = TRUE)
  out <- out[, .(yN = sum(yN), yNW = sum(yNW), yW_g = sum(yW_g), yNL = sum(yNL),
                 yL_mm = sum(yL_mm), nFish = sum(nFish)), by = .(SAid, class)]
  data.table::setorder(out, SAid, class)

  noData <- setdiff(su$SAid, out$SAid)
  if (length(noData) > 0) {
    log <- addToRatioLog(log, step, "exclusion",
      paste0("Included samples without usable ", tolower(target),
             " data: excluded from numerator and auxiliary total"),
      length(noData), verbose)
  }
  attr(out, "target") <- target
  attr(out, "classWidth") <- classWidth
  attr(out, "ratioLog") <- log
  out[]
}
