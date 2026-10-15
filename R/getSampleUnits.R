#' Build the table of sample units for ratio estimation (hierarchies 1, 2, 5, 7 and 13)
#'
#' First step of the ratio estimation workflow (see [doRatioEstimation()]).
#' It returns one row per **lowest-level Sample (SA) record** of the requested
#' species and catch category, with everything later steps need and nothing
#' hidden. A lowest-level SA is one with no sub-sample below it: the SA itself
#' when there is no sub-sampling, otherwise the smallest sub-samples, where
#' the fish were measured. Parent SA records are used only for their weights.
#'
#'
#' * the path through the upper hierarchy (`VSid`, `FTid`, `FOid`, `SSid` in
#'   hierarchy 1, `FTid`, `FOid` and `SSid` in hierarchy 2, `OSid`, `LEid`,
#'   optionally `FTid`, and `SSid` in
#'   hierarchy 5) with
#'   the selection method, its design class ([classifySelectionMethod()]),
#'   stratum, `numTotal` and `numSamp` of every stage,
#' * the SA sub-sample tree: `topSAid`, `SAlevel` and `fSample`, the
#'   product of the within-sample factors from the lowest-level SA up to the
#'   top SA (see `withinSample`),
#' * the domain variables `year`, `quarter`, `month`, `area`, `statRect` and
#'   `metier6`,
#' * `xSample`, the live weight (kg) of the catch fraction the lowest-level SA
#'   represents: the sampled live weight times `fSample`. This is the
#'   default auxiliary variable of the ratio estimators.
#' * `include` and `excludeReason`.
#'
#' **Within-sample factor.** With `sampleRaising("weight")` (default) each SA
#' record gives `SAtotalWtMes / SAsampWtMes`, with `"count"`
#' `SAnumTotal / SAnumSamp`, and with `"none"` 1. With `"weight"`, `xSample` is the
#' live total weight of the top-level SA fraction. With `"count"` and
#' `"none"`, `xSample` is `fSample` times the live sampled weight of the
#' lowest-level SA (`SAsampWtMes`), and may be missing when that is not
#' filled. It is only needed when the DE or totals ratio uses
#' `"speciesLiveWeight"`.
#'
#' **Live weight.** `SAtotalWtLive` is used when present, otherwise
#' `SAtotalWtMes * SAconFacMeasLive`. A missing conversion factor is taken as
#' 1 and logged. A presentation other than `WHL` with a factor of 1 is logged
#' as suspicious.
#'
#' **Domain variables.** Date in hierarchies 1, 2 and 13: `FOendDate`, else
#' `FOstartDate`, else `FTarvDate` (not in 13). In hierarchy 5: `LEdate`,
#' else `FTarvDate` (when FT is used), else `OSsamDate`. In hierarchy 7:
#' `LEdate`, else `OSsamDate`. Area, rectangle and métier: the SA value when
#' filled, else the FO value (hierarchies 1, 2 and 13) or the LE value
#' (hierarchies 5 and 7). In hierarchy 7, LE is not a sampling unit: it links
#' to SS (`LE$SSid`) and describes the sampled landing. More than one LE per
#' SS is logged and the first is used. Without LE, area and métier come from
#' SA only. `areaLookup` recodes areas (for example divisions to sub-areas).
#'
#' **Recorded zeros.** An SA with a total or sampled weight of 0 records that
#' the species was looked for and not found. [generateZerosUsingSL()] writes
#' such rows: since RDBEScore 0.4.0 with a sampled weight of 0 and no total
#' weight, before that with a total weight of 0. Both are read as zeros. The
#' rows of [generateNAsUsingSL()] have no weights and are not zeros. A zero
#' is flagged `include = FALSE` with `excludeReason = "recorded zero (weight
#' 0)"` and logged as information: its haul (or landing event) counts as
#' sampled with zero catch.
#'
#' **Exclusions.** Samples not sampled (`SAsamp == "N"`, also inherited from a
#' parent SA), lower hierarchy D, samples without the weights needed for
#' raising, and samples without fish records (no FM for lower hierarchy A or
#' B, no BV for C) are flagged `include = FALSE`. They add nothing to the
#' estimate. A haul whose samples of the species are all excluded is treated
#' as not sampled (non-response) by [getStageWeights()].
#'
#' Upper hierarchies 1, 2, 5, 7 and 13 are implemented. The object must hold one
#' hierarchy. Clustering (`XXclustering` other than `"N"`) is logged. It does
#' not affect the point estimates.
#'
#' @param RDBESDataObj A valid `RDBESDataObject` of hierarchy 1, 2, 5, 7 or 13.
#' @param species Species to estimate, as AphiaID(s) (WoRMS), matched to
#'   `SAspeCode`, for example `127143` for plaice. Several values are allowed.
#'   FAO 3-alpha codes are not accepted: rows added by
#'   [generateZerosUsingSL()] and [generateNAsUsingSL()] carry the AphiaID
#'   only.
#' @param catchCat Catch category (`SAcatchCat`). Only `"Lan"` is supported
#'   in this version. Default `"Lan"`.
#' @param areaLookup Optional `data.frame` with columns `from` and `to` to
#'   recode areas before defining domains.
#' @param strict Passed to [filterRDBESDataObject()], which validates the
#'   object. Default `TRUE`.
#' @param withinSample The within-sample raising, made with
#'   [sampleRaising()]. Default `sampleRaising("weight")`. It must be the
#'   same as `SA` in the scheme used later.
#' @param verbose Logical. Print log entries as messages. Default `FALSE`.
#'
#' @return A `data.table`, one row per lowest-level SA. The assumptions and exclusions
#'   log is attached as attribute `"ratioLog"`. The attributes `"species"`,
#'   `"catchCat"`, `"hierarchy"` and `"withinSample"` hold the selection.
#' @seealso [doRatioEstimation()], [expandWithinSample()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
#' @export
getSampleUnits <- function(RDBESDataObj,
                           species,
                           catchCat = "Lan",
                           areaLookup = NULL,
                           strict = TRUE,
                           withinSample = sampleRaising("weight"),
                           verbose = FALSE) {
  step <- "getSampleUnits"
  log <- newRatioLog()
  if (!inherits(withinSample, "RDBESSampleRaising")) {
    stop("withinSample must be made with sampleRaising().", call. = FALSE)
  }
  wsMethod <- withinSample$method

  if (!identical(catchCat, "Lan")) {
    stop("Only catchCat = 'Lan' (landings) is supported in this version.",
         call. = FALSE)
  }
  # ---------------------------------------------------------------------------
  # SA records of the species and catch category. filterRDBESDataObject()
  # also validates the object, so it is called only once. It matches every
  # field to all values given, which is safe here because species codes and
  # catch category codes cannot coincide.
  # ---------------------------------------------------------------------------
  # Species are AphiaIDs only, as in the species list (IScommTaxon) and the
  # other RDBEScore functions. Rows added by generateZerosUsingSL() and
  # generateNAsUsingSL() have SAspeCode but no SAspeCodeFAO.
  checkAphiaIds(species, RDBESDataObj$SA)
  speField <- "SAspeCode"
  filtered <- filterRDBESDataObject(RDBESDataObj, c(speField, "SAcatchCat"),
                                    c(species, catchCat), strict = strict)
  if (is.null(RDBESDataObj$SA) || nrow(RDBESDataObj$SA) == 0) {
    stop("The SA table is empty.")
  }

  # Sampling stages between SD and SA: from the package hierarchy table,
  # plus an optional stage when the data use it (FT under LE in H5)
  sc <- ratioStageChain(RDBESDataObj)
  spec <- sc$spec
  stages <- c(sc$stages, "SS")

  sa <- data.table::copy(filtered$SA)
  if (nrow(sa) == 0) {
    stop("No SA records for ", speField, " = ",
         paste(species, collapse = ", "), " and SAcatchCat = ", catchCat,
         call. = FALSE)
  }

  # Sub-sample tree: parent is the SA in the same SS whose SAseqNum equals
  # this record's SAparSequNum
  sa[, parentSAid := sa[sa, on = .(SSid, SAseqNum = SAparSequNum),
                        x.SAid, mult = "first"]]
  orphan <- sa[!is.na(SAparSequNum) & is.na(parentSAid), .N]
  if (orphan > 0) {
    log <- addToRatioLog(log, step, "warning",
      "SA records with SAparSequNum but no matching parent SA are treated as top-level samples",
      orphan, verbose)
  }

  # Live total weight (g) and raising factor of each SA node
  sa[, convFac := as.numeric(SAconFacMeasLive)]
  noConv <- sa[is.na(convFac) & is.na(SAtotalWtLive), .N]
  if (noConv > 0) {
    log <- addToRatioLog(log, step, "assumption",
      "SAconFacMeasLive missing, measured weight taken as live weight",
      noConv, verbose)
  }
  sa[is.na(convFac), convFac := 1]
  suspicious <- sa[!isBlank(SApres) & SApres != "WHL" & convFac == 1 &
                     is.na(SAtotalWtLive), .N]
  if (suspicious > 0) {
    log <- addToRatioLog(log, step, "warning",
      "SApres is not WHL but SAconFacMeasLive is 1: check that SAtotalWtMes is live weight",
      suspicious, verbose)
  }
  sa[, WliveNode := data.table::fifelse(!is.na(SAtotalWtLive),
                                        as.numeric(SAtotalWtLive),
                                        as.numeric(SAtotalWtMes) * convFac)]
  # A recorded zero: the species was looked for and not found. Rows made by
  # generateZerosUsingSL() have a total weight of 0 (before RDBEScore 0.4.0)
  # or a sampled weight of 0 and no total weight (since 0.4.0). Rows made by
  # generateNAsUsingSL() have no weights at all, so they are not zeros.
  zeroWt <- function(col) {
    if (!col %in% names(sa)) return(rep(FALSE, nrow(sa)))
    v <- suppressWarnings(as.numeric(sa[[col]]))
    !is.na(v) & v == 0
  }
  sa[, zeroNode := zeroWt("SAtotalWtLive") | zeroWt("SAtotalWtMes") |
       zeroWt("SAsampWtLive") | zeroWt("SAsampWtMes")]
  if (wsMethod == "weight") {
    sa[, fNode := as.numeric(SAtotalWtMes) / as.numeric(SAsampWtMes)]
  } else if (wsMethod == "count") {
    sa[, fNode := as.numeric(SAnumTotal) / as.numeric(SAnumSamp)]
  } else {
    sa[, fNode := 1]
  }
  sa[!is.finite(fNode), fNode := NA_real_]

  # Walk up the tree from every node: raising factor of the ancestors,
  # and inherited "not sampled"
  idx <- match(sa$parentSAid, sa$SAid)
  ancFactor <- rep(1, nrow(sa))
  notSampled <- !is.na(sa$SAsamp) & sa$SAsamp == "N"
  cur <- idx
  guard <- 0L
  while (any(!is.na(cur))) {
    guard <- guard + 1L
    if (guard > 50L) stop("Circular SAparSequNum references in SA.")
    has <- !is.na(cur)
    ancFactor[has] <- ancFactor[has] * sa$fNode[cur[has]]
    notSampled[has] <- notSampled[has] |
      (!is.na(sa$SAsamp[cur[has]]) & sa$SAsamp[cur[has]] == "N")
    cur[has] <- idx[cur[has]]
  }
  sa[, notSampledInTree := notSampled]
  sa[, fSample := ancFactor * fNode]
  if (wsMethod == "weight") {
    sa[, xSample := WliveNode * ancFactor / 1000]
  } else {
    # xSample = f x m, with m the live sampled weight of this SA
    sa[, xSample := fSample * as.numeric(SAsampWtMes) * convFac / 1000]
  }

  # Top-level SA and sub-sample level, as in createRDBESEstObject()
  lookup <- prepareSubSampleLevelLookup(sa[, .(SAid, SAparentID = parentSAid)])
  sa[lookup, on = "SAid", `:=`(topSAid = i.topLevelSAid,
                               SAlevel = i.subSampleLevel)]

  # Lowest-level SA only: SA records with no sub-sample below them
  isParent <- sa$SAid %in% sa$parentSAid
  su <- sa[!isParent]

  # ---------------------------------------------------------------------------
  # Upper hierarchy path and design variables
  # ---------------------------------------------------------------------------
  designCols <- c("stratumName", "selectMeth", "numTotal", "numSamp",
                  "clustering", "samp")
  domainSourceCols <- c(spec$dateCols, spec$areaCol, spec$statRectCol,
                        spec$metierCol)
  extraCols <- lapply(stages, function(tb) {
    domainSourceCols[substr(domainSourceCols, 1, 2) == tb]
  })
  names(extraCols) <- stages
  path <- unique(su[, .(SSid)])
  for (k in rev(seq_along(stages))) {
    tb <- stages[k]
    tbl <- RDBESDataObj[[tb]]
    idCol <- paste0(tb, "id")
    parentId <- if (k > 1) paste0(stages[k - 1], "id") else "SDid"
    cols <- intersect(c(idCol, parentId, paste0(tb, designCols),
                        extraCols[[tb]]), names(tbl))
    path <- merge(path, tbl[, cols, with = FALSE], by = idCol, all.x = TRUE)
  }
  de <- merge(RDBESDataObj$SD[, .(SDid, DEid)],
              RDBESDataObj$DE[, .(DEid, DEyear)], by = "DEid")
  path <- merge(path, de[, .(SDid, DEyear)], by = "SDid", all.x = TRUE)

  for (tb in stages) {
    sm <- paste0(tb, "selectMeth")
    if (sm %in% names(path)) {
      path[, (paste0(tb, "selectionClass")) := classifySelectionMethod(get(sm))]
    }
  }

  # SA has its own (usually empty) links such as LEid in H5: the path wins
  clash <- setdiff(intersect(names(su), names(path)), "SSid")
  if (length(clash) > 0) su[, (clash) := NULL]
  su <- merge(su, path, by = "SSid", all.x = TRUE)
  su[, SAselectionClass := classifySelectionMethod(SAselectMeth)]

  # Tables that describe the sample but are not sampling units (LE under SS
  # in H7): their domain columns, one row per link
  for (tb in names(spec$info)) {
    link <- spec$info[[tb]]
    tbl <- RDBESDataObj[[tb]]
    cols <- intersect(domainSourceCols[substr(domainSourceCols, 1, 2) == tb],
                      names(tbl))
    if (is.null(tbl) || nrow(tbl) == 0 || !link %in% names(tbl) ||
        length(cols) == 0) {
      next
    }
    info <- tbl[!is.na(get(link)) & get(link) %in% su[[link]],
                c(link, cols), with = FALSE]
    nPer <- info[, .N, by = link]
    nMulti <- nPer[N > 1, .N]
    if (nMulti > 0) {
      log <- addToRatioLog(log, step, "warning",
        paste0("More than one ", tb, " per ", link,
               ": the domain is taken from the first ", tb), nMulti, verbose)
    }
    info <- info[, .SD[1], by = link]
    su[, (intersect(cols, names(su))) := NULL]
    su <- merge(su, info, by = link, all.x = TRUE)
  }

  # Design classes and clustering, logged per stage
  for (tb in c(stages, "SA")) {
    cl <- paste0(tb, "selectionClass")
    idCol <- paste0(tb, "id")
    if (!cl %in% names(su)) next
    for (cls in c("Quasi-probabilistic", "Non-probabilistic", "Other",
                  "Unrecognised")) {
      nUnits <- data.table::uniqueN(su[get(cl) == cls, get(idCol)])
      if (nUnits > 0) {
        log <- addToRatioLog(log, step,
          if (cls == "Unrecognised") "warning" else "assumption",
          paste0(tb, " selection is ", cls,
                 ": the raising assumes the sampled ", tb,
                 " units represent the unsampled units"),
          nUnits, verbose)
      }
    }
    clu <- paste0(tb, "clustering")
    if (clu %in% names(su)) {
      nClu <- data.table::uniqueN(su[!isBlank(get(clu)) & get(clu) != "N",
                                     get(idCol)])
      if (nClu > 0) {
        log <- addToRatioLog(log, step, "info",
          paste0(tb, " units are clustered. Ignored by the point estimates, ",
                 "relevant for variance"), nClu, verbose)
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Domain variables
  # ---------------------------------------------------------------------------
  su[, sampDate := firstDate(su, spec$dateCols)]
  su[, year := as.integer(format(sampDate, "%Y"))]
  su[is.na(year), year := as.integer(DEyear)]
  su[, month := as.integer(format(sampDate, "%m"))]
  su[, quarter := (month - 1L) %/% 3L + 1L]
  nNoDate <- su[is.na(sampDate), .N]
  if (nNoDate > 0) {
    log <- addToRatioLog(log, step, "warning",
      paste0("No date in ",
             paste(intersect(spec$dateCols, names(su)), collapse = ", "),
             ": quarter and month are NA for these samples"),
      nNoDate, verbose)
  }
  su[, area := firstValue(su, c("SAarea", spec$areaCol))]
  su[, statRect := firstValue(su, c("SAstatRect", spec$statRectCol))]
  su[, metier6 := firstValue(su, c("SAmetier6", spec$metierCol))]
  if (!is.null(areaLookup)) {
    al <- data.table::as.data.table(areaLookup)
    if (!all(c("from", "to") %in% names(al))) {
      stop("areaLookup must have columns 'from' and 'to'.", call. = FALSE)
    }
    hit <- match(su$area, al$from)
    su[!is.na(hit), area := al$to[hit[!is.na(hit)]]]
  }

  # ---------------------------------------------------------------------------
  # Inclusion flags
  # ---------------------------------------------------------------------------
  su[, excludeReason := NA_character_]
  # A weight of 0 records that the species was looked for and not
  # found (e.g. rows made by generateZerosUsingSL()). It adds nothing to the
  # estimate, and its haul counts as sampled with zero catch.
  # Zeros are read from the weights, whatever the within-sample factor
  su[zeroNode == TRUE, excludeReason := zeroSampleReason()]
  su[is.na(excludeReason) & notSampledInTree == TRUE,
     excludeReason := "SAsamp is N (not sampled)"]
  su[is.na(excludeReason) & SAlowHierarchy == "D",
     excludeReason := "lower hierarchy D (no measurements)"]
  if (wsMethod == "weight") {
    su[is.na(excludeReason) & (is.na(fSample) | is.na(xSample)),
       excludeReason := "missing SAtotalWtMes or SAsampWtMes in the SA tree"]
  } else if (wsMethod == "count") {
    su[is.na(excludeReason) & is.na(fSample),
       excludeReason := "missing SAnumTotal or SAnumSamp in the SA tree"]
  }
  # Measured SA without fish records would add weight to the auxiliary total
  # but no fish to the numerator: lower hierarchies A and B need FM, C needs BV
  saWithFM <- RDBESDataObj$FM$SAid
  saWithBV <- RDBESDataObj$BV$SAid
  su[is.na(excludeReason) & SAlowHierarchy %in% c("A", "B") &
       !SAid %in% saWithFM,
     excludeReason := "lower hierarchy A or B but no FM records"]
  su[is.na(excludeReason) & SAlowHierarchy == "C" & !SAid %in% saWithBV,
     excludeReason := "lower hierarchy C but no BV records"]
  su[, include := is.na(excludeReason)]
  nZero <- su[excludeReason == zeroSampleReason(), .N]
  if (nZero > 0) {
    log <- addToRatioLog(log, step, "info",
      "Recorded zeros (weight 0): the species was not found, zero catch",
      nZero, verbose)
  }
  ex <- su[include == FALSE & excludeReason != zeroSampleReason(), .N,
           by = excludeReason]
  for (i in seq_len(nrow(ex))) {
    log <- addToRatioLog(log, step, "exclusion",
      paste0("Excluded from numerator and auxiliary total: ",
             ex$excludeReason[i]), ex$N[i], verbose)
  }
  log <- addToRatioLog(log, step, "info",
    paste0("Lowest-level SA of ", paste(species, collapse = ", "),
           " (", catchCat, ") included"), su[include == TRUE, .N], verbose)

  keep <- c("SAid", "topSAid", "SAlevel", "SSid",
            intersect(paste0(rev(stages[stages != "SS"]), "id"), names(su)),
            unlist(lapply(c(stages, "SA"), function(tb) {
              intersect(paste0(tb, c("selectMeth", "selectionClass",
                                     "stratumName", "numTotal", "numSamp",
                                     "clustering")), names(su))
            })),
            "SAlowHierarchy", "SAspeCodeFAO", "SAspeCode", "SAcatchCat",
            "SApres", "SAconFacMeasLive",
            "year", "quarter", "month", "area", "statRect", "metier6",
            "fSample", "xSample", "include", "excludeReason")
  keep <- unique(intersect(keep, names(su)))
  out <- su[, keep, with = FALSE]
  data.table::setorder(out, SAid)
  attr(out, "species") <- species
  attr(out, "catchCat") <- catchCat
  attr(out, "hierarchy") <- spec$hierarchy
  attr(out, "withinSample") <- wsMethod
  attr(out, "areaLookup") <- areaLookup
  attr(out, "ratioLog") <- log
  out[]
}
