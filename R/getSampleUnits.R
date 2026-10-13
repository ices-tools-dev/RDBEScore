#' Build the table of sample units for ratio estimation (hierarchy 1)
#'
#' First step of the ratio estimation workflow (see [doRatioEstimation()]).
#' It returns one row per **lowest-level Sample (SA) record** of the requested
#' species and catch category, with everything later steps need and nothing
#' hidden. A lowest-level SA is one with no sub-sample below it: the SA itself
#' when there is no sub-sampling, otherwise the smallest sub-samples, where
#' the fish were measured. Parent SA records are used only for their weights.
#'
#'
#' * the path through the upper hierarchy (`VSid`, `FTid`, `FOid`, `SSid`) with
#'   the selection method, its design class ([classifySelectionMethod()]),
#'   stratum, `numTotal` and `numSamp` of every stage,
#' * the SA sub-sample tree: `topSAid`, `SAlevel` and `raiseFactor`, the
#'   product of `SAtotalWtMes / SAsampWtMes` from the lowest-level SA up to
#'   the top SA,
#' * the domain variables `year`, `quarter`, `month`, `area`, `statRect` and
#'   `metier6`,
#' * `x`, the live weight (kg) of the catch fraction the lowest-level SA
#'   represents. This is the auxiliary variable of the ratio estimators.
#' * `include` and `excludeReason`.
#'
#' **Live weight.** `SAtotalWtLive` is used when present, otherwise
#' `SAtotalWtMes * SAconFacMeasLive`. A missing conversion factor is taken as
#' 1 and logged. A presentation other than `WHL` with a factor of 1 is logged
#' as suspicious.
#'
#' **Domain variables.** Date: `FOendDate`, else `FOstartDate`, else
#' `FTarvDate`. Area, rectangle and métier: the SA value when filled, else the
#' FO value. `areaLookup` recodes areas (for example divisions to
#' sub-areas).
#'
#' **Recorded zeros.** An SA with a total weight of 0 (for example made by
#' [generateZerosUsingSL()]) records that the species was looked for and not
#' found. It is flagged `include = FALSE` with `excludeReason = "recorded
#' zero (total weight 0)"` and logged as information: its haul counts as
#' sampled with zero catch.
#'
#' **Exclusions.** Samples not sampled (`SAsamp == "N"`, also inherited from a
#' parent SA), lower hierarchy D, samples without the weights needed for
#' raising, and samples without fish records (no FM for lower hierarchy A or
#' B, no BV for C) are flagged `include = FALSE`. They add nothing to the
#' estimate. A haul whose samples of the species are all excluded is treated
#' as not sampled (non-response) by [getStageWeights()].
#'
#' Only upper hierarchy 1 is implemented. Clustering (`XXclustering` other
#' than `"N"`) is logged. It does not affect the point estimates.
#'
#' @param RDBESDataObj A valid `RDBESDataObject` of hierarchy 1.
#' @param species Species to estimate. A FAO 3-alpha code matched to
#'   `SAspeCodeFAO`, or an AphiaID (numeric) matched to `SAspeCode`. Several
#'   values are allowed.
#' @param catchCat Catch category (`SAcatchCat`). Only `"Lan"` is supported
#'   in this version. Default `"Lan"`.
#' @param areaLookup Optional `data.frame` with columns `from` and `to` to
#'   recode areas before defining domains.
#' @param strict Passed to [filterRDBESDataObject()], which validates the
#'   object. Default `TRUE`.
#' @param verbose Logical. Print log entries as messages. Default `FALSE`.
#'
#' @return A `data.table`, one row per lowest-level SA. The assumptions and exclusions
#'   log is attached as attribute `"ratioLog"`.
#' @seealso [doRatioEstimation()], [expandWithinSample()]
#' @export
getSampleUnits <- function(RDBESDataObj,
                           species,
                           catchCat = "Lan",
                           areaLookup = NULL,
                           strict = TRUE,
                           verbose = FALSE) {
  step <- "getSampleUnits"
  log <- newRatioLog()

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
  bySpeCode <- suppressWarnings(!any(is.na(as.numeric(species))))
  speField <- if (bySpeCode) "SAspeCode" else "SAspeCodeFAO"
  filtered <- filterRDBESDataObject(RDBESDataObj, c(speField, "SAcatchCat"),
                                    c(species, catchCat), strict = strict)
  if (is.null(RDBESDataObj$SA) || nrow(RDBESDataObj$SA) == 0) {
    stop("The SA table is empty.")
  }

  hier <- unique(RDBESDataObj$DE$DEhierarchy)
  if (length(hier) != 1 || !identical(as.integer(hier), 1L)) {
    stop("Only upper hierarchy 1 is implemented. DEhierarchy in the data: ",
         paste(hier, collapse = ", "), call. = FALSE)
  }

  # Sampling stages between SD and SA, taken from the package hierarchy table
  chain <- getTablesInRDBESHierarchy(1, includeOptTables = FALSE,
                                     includeLowHierTables = FALSE,
                                     includeTablesNotInSampHier = FALSE)
  stages <- setdiff(chain, c("DE", "SD", "SA"))

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
  sa[, fNode := as.numeric(SAtotalWtMes) / as.numeric(SAsampWtMes)]
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
  sa[, raiseFactor := ancFactor * fNode]
  sa[, x := WliveNode * ancFactor / 1000]

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
  extraCols <- list(VS = character(0),
                    FT = c("FTarvDate", "FTdepDate"),
                    FO = c("FOendDate", "FOstartDate", "FOarea",
                           "FOstatRect", "FOmetier6"),
                    SS = character(0))
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

  su <- merge(su, path, by = "SSid", all.x = TRUE)
  su[, SAselectionClass := classifySelectionMethod(SAselectMeth)]

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
  d <- parseRDBESDate(su$FOendDate)
  if ("FOstartDate" %in% names(su)) {
    d[is.na(d)] <- parseRDBESDate(su$FOstartDate)[is.na(d)]
  }
  if ("FTarvDate" %in% names(su)) {
    d[is.na(d)] <- parseRDBESDate(su$FTarvDate)[is.na(d)]
  }
  su[, sampDate := d]
  su[, year := as.integer(format(sampDate, "%Y"))]
  su[is.na(year), year := as.integer(DEyear)]
  su[, month := as.integer(format(sampDate, "%m"))]
  su[, quarter := (month - 1L) %/% 3L + 1L]
  nNoDate <- su[is.na(sampDate), .N]
  if (nNoDate > 0) {
    log <- addToRatioLog(log, step, "warning",
      "No FO or FT date: quarter and month are NA for these samples",
      nNoDate, verbose)
  }
  su[, area := data.table::fifelse(!isBlank(SAarea), as.character(SAarea),
                                   as.character(FOarea))]
  su[, statRect := data.table::fifelse(!isBlank(SAstatRect),
                                       as.character(SAstatRect),
                                       as.character(FOstatRect))]
  su[, metier6 := data.table::fifelse(!isBlank(SAmetier6),
                                      as.character(SAmetier6),
                                      as.character(FOmetier6))]
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
  # A total weight of 0 records that the species was looked for and not
  # found (e.g. rows made by generateZerosUsingSL()). It adds nothing to the
  # estimate, and its haul counts as sampled with zero catch.
  su[!is.na(x) & x == 0, excludeReason := zeroSampleReason()]
  su[is.na(excludeReason) & notSampledInTree == TRUE,
     excludeReason := "SAsamp is N (not sampled)"]
  su[is.na(excludeReason) & SAlowHierarchy == "D",
     excludeReason := "lower hierarchy D (no measurements)"]
  su[is.na(excludeReason) & (is.na(raiseFactor) | is.na(x)),
     excludeReason := "missing SAtotalWtMes or SAsampWtMes in the SA tree"]
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
      "Recorded zeros (total weight 0): the species was not found, zero catch",
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
            "raiseFactor", "x", "include", "excludeReason")
  keep <- unique(intersect(keep, names(su)))
  out <- su[, keep, with = FALSE]
  data.table::setorder(out, SAid)
  attr(out, "ratioLog") <- log
  out[]
}
