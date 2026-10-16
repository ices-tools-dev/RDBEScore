# AI-assisted: Yes
# Review: pending
# Notes/scope:
# Simulated hierarchy 1 data with a known truth, used to test the ratio
# estimation functions (doRatioEstimation and its steps).
#
# A population of fish is generated per haul (age, length in mm, live weight
# in g and commercial size category). Sampled hauls get SA records (one per
# haul, or one per size category when stratifiedSA = TRUE) with the chosen
# lower hierarchy. CL holds the landed weight of ALL hauls (sampled or not)
# by year, quarter, area, metier and size category. Weights in SA and CL are
# built from the same rounded per haul and category weights, so that a
# census design reproduces the truth exactly.
#
# For stage-wise raising the upper tables also carry design information:
# FOsamp is "N" for hauls that were not sampled, sampledHaulsPerTrip may
# differ per trip (recycled over trips), and every stage has XXincProb,
# XXauxVarValue (landed live weight of the unit, kg) and XXauxVarTot (the
# total of the parent). DEauxVarTot is the landed live weight in CL (kg).
# VSnumTotal is vesselsInPopulation, while the fish, CL and DE totals cover
# the sampled vessels only. vesselEffect fixes the vessel effects on the age
# composition (log scale), so that sampling intensity can be made to depend
# on composition.
#
# Returns list(obj = RDBESDataObject, truthAge, truthLength, fish)
simulateH1RatioData <- function(seed = 1,
                                year = 2024L,
                                nVessels = 4,
                                tripsPerVessel = 2,
                                haulsPerTrip = 2,
                                sampledHaulsPerTrip = haulsPerTrip,
                                vesselsInPopulation = 2L * nVessels,
                                vesselEffect = NULL,
                                fishPerHaul = c(150, 400),
                                quarters = c(1L, 2L),
                                areas = c("27.4.a", "27.4.b"),
                                metiers = "OTB_DEF_70-99_0_0",
                                lowHierarchy = "C",
                                stratifiedSA = FALSE,
                                boxFish = Inf,
                                subsample = FALSE,
                                gutted = FALSE,
                                gutFactor = 0.88,
                                nPerLengthClass = 2,
                                selectMeth = c(VS = "SRSWOR", FT = "CENSUS",
                                               FO = "CENSUS")) {
  set.seed(seed)
  ages <- 1:12

  # ---- population -----------------------------------------------------------
  trips <- data.table::data.table(VSid = rep(seq_len(nVessels),
                                             each = tripsPerVessel))
  trips[, FTid := .I]
  trips[, FTseqNum := seq_len(.N), by = VSid]
  trips[, quarter := quarters[(FTid - 1L) %% length(quarters) + 1L]]
  trips[, area := areas[(VSid - 1L) %% length(areas) + 1L]]
  trips[, metier6 := metiers[(FTid - 1L) %% length(metiers) + 1L]]
  trips[, date := sprintf("%d-%02d-15", year, (quarter - 1L) * 3L + 2L)]
  theta <- stats::rnorm(nVessels, 0, 0.4)
  # Fixed vessel effects on the age composition (the draw above is kept so
  # that the random stream does not change)
  if (!is.null(vesselEffect)) theta <- rep_len(vesselEffect, nVessels)

  hauls <- trips[, .(FOseqNum = seq_len(haulsPerTrip)), by = FTid]
  hauls[, FOid := .I]
  hauls <- merge(hauls, trips, by = "FTid")
  nSampTrip <- rep_len(sampledHaulsPerTrip, nrow(trips))
  hauls[, sampled := FOseqNum <= nSampTrip[FTid]]

  fish <- hauls[, {
    nf <- sample(fishPerHaul[1]:fishPerHaul[2], 1)
    p <- stats::dpois(ages - 1, 3 * exp(theta[VSid]))
    .(age = sample(ages, nf, replace = TRUE, prob = p))
  }, by = .(FOid)]
  fish[, len := 500 * (1 - exp(-0.25 * (age + 0.5))) *
         exp(stats::rnorm(.N, 0, 0.07))]
  fish[, len := round(len)]
  fish[, wLive := 0.0089 * (len / 10)^3.05 * exp(stats::rnorm(.N, 0, 0.07))]
  fish[, wLive := round(wLive, 1)]
  fish[, commCat := as.character(cut(len, c(0, 250, 300, 350, Inf),
                                     labels = c("4", "3", "2", "1"),
                                     right = FALSE))]
  fish[, fishId := seq_len(.N), by = FOid]
  fish <- merge(fish, hauls[, .(FOid, FTid, VSid, quarter, area, metier6,
                                sampled)], by = "FOid")

  # rounded live weight per haul and size category (g)
  hc <- fish[, .(Wlive = round(sum(wLive))), by = .(FOid, commCat)]

  # ---- truth ----------------------------------------------------------------
  fish[, year := year]
  truthAge <- fish[, .(N = .N, W = sum(wLive)),
                   by = .(year, quarter, area, metier6, age)]
  truthLength <- fish[, .(N = .N), by = .(year, quarter, area, metier6,
                                          lengthClass_mm = floor(len / 10) * 10)]

  # ---- auxiliary values: landed live weight per unit (kg) --------------------
  haulKg <- hc[, .(kg = sum(Wlive) / 1000), by = FOid]
  haulKg <- merge(haulKg, hauls[, .(FOid, FTid, VSid)], by = "FOid")
  tripKg <- haulKg[, .(kg = sum(kg)), by = .(FTid, VSid)]
  vesselKg <- tripKg[, .(kg = sum(kg)), by = VSid]
  hKg <- haulKg$kg[match(hauls$FOid, haulKg$FOid)]
  hTripKg <- tripKg$kg[match(hauls$FTid, tripKg$FTid)]
  tKg <- tripKg$kg[match(trips$FTid, tripKg$FTid)]
  tVesKg <- vesselKg$kg[match(trips$VSid, vesselKg$VSid)]
  vKg <- vesselKg$kg[match(seq_len(nVessels), vesselKg$VSid)]

  # ---- upper hierarchy tables -----------------------------------------------
  DE <- data.frame(DEid = 1L, DErecType = "DE", DEsampScheme = "Sim",
                   DEsampSchemeType = "NatPilCF", DEyear = year,
                   DEstratumName = "U", DEhierarchyCor = "Y",
                   DEhierarchy = 1L, DEsamp = "Y")
  SD <- data.frame(SDid = 1L, DEid = 1L, SDrecType = "SD", SDctry = "ZW",
                   SDinst = "4484")
  VS <- data.frame(VSid = seq_len(nVessels), SDid = 1L, VSrecType = "VS",
                   VSseqNum = seq_len(nVessels),
                   VSencrVessCode = paste0("V", seq_len(nVessels)),
                   VSstratification = "N", VSstratumName = "U",
                   VSclustering = "N", VSclusterName = "U",
                   VSnumTotal = vesselsInPopulation, VSnumSamp = nVessels,
                   VSselectMeth = selectMeth[["VS"]], VSunitName = "vessel",
                   VSincProb = nVessels / vesselsInPopulation,
                   VSauxVarValue = vKg,
                   VSauxVarTot = sum(vKg) * vesselsInPopulation / nVessels,
                   VSauxVarName = "LandWt", VSauxVarUnit = "kg",
                   VSsamp = "Y")
  FT <- data.frame(FTid = trips$FTid, VSid = trips$VSid, FTrecType = "FT",
                   FTencrVessCode = paste0("V", trips$VSid),
                   FTseqNum = trips$FTseqNum, FTstratification = "N",
                   FTstratumName = "U", FTclustering = "N",
                   FTclusterName = "U", FTsampType = "SeaS",
                   FTfoNum = haulsPerTrip, FTdepDate = trips$date,
                   FTarvDate = trips$date, FTnumTotal = tripsPerVessel,
                   FTnumSamp = tripsPerVessel,
                   FTselectMeth = selectMeth[["FT"]], FTunitName = "trip",
                   FTincProb = 1, FTauxVarValue = tKg, FTauxVarTot = tVesKg,
                   FTauxVarName = "LandWt", FTauxVarUnit = "kg",
                   FTsamp = "Y")
  FO <- data.frame(FOid = hauls$FOid, FTid = hauls$FTid, FOrecType = "FO",
                   FOstratification = "N", FOseqNum = hauls$FOseqNum,
                   FOstratumName = "U", FOclustering = "N",
                   FOclusterName = "U", FOaggLev = "H", FOval = "V",
                   FOcatReg = "Lan", FOstartDate = hauls$date,
                   FOendDate = hauls$date, FOarea = hauls$area,
                   FOmetier6 = hauls$metier6, FOnumTotal = haulsPerTrip,
                   FOnumSamp = nSampTrip[hauls$FTid],
                   FOselectMeth = selectMeth[["FO"]], FOunitName = "haul",
                   FOincProb = nSampTrip[hauls$FTid] / haulsPerTrip,
                   FOauxVarValue = hKg, FOauxVarTot = hTripKg,
                   FOauxVarName = "LandWt", FOauxVarUnit = "kg",
                   FOsamp = ifelse(hauls$sampled, "Y", "N"))
  sampledHauls <- hauls[sampled == TRUE]
  SS <- data.frame(SSid = sampledHauls$FOid, FOid = sampledHauls$FOid,
                   SSrecType = "SS", SSseqNum = 1L, SSstratification = "N",
                   SSstratumName = "U", SSclustering = "N",
                   SSclusterName = "U", SScatchFra = "Lan",
                   SSspecListName = "Sim", SSuseCalcZero = "Y",
                   SSnumTotal = 1L, SSnumSamp = 1L, SSselectMeth = "CENSUS",
                   SSunitName = "species", SSsamp = "Y")
  FO <- FO[FO$FOid %in% hauls$FOid, ]

  # ---- samples ----------------------------------------------------------------
  pres <- if (gutted) "GUT" else "WHL"
  presFac <- if (gutted) gutFactor else 1
  conv <- if (gutted) 1 / gutFactor else 1
  lhOf <- function(i) lowHierarchy[(i - 1L) %% length(lowHierarchy) + 1L]

  saRows <- list()
  fmRows <- list()
  bvRows <- list()
  saId <- 0L
  fmId <- 0L
  bvId <- 0L
  addSA <- function(SSid, seqNum, parSeq, stratumName, commCat, Wtot, Wsamp,
                    lh) {
    saId <<- saId + 1L
    saRows[[length(saRows) + 1L]] <<- data.frame(
      SAid = saId, SSid = SSid, SArecType = "SA", SAseqNum = seqNum,
      SAparSequNum = parSeq,
      SAstratification = if (stratumName == "U") "N" else "Y",
      SAstratumName = stratumName, SAspeCode = "127143",
      SAspeCodeFAO = "PLE", SAstateOfProc = "FRE", SApres = pres,
      SAspecState = "DeadOrZeroProbSurvival", SAcatchCat = "Lan",
      SAlandCat = "HuC", SAcommCatScl = if (is.na(commCat)) NA else "EU",
      SAcommCat = commCat, SAsex = "U", SAunitType = "Individuals",
      SAtotalWtMes = as.integer(round(Wtot * presFac)),
      SAsampWtMes = as.integer(round(Wsamp * presFac)),
      SAconFacMeasLive = conv, SAnumTotal = NA_real_, SAnumSamp = NA_real_,
      SAselectMeth = "SRSWOR", SAunitName = paste0("SA", saId),
      SAlowHierarchy = lh, SAsamp = "Y")
    saId
  }
  addLower <- function(sid, lh, bx) {
    wCol <- if (gutted) "WeightGutted" else "WeightLive"
    if (lh %in% c("A", "B")) {
      lf <- bx[, .(n = .N), by = .(cls = floor(len / 10) * 10)]
      ids <- fmId + seq_len(nrow(lf))
      fmId <<- fmId + nrow(lf)
      fmRows[[length(fmRows) + 1L]] <<- data.frame(
        FMid = ids, SAid = sid, FMrecType = "FM", FMstateOfProc = "FRE",
        FMpres = pres, FMclassMeas = as.integer(lf$cls),
        FMnumAtUnit = as.integer(lf$n), FMtypeMeas = "LengthTotal",
        FMmethod = "Measured", FMaccuracy = "cm", FMtypeAssess = "LengthTotal")
      if (lh == "A") {
        bx <- data.table::copy(bx)
        bx[, cls := floor(len / 10) * 10]
        aged <- bx[, .SD[sample.int(.N, min(.N, nPerLengthClass))], by = cls]
        aged[, FMid := ids[match(cls, lf$cls)]]
        addBV(aged, sid = NA_integer_, wCol = wCol)
      }
    } else if (lh == "C") {
      bx <- data.table::copy(bx)
      bx[, FMid := NA_integer_]
      addBV(bx, sid = sid, wCol = wCol)
    }
  }
  addBV <- function(fsh, sid, wCol) {
    long <- data.table::rbindlist(list(
      fsh[, .(FMid, fishId, BVtypeMeas = "Age", BVvalueMeas = as.character(age),
              BVvalUnitScale = "Ageyear")],
      fsh[, .(FMid, fishId, BVtypeMeas = "LengthTotal",
              BVvalueMeas = as.character(len), BVvalUnitScale = "Lengthmm")],
      fsh[, .(FMid, fishId, BVtypeMeas = wCol,
              BVvalueMeas = as.character(round(wLive * presFac, 1)),
              BVvalUnitScale = "Weightg")]))
    ids <- bvId + seq_len(nrow(long))
    bvId <<- bvId + nrow(long)
    bvRows[[length(bvRows) + 1L]] <<- data.frame(
      BVid = ids, SAid = sid, FMid = long$FMid, BVrecType = "BV",
      BVfishId = as.character(long$fishId), BVstateOfProc = "FRE",
      BVpres = pres, BVstratification = "N", BVstratumName = "U",
      BVtypeMeas = long$BVtypeMeas, BVvalueMeas = long$BVvalueMeas,
      BVvalUnitScale = long$BVvalUnitScale, BVselectMeth = "SRSWOR",
      BVunitName = paste0("F", long$fishId), BVsampler = "Observer")
  }
  takeBox <- function(f) {
    if (is.infinite(boxFish) || boxFish >= nrow(f)) return(f)
    f[sample.int(nrow(f), boxFish)]
  }

  for (i in seq_len(nrow(sampledHauls))) {
    h <- sampledHauls$FOid[i]
    lh <- lhOf(i)
    fh <- fish[FOid == h]
    wh <- hc[FOid == h]
    if (stratifiedSA) {
      for (cc in sort(unique(fh$commCat))) {
        fc <- fh[commCat == cc]
        bx <- takeBox(fc)
        Wt <- wh[commCat == cc, Wlive]
        Ws <- if (nrow(bx) == nrow(fc)) Wt else sum(bx$wLive)
        sid <- addSA(h, which(sort(unique(fh$commCat)) == cc), NA_real_, cc,
                     cc, Wt, Ws, lh)
        addLower(sid, lh, bx)
      }
    } else if (subsample) {
      Wt <- sum(wh$Wlive)
      half <- fh[sample.int(nrow(fh), floor(nrow(fh) / 2))]
      quarterBox <- half[sample.int(nrow(half), floor(nrow(half) / 2))]
      addSA(h, 1, NA_real_, "U", NA_character_, Wt, sum(half$wLive), lh)
      sid <- addSA(h, 2, 1, "U", NA_character_, sum(half$wLive),
                   sum(quarterBox$wLive), lh)
      addLower(sid, lh, quarterBox)
    } else {
      bx <- takeBox(fh)
      Wt <- sum(wh$Wlive)
      Ws <- if (nrow(bx) == nrow(fh)) Wt else sum(bx$wLive)
      sid <- addSA(h, 1, NA_real_, "U", NA_character_, Wt, Ws, lh)
      addLower(sid, lh, bx)
    }
  }
  SA <- do.call(rbind, saRows)
  FM <- if (length(fmRows)) do.call(rbind, fmRows) else NULL
  BV <- if (length(bvRows)) do.call(rbind, bvRows) else NULL

  # ---- CL: all hauls, rounded live weights, by size category -----------------
  clw <- merge(hc, hauls[, .(FOid, quarter, area, metier6)], by = "FOid")
  clw <- clw[, .(CLoffWeight = sum(Wlive) / 1000),
             by = .(quarter, area, metier6, commCat)]
  CL <- data.frame(CLid = seq_len(nrow(clw)), CLrecType = "CL",
                   CLlanCou = "ZW", CLvesFlagCou = "ZW", CLyear = year,
                   CLquar = clw$quarter, CLarea = clw$area,
                   CLspecCode = 127143L, CLspecFAO = "PLE",
                   CLlandCat = "HuC", CLcatchCat = "Lan",
                   CLsizeCatScale = "EU", CLsizeCat = clw$commCat,
                   CLmetier6 = clw$metier6, CLoffWeight = clw$CLoffWeight,
                   CLsciWeight = clw$CLoffWeight)

  DE$DEauxVarTot <- sum(CL$CLoffWeight)
  DE$DEauxVarName <- "LandWt"
  DE$DEauxVarUnit <- "kg"

  lst <- list(DE = DE, SD = SD, VS = VS, FT = FT, FO = FO, SS = SS, SA = SA,
              FM = FM, BV = BV, CL = CL)
  lst <- lst[!vapply(lst, is.null, logical(1))]
  # Exported entry point, so the simulator also works with library(RDBEScore).
  # Its note that local tables bypass the RDBES upload checks is muffled.
  obj <- withCallingHandlers(
    createRDBESDataObject(lst, castToCorrectDataTypes = TRUE,
                          addMissingColumns = TRUE, strict = TRUE),
    warning = function(w) {
      if (grepl("bypasses the RDBES upload", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    })
  list(obj = obj, truthAge = truthAge[order(quarter, area, metier6, age)],
       truthLength = truthLength, fish = fish)
}
