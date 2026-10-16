# AI-assisted: Yes
# Review: pending
# Notes/scope:
# Simulated hierarchy 5 data with a known truth, used to test stage-wise
# raising on onshore sampling (DE - SD - OS - LE - (FT) - SS - SA).
#
# Built on simulateH1RatioData(): the same fish, samples, truth and DE total,
# with the upper tables relabelled:
#   vessel (VS) -> onshore event (OS), e.g. an auction day
#   trip   (FT) -> landing event (LE)
#   haul   (FO) -> trip (FT) under LE, only when useFT = TRUE
# Without FT every landing event must have one "haul" (haulsPerTrip = 1), and
# SS links to LE. sampledHaulsPerTrip (0 or 1 per landing event, recycled)
# then says which landing events were sampled. osStrata gives the design
# strata of OS, recycled over onshore events. OSnumTotal is the number of
# onshore events in the population of each stratum (osInPopulation in total,
# shared over the strata in proportion to the sampled ones).
# VD holds one vessel per landing event's vessel, with VDlen in metres.
#
# Returns list(obj = RDBESDataObject, truthAge, truthLength, fish)
simulateH5RatioData <- function(..., useFT = FALSE, haulsPerTrip = 1,
                                osStrata = "U", osInPopulation = NULL,
                                selectMeth = c(OS = "SRSWOR", LE = "CENSUS",
                                               FT = "CENSUS")) {
  if (!useFT && haulsPerTrip != 1) {
    stop("Without FT every landing event has one sample: haulsPerTrip = 1.")
  }
  sim <- simulateH1RatioData(..., haulsPerTrip = haulsPerTrip,
                             selectMeth = c(VS = selectMeth[["OS"]],
                                            FT = selectMeth[["LE"]],
                                            FO = selectMeth[["FT"]]))
  h1 <- sim$obj
  VS <- h1$VS
  FT1 <- h1$FT
  FO1 <- h1$FO
  nOS <- nrow(VS)
  if (is.null(osInPopulation)) osInPopulation <- unique(VS$VSnumTotal)

  # ---- OS: onshore events ---------------------------------------------------
  osStratum <- rep_len(osStrata, nOS)
  nSampStratum <- table(osStratum)[osStratum]
  firstTrip <- FT1[, .SD[1], by = VSid]
  OS <- data.frame(OSid = VS$VSid, SDid = VS$SDid, OSrecType = "OS",
                   OSseqNum = VS$VSseqNum,
                   OSstratification = if (length(osStrata) > 1) "Y" else "N",
                   OSlocode = "ZWHRE", OSlocName = "Harare",
                   OSlocType = "Auction",
                   OSsamDate = firstTrip$FTarvDate[match(VS$VSid,
                                                         firstTrip$VSid)],
                   OSstratumName = osStratum, OSclustering = "N",
                   OSclusterName = "U",
                   OSnumTotal = as.integer(round(as.numeric(nSampStratum) *
                                                   osInPopulation / nOS)),
                   OSnumSamp = as.integer(nSampStratum),
                   OSselectMeth = selectMeth[["OS"]],
                   OSunitName = paste0("day", VS$VSid),
                   OSincProb = nOS / osInPopulation,
                   OSauxVarValue = VS$VSauxVarValue,
                   # total of the stratum, scaled to the population
                   OSauxVarTot = stats::ave(VS$VSauxVarValue, osStratum,
                                            FUN = sum) *
                     osInPopulation / nOS,
                   OSauxVarName = "LandWt", OSauxVarUnit = "kg", OSsamp = "Y")

  # ---- LE: landing events ---------------------------------------------------
  firstHaul <- FO1[, .SD[1], by = FTid]
  fh <- firstHaul[match(FT1$FTid, firstHaul$FTid)]
  if (useFT) {
    leSampled <- rep(TRUE, nrow(FT1))
  } else {
    leSampled <- fh$FOsamp != "N"
  }
  leNumSamp <- stats::ave(as.integer(leSampled), FT1$VSid, FUN = sum)
  LE <- data.frame(LEid = FT1$FTid, OSid = FT1$VSid, VDid = FT1$VSid,
                   LErecType = "LE", LEencrVessCode = FT1$FTencrVessCode,
                   LEstratification = "N", LEseqNum = FT1$FTseqNum,
                   LEstratumName = "U", LEclustering = "N",
                   LEclusterName = "U", LEmixedTrip = "N", LEcatReg = "Lan",
                   LEdate = FT1$FTarvDate, LEarea = fh$FOarea,
                   LEmetier6 = fh$FOmetier6, LEnumTotal = FT1$FTnumTotal,
                   LEnumSamp = leNumSamp, LEselectMeth = selectMeth[["LE"]],
                   LEunitName = paste0("landing", FT1$FTid),
                   LEincProb = leNumSamp / FT1$FTnumTotal,
                   LEauxVarValue = FT1$FTauxVarValue,
                   LEauxVarTot = FT1$FTauxVarTot,
                   LEauxVarName = "LandWt", LEauxVarUnit = "kg",
                   LEsamp = ifelse(leSampled, "Y", "N"))
  VD <- data.frame(VDid = VS$VSid, VDrecType = "VD",
                   VDencrVessCode = VS$VSencrVessCode, VDyear = 2024L,
                   VDctry = "ZW", VDflgCtry = "ZW",
                   VDlen = 10 + 2 * VS$VSid, VDlenCat = "VL1218")

  # ---- FT under LE, and SS --------------------------------------------------
  SS <- as.data.frame(h1$SS)
  FT <- NULL
  if (useFT) {
    FT <- data.frame(FTid = FO1$FOid, LEid = FO1$FTid, FTrecType = "FT",
                     FTencrVessCode = FT1$FTencrVessCode[match(FO1$FTid,
                                                               FT1$FTid)],
                     FTseqNum = FO1$FOseqNum, FTstratification = "N",
                     FTstratumName = "U", FTclustering = "N",
                     FTclusterName = "U", FTsampType = "MarS",
                     FTarvDate = FO1$FOendDate, FTdepDate = FO1$FOstartDate,
                     FTnumTotal = FO1$FOnumTotal, FTnumSamp = FO1$FOnumSamp,
                     FTselectMeth = selectMeth[["FT"]], FTunitName = "trip",
                     FTincProb = FO1$FOincProb,
                     FTauxVarValue = FO1$FOauxVarValue,
                     FTauxVarTot = FO1$FOauxVarTot,
                     FTauxVarName = "LandWt", FTauxVarUnit = "kg",
                     FTsamp = FO1$FOsamp)
    SS$FTid <- SS$FOid
  } else {
    SS$LEid <- FO1$FTid[match(SS$FOid, FO1$FOid)]
  }
  SS$FOid <- NA_integer_

  DE <- as.data.frame(h1$DE)
  DE$DEhierarchy <- 5L
  lst <- list(DE = DE, SD = as.data.frame(h1$SD), OS = OS, LE = LE, FT = FT,
              SS = SS, SA = as.data.frame(h1$SA), FM = h1$FM, BV = h1$BV,
              VD = VD, CL = as.data.frame(h1$CL))
  lst <- lst[!vapply(lst, is.null, logical(1))]
  lst <- lapply(lst, as.data.frame)
  obj <- withCallingHandlers(
    createRDBESDataObject(lst, castToCorrectDataTypes = TRUE,
                          addMissingColumns = TRUE, strict = TRUE),
    warning = function(w) {
      if (grepl("bypasses the RDBES upload", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    })
  sim$obj <- obj
  sim
}
