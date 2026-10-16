# AI-assisted: Yes
# Review: pending
# Notes/scope:
# Simulated hierarchy 2 data with a known truth, used to test stage-wise
# raising on at-sea sampling of trips (DE - SD - FT - FO - SS - SA).
#
# Built on simulateH1RatioData(): the same fish, samples, truth and DE total,
# without the vessel stage. Trips are the top stage and link to SD. Every
# trip keeps its vessel through VDid (VD has one row per vessel, with VDlen
# in metres). ftStrata gives the design strata of FT, recycled over trips.
# FTnumTotal is the number of trips in the population of each stratum
# (tripsInPopulation in total, shared over the strata in proportion to the
# sampled ones). The default is the number of simulated trips, a census.
#
# Returns list(obj = RDBESDataObject, truthAge, truthLength, fish)
simulateH2RatioData <- function(..., ftStrata = "U", tripsInPopulation = NULL,
                                selectMeth = c(FT = "SRSWOR",
                                               FO = "CENSUS")) {
  sim <- simulateH1RatioData(..., selectMeth = c(VS = "CENSUS",
                                                 FT = selectMeth[["FT"]],
                                                 FO = selectMeth[["FO"]]))
  h1 <- sim$obj
  VS <- h1$VS
  FT1 <- h1$FT
  nFT <- nrow(FT1)
  if (is.null(tripsInPopulation)) tripsInPopulation <- nFT

  # ---- FT: trips sampled directly in SD -------------------------------------
  ftStratum <- rep_len(ftStrata, nFT)
  nSampStratum <- as.numeric(table(ftStratum)[ftStratum])
  FT <- as.data.frame(FT1)
  FT$SDid <- VS$SDid[match(FT1$VSid, VS$VSid)]
  FT$VDid <- FT1$VSid
  FT$VSid <- NULL
  FT$FTstratification <- if (length(ftStrata) > 1) "Y" else "N"
  FT$FTstratumName <- ftStratum
  FT$FTnumTotal <- as.integer(round(nSampStratum * tripsInPopulation / nFT))
  FT$FTnumSamp <- as.integer(nSampStratum)
  FT$FTincProb <- nFT / tripsInPopulation
  # total of the stratum, scaled to the population
  FT$FTauxVarTot <- stats::ave(FT1$FTauxVarValue, ftStratum, FUN = sum) *
    tripsInPopulation / nFT

  VD <- data.frame(VDid = VS$VSid, VDrecType = "VD",
                   VDencrVessCode = VS$VSencrVessCode, VDyear = 2024L,
                   VDctry = "ZW", VDflgCtry = "ZW",
                   VDlen = 10 + 2 * VS$VSid, VDlenCat = "VL1218")

  DE <- as.data.frame(h1$DE)
  DE$DEhierarchy <- 2L
  lst <- list(DE = DE, SD = h1$SD, FT = FT, FO = h1$FO, SS = h1$SS,
              SA = h1$SA, FM = h1$FM, BV = h1$BV, VD = VD, CL = h1$CL)
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
