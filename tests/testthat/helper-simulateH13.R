# AI-assisted: Yes
# Review: pending
# Notes/scope:

# Simulated hierarchy 13 data with a known truth, used to test stage-wise
# raising on hauls sampled directly (DE - SD - FO - SS - SA).
#
# Built on simulateH1RatioData(): the same fish, samples, truth and DE total,
# without the vessel and trip stages. Hauls are the only stage and link to
# SD. All hauls of the simulated trips are listed in FO, the unsampled ones
# with FOsamp = "N". foStrata gives the design strata of FO, recycled over
# hauls. FOnumTotal is the number of hauls in the population of each stratum
# (haulsInPopulation in total, shared over the strata in proportion to the
# listed hauls), FOnumSamp the number sampled in the stratum. The default
# population is the listed hauls.
#
# Returns list(obj = RDBESDataObject, truthAge, truthLength, fish)
simulateH13RatioData <- function(..., foStrata = "U",
                                 haulsInPopulation = NULL,
                                 selectMeth = c(FO = "SRSWOR")) {
  sim <- simulateH1RatioData(..., selectMeth = c(VS = "CENSUS",
                                                 FT = "CENSUS",
                                                 FO = selectMeth[["FO"]]))
  h1 <- sim$obj
  FO1 <- h1$FO
  nFO <- nrow(FO1)
  if (is.null(haulsInPopulation)) haulsInPopulation <- nFO

  # ---- FO: hauls sampled directly in SD -------------------------------------
  foStratum <- rep_len(foStrata, nFO)
  nListed <- as.numeric(table(foStratum)[foStratum])
  sampled <- FO1$FOsamp == "Y"
  nSampStratum <- stats::ave(as.numeric(sampled), foStratum, FUN = sum)
  FO <- as.data.frame(FO1)
  FO$SDid <- h1$SD$SDid[1]
  FO$FTid <- NULL
  FO$FOstratification <- if (length(foStrata) > 1) "Y" else "N"
  FO$FOstratumName <- foStratum
  FO$FOnumTotal <- as.integer(round(nListed * haulsInPopulation / nFO))
  FO$FOnumSamp <- as.integer(nSampStratum)
  FO$FOincProb <- nSampStratum / FO$FOnumTotal
  # total of the stratum, scaled to the population
  FO$FOauxVarTot <- stats::ave(FO1$FOauxVarValue, foStratum, FUN = sum) *
    haulsInPopulation / nFO

  DE <- as.data.frame(h1$DE)
  DE$DEhierarchy <- 13L
  lst <- list(DE = DE, SD = h1$SD, FO = FO, SS = h1$SS, SA = h1$SA,
              FM = h1$FM, BV = h1$BV, CL = h1$CL)
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
