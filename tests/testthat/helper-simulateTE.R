# AI-assisted: Yes
# Review: pending
# Notes/scope:

# Simulated data with temporal events (TE) and a known truth, used to test
# stage-wise raising on hierarchies 3, 8 and 10:
#   H3:  DE - SD - TE - VS - FT - FO - SS - SA
#   H8:  DE - SD - TE - VS - LE - SS - SA
#   H10: DE - SD - VS - TE - FT - FO - SS - SA
#
# All are built on simulateH1RatioData(): the same fish, samples, truth and
# DE total, with a TE stage added and, for H8, the trips relabelled as
# landing events (one sample per landing event, as in helper-simulateH5.R).
#
# In H3 and H8 the vessels are shared over nTE temporal events (for example
# weeks), recycled in vessel order. TEnumTotal is teInPopulation, the number
# of temporal events in the population (default nTE, a census). Within a
# TE, VSnumTotal is the TE's share of vesselsInPopulation, in proportion to
# its sampled vessels. In H10 a temporal event is one quarter of one vessel:
# the trips of a vessel in that quarter. TEnumTotal is the number of quarters
# of the vessel in the population (teInPopulationPerVessel, default the
# simulated quarters of that vessel).
#
# Every stage has XXincProb, XXauxVarValue (landed live weight of the unit,
# kg, scaled to its population) and XXauxVarTot (the total of the parent,
# scaled to its population).
#
# Each returns list(obj = RDBESDataObject, truthAge, truthLength, fish)

# Builds the RDBESDataObject from a list of tables
simTEObject <- function(lst) {
  lst <- lst[!vapply(lst, is.null, logical(1))]
  lst <- lapply(lst, as.data.frame)
  withCallingHandlers(
    createRDBESDataObject(lst, castToCorrectDataTypes = TRUE,
                          addMissingColumns = TRUE, strict = TRUE),
    warning = function(w) {
      if (grepl("bypasses the RDBES upload", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    })
}

# One TE row per temporal event, with design columns
simTETable <- function(TEid, parentCol, parentId, numTotal, numSamp, kg,
                       kgTot, selectMeth) {
  te <- data.frame(TEid = TEid, TErecType = "TE",
                   TEseqNum = seq_along(TEid), TEstratification = "N",
                   TEtimeUnit = "Week", TEstratumName = "U",
                   TEclustering = "N", TEclusterName = "U",
                   TEnumTotal = as.integer(numTotal),
                   TEnumSamp = as.integer(numSamp),
                   TEselectMeth = selectMeth,
                   TEunitName = paste0("week", TEid),
                   TEincProb = numSamp / numTotal,
                   TEauxVarValue = kg, TEauxVarTot = kgTot,
                   TEauxVarName = "LandWt", TEauxVarUnit = "kg",
                   TEsamp = "Y")
  te[[parentCol]] <- parentId
  te
}

# TE above the vessels (H3 and H8): returns list(TE, VS)
simTEOverVessels <- function(h1, nTE, teInPopulation, vesselsInPopulation,
                             selectMeth) {
  VS <- as.data.frame(h1$VS)
  nVes <- nrow(VS)
  if (is.null(teInPopulation)) teInPopulation <- nTE
  if (is.null(vesselsInPopulation)) vesselsInPopulation <- unique(VS$VSnumTotal)
  teOfVes <- rep_len(seq_len(nTE), nVes)
  vesScale <- vesselsInPopulation / nVes
  nVesTE <- as.numeric(table(teOfVes)[as.character(teOfVes)])
  # landings of the TE: its sampled vessels scaled to all its vessels
  teKg <- tapply(VS$VSauxVarValue, teOfVes, sum) * vesScale
  TE <- simTETable(seq_len(nTE), "SDid", VS$SDid[1],
                   numTotal = teInPopulation, numSamp = nTE, kg = teKg,
                   kgTot = sum(teKg) * teInPopulation / nTE,
                   selectMeth = selectMeth)
  VS$TEid <- teOfVes
  VS$VSnumTotal <- as.integer(round(nVesTE * vesScale))
  VS$VSnumSamp <- as.integer(nVesTE)
  VS$VSincProb <- VS$VSnumSamp / VS$VSnumTotal
  VS$VSauxVarTot <- teKg[as.character(teOfVes)]
  list(TE = TE, VS = VS)
}

simulateH3RatioData <- function(..., nTE = 2, teInPopulation = NULL,
                                vesselsInPopulation = NULL,
                                selectMeth = c(TE = "SRSWOR", VS = "SRSWOR",
                                               FT = "CENSUS",
                                               FO = "CENSUS")) {
  args <- list(...)
  if (!is.null(vesselsInPopulation)) {
    args$vesselsInPopulation <- vesselsInPopulation
  }
  args$selectMeth <- selectMeth[c("VS", "FT", "FO")]
  sim <- do.call(simulateH1RatioData, args)
  h1 <- sim$obj
  up <- simTEOverVessels(h1, nTE, teInPopulation, vesselsInPopulation,
                         selectMeth[["TE"]])
  DE <- as.data.frame(h1$DE)
  DE$DEhierarchy <- 3L
  sim$obj <- simTEObject(list(DE = DE, SD = h1$SD, TE = up$TE, VS = up$VS,
                              FT = h1$FT, FO = h1$FO, SS = h1$SS, SA = h1$SA,
                              FM = h1$FM, BV = h1$BV, CL = h1$CL))
  sim
}

simulateH8RatioData <- function(..., nTE = 2, teInPopulation = NULL,
                                vesselsInPopulation = NULL,
                                selectMeth = c(TE = "SRSWOR", VS = "SRSWOR",
                                               LE = "CENSUS")) {
  args <- list(...)
  if (!is.null(vesselsInPopulation)) {
    args$vesselsInPopulation <- vesselsInPopulation
  }
  # one haul per trip, so that each landing event has one sample
  args$haulsPerTrip <- 1
  args$selectMeth <- c(VS = selectMeth[["VS"]], FT = "CENSUS",
                       FO = selectMeth[["LE"]])
  sim <- do.call(simulateH1RatioData, args)
  h1 <- sim$obj
  up <- simTEOverVessels(h1, nTE, teInPopulation, vesselsInPopulation,
                         selectMeth[["TE"]])
  FT1 <- h1$FT
  FO1 <- h1$FO
  fo <- FO1[match(FT1$FTid, FO1$FTid)]
  # ---- LE: one landing event per simulated trip -----------------------------
  sampled <- fo$FOsamp == "Y"
  nSampVes <- stats::ave(as.numeric(sampled), FT1$VSid, FUN = sum)
  LE <- data.frame(LEid = FT1$FTid, VSid = FT1$VSid, LErecType = "LE",
                   LEencrVessCode = paste0("V", FT1$VSid),
                   LEstratification = "N", LEseqNum = FT1$FTseqNum,
                   LEstratumName = "U", LEclustering = "N",
                   LEclusterName = "U", LEmixedTrip = "N", LEcatReg = "Lan",
                   LElocode = "ZWHRE", LElocType = "Port", LEctry = "ZW",
                   LEdate = fo$FOendDate, LEarea = fo$FOarea,
                   LEmetier6 = fo$FOmetier6,
                   LEnumTotal = FT1$FTnumTotal,
                   LEnumSamp = as.integer(nSampVes),
                   LEselectMeth = selectMeth[["LE"]],
                   LEunitName = paste0("landing", FT1$FTid),
                   LEincProb = nSampVes / FT1$FTnumTotal,
                   LEauxVarValue = FT1$FTauxVarValue,
                   LEauxVarTot = FT1$FTauxVarTot,
                   LEauxVarName = "LandWt", LEauxVarUnit = "kg",
                   LEsamp = fo$FOsamp)
  SS <- as.data.frame(h1$SS)
  SS$LEid <- FO1$FTid[match(SS$FOid, FO1$FOid)]
  SS$FOid <- NULL
  DE <- as.data.frame(h1$DE)
  DE$DEhierarchy <- 8L
  sim$obj <- simTEObject(list(DE = DE, SD = h1$SD, TE = up$TE, VS = up$VS,
                              LE = LE, SS = SS, SA = h1$SA, FM = h1$FM,
                              BV = h1$BV, CL = h1$CL))
  sim
}

simulateH10RatioData <- function(..., teInPopulationPerVessel = NULL,
                                 selectMeth = c(VS = "SRSWOR",
                                                TE = "CENSUS",
                                                FT = "CENSUS",
                                                FO = "CENSUS")) {
  args <- list(...)
  args$selectMeth <- selectMeth[c("VS", "FT", "FO")]
  sim <- do.call(simulateH1RatioData, args)
  h1 <- sim$obj
  VS <- as.data.frame(h1$VS)
  FT <- as.data.frame(h1$FT)
  # ---- TE: the trips of one vessel in one quarter ---------------------------
  q <- (as.integer(format(as.Date(FT$FTarvDate), "%m")) - 1L) %/% 3L + 1L
  key <- paste(FT$VSid, q)
  teKey <- unique(data.frame(VSid = FT$VSid, key = key))
  teKey$TEid <- seq_len(nrow(teKey))
  FT$TEid <- teKey$TEid[match(key, teKey$key)]
  teKg <- tapply(FT$FTauxVarValue, FT$TEid, sum)
  nTEves <- as.numeric(table(teKey$VSid)[as.character(teKey$VSid)])
  teTot <- if (is.null(teInPopulationPerVessel)) {
    nTEves
  } else {
    rep_len(teInPopulationPerVessel, nrow(teKey))
  }
  vesKg <- VS$VSauxVarValue[match(teKey$VSid, VS$VSid)]
  TE <- simTETable(teKey$TEid, "VSid", teKey$VSid, numTotal = teTot,
                   numSamp = nTEves,
                   kg = as.numeric(teKg[as.character(teKey$TEid)]),
                   kgTot = vesKg * teTot / nTEves,
                   selectMeth = selectMeth[["TE"]])
  nFTte <- as.numeric(table(FT$TEid)[as.character(FT$TEid)])
  FT$VSid <- NULL
  FT$FTnumTotal <- as.integer(nFTte)
  FT$FTnumSamp <- as.integer(nFTte)
  FT$FTincProb <- 1
  FT$FTauxVarTot <- as.numeric(teKg[as.character(FT$TEid)])
  # the vessel's landings over all its temporal events
  VS$VSauxVarValue <- VS$VSauxVarValue *
    (teTot / nTEves)[match(VS$VSid, teKey$VSid)]
  VS$VSauxVarTot <- sum(VS$VSauxVarValue) * unique(VS$VSnumTotal) / nrow(VS)
  DE <- as.data.frame(h1$DE)
  DE$DEhierarchy <- 10L
  sim$obj <- simTEObject(list(DE = DE, SD = h1$SD, VS = VS, TE = TE, FT = FT,
                              FO = h1$FO, SS = h1$SS, SA = h1$SA, FM = h1$FM,
                              BV = h1$BV, CL = h1$CL))
  sim
}
