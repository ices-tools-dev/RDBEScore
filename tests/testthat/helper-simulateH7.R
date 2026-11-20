# AI-assisted: Yes
# Review: pending
# Notes/scope:

# Simulated hierarchy 7 data with a known truth, used to test stage-wise
# raising on onshore sampling without trips (DE - SD - OS - SS - SA, with LE
# under SS as information, not as a sampling unit).
#
# Built on simulateH1RatioData() with one haul per trip: the same fish,
# samples, truth and DE total. Each simulated haul becomes an onshore event
# (OS, for example an auction day) with one sample. Unsampled hauls are
# listed with OSsamp = "N". LE holds the date, area and metier of the sampled
# landing and links to SS through SSid. With useLE = FALSE there is no LE,
# and the area and metier are on SA instead. osStrata gives the design strata
# of OS, recycled over onshore events. OSnumTotal is the number of onshore
# events in the population of each stratum (osInPopulation in total, shared
# over the strata in proportion to the listed ones).
#
# Returns list(obj = RDBESDataObject, truthAge, truthLength, fish)
simulateH7RatioData <- function(..., useLE = TRUE, osStrata = "U",
                                osInPopulation = NULL,
                                selectMeth = c(OS = "SRSWOR")) {
  sim <- simulateH1RatioData(..., haulsPerTrip = 1,
                             selectMeth = c(VS = "CENSUS", FT = "CENSUS",
                                            FO = selectMeth[["OS"]]))
  h1 <- sim$obj
  FO1 <- h1$FO
  nOS <- nrow(FO1)
  if (is.null(osInPopulation)) osInPopulation <- nOS

  # ---- OS: onshore events, one per simulated haul ---------------------------
  osStratum <- rep_len(osStrata, nOS)
  nListed <- as.numeric(table(osStratum)[osStratum])
  sampled <- FO1$FOsamp == "Y"
  nSampStratum <- stats::ave(as.numeric(sampled), osStratum, FUN = sum)
  OS <- data.frame(OSid = FO1$FOid, SDid = h1$SD$SDid[1], OSrecType = "OS",
                   OSseqNum = seq_len(nOS),
                   OSstratification = if (length(osStrata) > 1) "Y" else "N",
                   OSlocode = "ZWHRE", OSlocName = "Harare",
                   OSlocType = "Auction", OSsamDate = FO1$FOendDate,
                   OSstratumName = osStratum, OSclustering = "N",
                   OSclusterName = "U",
                   OSnumTotal = as.integer(round(nListed * osInPopulation /
                                                   nOS)),
                   OSnumSamp = as.integer(nSampStratum),
                   OSselectMeth = selectMeth[["OS"]],
                   OSunitName = paste0("day", FO1$FOid),
                   OSincProb = nSampStratum /
                     round(nListed * osInPopulation / nOS),
                   OSauxVarValue = FO1$FOauxVarValue,
                   # total of the stratum, scaled to the population
                   OSauxVarTot = stats::ave(FO1$FOauxVarValue, osStratum,
                                            FUN = sum) * osInPopulation / nOS,
                   OSauxVarName = "LandWt", OSauxVarUnit = "kg",
                   OSsamp = FO1$FOsamp)

  SS <- as.data.frame(h1$SS)
  SS$OSid <- SS$FOid
  SS$FOid <- NULL
  SA <- as.data.frame(h1$SA)
  foOfSA <- SS$OSid[match(SA$SSid, SS$SSid)]
  LE <- NULL
  if (useLE) {
    fo <- FO1[match(SS$OSid, FO1$FOid)]
    LE <- data.frame(LEid = SS$SSid, SSid = SS$SSid, LErecType = "LE",
                     LEseqNum = 1L, LEstratification = "N",
                     LEstratumName = "U", LEclustering = "N",
                     LEclusterName = "U", LEmixedTrip = "N",
                     LEcatReg = "Lan", LElocode = "ZWHRE",
                     LElocName = "Harare", LElocType = "Auction",
                     LEctry = "ZW", LEdate = fo$FOendDate,
                     LEarea = fo$FOarea, LEmetier6 = fo$FOmetier6,
                     LEsamp = "Y", LEunitName = paste0("landing", SS$SSid))
  } else {
    fo <- FO1[match(foOfSA, FO1$FOid)]
    SA$SAarea <- fo$FOarea
    SA$SAmetier6 <- fo$FOmetier6
  }

  DE <- as.data.frame(h1$DE)
  DE$DEhierarchy <- 7L
  lst <- list(DE = DE, SD = h1$SD, OS = OS, LE = LE, SS = SS, SA = SA,
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
