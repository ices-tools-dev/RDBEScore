# AI-assisted: Yes
# Review: pending
# Notes/scope:

# Simulated data with a known truth for hierarchies 4, 9 and 12, used to
# test stage-wise raising:
#   H4:  DE - SD - OS - FT - LE - SS - SA
#   H12: DE - SD - LO - TE - LE - SS - SA
#   H9:  DE - SD - LO - TE - SS - SA, with LE linked to SA and SS
#
# All are built on simulateH1RatioData() by relabelling its three stages
# vessel (VS), trip (FT) and haul (FO): the same fish, samples, truth, DE
# total and design columns (XXnumTotal, XXnumSamp, XXincProb, XXauxVarValue,
# XXauxVarTot, XXsamp). The landing events of H4 and H12 are the simulated
# hauls, with their date, area and metier. H9 uses one haul per trip: a
# temporal event is a trip, and each sample gets an LE with the date, area
# and metier of its haul. With leLink = "SS" the LE links to SS only.
#
# Each returns list(obj = RDBESDataObject, truthAge, truthLength, fish)

simDesignCols <- c("seqNum", "stratification", "stratumName", "clustering",
                   "clusterName", "numTotal", "numSamp", "selectMeth",
                   "unitName", "incProb", "auxVarValue", "auxVarTot",
                   "auxVarName", "auxVarUnit", "samp")

# The design columns of table `from` (prefix fromP), renamed to prefix toP
simCopyDesign <- function(from, fromP, toP) {
  from <- as.data.frame(from)
  cols <- intersect(paste0(fromP, simDesignCols), names(from))
  out <- from[, cols, drop = FALSE]
  names(out) <- sub(paste0("^", fromP), toP, names(out))
  out
}

simRelabelObject <- function(lst) {
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

# Landing events made from the simulated hauls, under parent `parentCol`
simLEFromHauls <- function(FO, parentCol) {
  FO <- as.data.frame(FO)
  le <- data.frame(LEid = FO$FOid, LErecType = "LE", LEmixedTrip = "N",
                   LEcatReg = "Lan", LElocode = "ZWHRE", LElocType = "Port",
                   LEctry = "ZW", LEdate = FO$FOendDate, LEarea = FO$FOarea,
                   LEmetier6 = FO$FOmetier6)
  le[[parentCol]] <- FO$FTid
  cbind(le, simCopyDesign(FO, "FO", "LE"))
}

simulateH4RatioData <- function(...) {
  sim <- simulateH1RatioData(...)
  h1 <- sim$obj
  VS <- as.data.frame(h1$VS)
  OS <- cbind(data.frame(OSid = VS$VSid, SDid = VS$SDid, OSrecType = "OS",
                         OSlocode = "ZWHRE", OSlocName = "Harare",
                         OSlocType = "Port",
                         OSsamDate = h1$FT$FTarvDate[match(VS$VSid,
                                                           h1$FT$VSid)]),
              simCopyDesign(VS, "VS", "OS"))
  FT <- as.data.frame(h1$FT)
  FT$OSid <- FT$VSid
  FT$VSid <- NULL
  FT$FTsampType <- "PortS"
  LE <- simLEFromHauls(h1$FO, "FTid")
  SS <- as.data.frame(h1$SS)
  SS$LEid <- SS$FOid
  SS$FOid <- NULL
  DE <- as.data.frame(h1$DE)
  DE$DEhierarchy <- 4L
  sim$obj <- simRelabelObject(list(DE = DE, SD = h1$SD, OS = OS, FT = FT,
                                   LE = LE, SS = SS, SA = h1$SA, FM = h1$FM,
                                   BV = h1$BV, CL = h1$CL))
  sim
}

# LO from the simulated vessels, TE from their trips
simLOTE <- function(h1) {
  VS <- as.data.frame(h1$VS)
  FT <- as.data.frame(h1$FT)
  LO <- cbind(data.frame(LOid = VS$VSid, SDid = VS$SDid, LOrecType = "LO",
                         LOlocode = "ZWHRE",
                         LOlocName = paste0("site", VS$VSid),
                         LOlocType = "Port"),
              simCopyDesign(VS, "VS", "LO"))
  TE <- cbind(data.frame(TEid = FT$FTid, LOid = FT$VSid, TErecType = "TE",
                         TEtimeUnit = "Week"),
              simCopyDesign(FT, "FT", "TE"))
  list(LO = LO, TE = TE)
}

simulateH12RatioData <- function(...) {
  sim <- simulateH1RatioData(...)
  h1 <- sim$obj
  up <- simLOTE(h1)
  LE <- simLEFromHauls(h1$FO, "TEid")
  SS <- as.data.frame(h1$SS)
  SS$LEid <- SS$FOid
  SS$FOid <- NULL
  DE <- as.data.frame(h1$DE)
  DE$DEhierarchy <- 12L
  sim$obj <- simRelabelObject(list(DE = DE, SD = h1$SD, LO = up$LO,
                                   TE = up$TE, LE = LE, SS = SS, SA = h1$SA,
                                   FM = h1$FM, BV = h1$BV, CL = h1$CL))
  sim
}

# H11: LO from the simulated vessels, TE from their trips, FT from their
# hauls, and one LE per FT (a link table, not a sampling unit)
simulateH11RatioData <- function(...) {
  sim <- simulateH1RatioData(...)
  h1 <- sim$obj
  up <- simLOTE(h1)
  FO <- as.data.frame(h1$FO)
  FT <- cbind(data.frame(FTid = FO$FOid, TEid = FO$FTid, FTrecType = "FT",
                         FTencrVessCode = "V1", FTsampType = "PortS",
                         FTdepDate = FO$FOendDate, FTarvDate = FO$FOendDate),
              simCopyDesign(FO, "FO", "FT"))
  LE <- data.frame(LEid = FO$FOid, FTid = FO$FOid, LErecType = "LE",
                   LEseqNum = 1L, LEstratification = "N",
                   LEstratumName = "U", LEclustering = "N",
                   LEclusterName = "U", LEmixedTrip = "N", LEcatReg = "Lan",
                   LElocode = "ZWHRE", LElocType = "Port", LEctry = "ZW",
                   LEdate = FO$FOendDate, LEarea = FO$FOarea,
                   LEmetier6 = FO$FOmetier6, LEsamp = FO$FOsamp,
                   LEunitName = paste0("landing", FO$FOid))
  SS <- as.data.frame(h1$SS)
  SS$LEid <- SS$FOid
  SS$FOid <- NULL
  DE <- as.data.frame(h1$DE)
  DE$DEhierarchy <- 11L
  sim$obj <- simRelabelObject(list(DE = DE, SD = h1$SD, LO = up$LO,
                                   TE = up$TE, FT = FT, LE = LE, SS = SS,
                                   SA = h1$SA, FM = h1$FM, BV = h1$BV,
                                   CL = h1$CL))
  sim
}

simulateH9RatioData <- function(..., leLink = c("SA", "SS")) {
  leLink <- match.arg(leLink)
  sim <- simulateH1RatioData(..., haulsPerTrip = 1)
  h1 <- sim$obj
  up <- simLOTE(h1)
  FO <- as.data.frame(h1$FO)
  # a trip is sampled (TEsamp) when its one haul is
  TE <- up$TE
  TE$TEsamp <- FO$FOsamp[match(TE$TEid, FO$FTid)]
  TE$TEnumSamp <- as.integer(stats::ave(as.numeric(TE$TEsamp == "Y"),
                                        TE$LOid, FUN = sum))
  TE$TEincProb <- TE$TEnumSamp / TE$TEnumTotal
  SS <- as.data.frame(h1$SS)
  SS$TEid <- FO$FTid[match(SS$FOid, FO$FOid)]
  SA <- as.data.frame(h1$SA)
  # ---- LE: one per sample, with the date, area and metier of its haul ------
  fo <- FO[match(SS$FOid[match(SA$SSid, SS$SSid)], FO$FOid), ]
  LE <- data.frame(LEid = SA$SAid, SSid = SA$SSid, LErecType = "LE",
                   LEseqNum = 1L, LEstratification = "N",
                   LEstratumName = "U", LEclustering = "N",
                   LEclusterName = "U", LEmixedTrip = "N", LEcatReg = "Lan",
                   LElocode = "ZWHRE", LElocType = "Port", LEctry = "ZW",
                   LEdate = fo$FOendDate, LEarea = fo$FOarea,
                   LEmetier6 = fo$FOmetier6, LEsamp = "Y",
                   LEunitName = paste0("landing", SA$SAid))
  if (leLink == "SA") {
    LE$SAid <- SA$SAid
  } else {
    LE <- LE[!duplicated(LE$SSid), ]
  }
  SS$FOid <- NULL
  DE <- as.data.frame(h1$DE)
  DE$DEhierarchy <- 9L
  sim$obj <- simRelabelObject(list(DE = DE, SD = h1$SD, LO = up$LO, TE = TE,
                                   SS = SS, LE = LE, SA = SA, FM = h1$FM,
                                   BV = h1$BV, CL = h1$CL))
  sim
}
