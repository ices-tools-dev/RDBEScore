capture.output({  ## suppresses printing of console output when running test()

# ---------------------------------------------------------------------------
# computeStageFactors(): the generic engine
# ---------------------------------------------------------------------------

hauls <- data.frame(FOid = 1:7,
                    FTid = c(1, 1, 1, 2, 2, 2, 2),
                    FOstratumName = c("D", "D", "N", "D", "D", "N", "N"),
                    FOnumTotal = c(6, 6, 2, 4, 4, 3, 3),
                    FOnumSamp = c(2, 2, 1, 2, 2, 2, 2),
                    FOauxVarTot = c(30, 30, 5, 20, 20, 9, 9),
                    FOauxVarValue = c(4, 6, 5, 3, 7, 2, 4),
                    FOincProb = c(0.5, 0.5, 0.5, 0.25, 0.25, 0.4, 0.4),
                    sampled = TRUE)

test_that("count gives N / n per parent x stratum", {
  f <- computeStageFactors(hauls, id = "FOid",
                           by = c("FTid", "FOstratumName"),
                           total = "FOnumTotal")
  expect_equal(f[FOid == 1, fStage], 6 / 2)
  expect_equal(f[FOid == 3, fStage], 2 / 1)
  expect_equal(f[FOid == 4, fStage], 4 / 2)
  expect_equal(f[FOid == 6, fStage], 3 / 2)
  expect_equal(f$n, c(2, 2, 1, 2, 2, 2, 2))
})

test_that("aux gives total / sum of sampled values", {
  f <- computeStageFactors(hauls, id = "FOid",
                           by = c("FTid", "FOstratumName"),
                           total = "FOauxVarTot", value = "FOauxVarValue")
  expect_equal(f[FOid == 1, fStage], 30 / (4 + 6))
  expect_equal(f[FOid == 6, fStage], 9 / (2 + 4))
  # weighted values add up to the total in every group
  m <- merge(f, hauls, by = "FOid")
  chk <- m[, .(est = sum(fStage * FOauxVarValue), tot = FOauxVarTot[1]),
           by = .(FTid.x, FOstratumName.x)]
  expect_equal(chk$est, chk$tot)
})

test_that("strata = NULL pools the strata of a parent", {
  h <- hauls
  h$FOnumTotal <- c(8, 8, 8, 7, 7, 7, 7)
  f <- computeStageFactors(h, id = "FOid", by = "FTid", total = "FOnumTotal")
  expect_equal(unique(f[FTid == 1, fStage]), 8 / 3)
  expect_equal(unique(f[FTid == 2, fStage]), 7 / 4)
})

test_that("prob gives 1 / inclusion probability, none gives 1", {
  f <- computeStageFactors(hauls, id = "FOid", by = "FTid",
                           value = "FOincProb", method = "prob")
  expect_equal(f$fStage, 1 / hauls$FOincProb)
  f0 <- computeStageFactors(hauls, id = "FOid", by = "FTid", method = "none")
  expect_true(all(f0$fStage == 1))
})

test_that("units not sampled are left out of n but keep the group total", {
  h <- hauls
  h$sampled <- c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  f <- computeStageFactors(h, id = "FOid", by = c("FTid", "FOstratumName"),
                           total = "FOnumTotal", sampled = "sampled")
  expect_false(2 %in% f$FOid)
  expect_equal(f[FOid == 1, fStage], 6 / 1)
})

test_that("a different reported numSamp is logged, not used", {
  h <- hauls
  h$FOnumSamp[1:2] <- 3
  f <- computeStageFactors(h, id = "FOid", by = c("FTid", "FOstratumName"),
                           total = "FOnumTotal", numSamp = "FOnumSamp")
  expect_equal(f[FOid == 1, fStage], 6 / 2)
  expect_true(any(grepl("differs from 'FOnumSamp'",
                        getRatioLog(f)$message)))
})

test_that("the engine stops on inconsistent or missing information", {
  h <- hauls
  h$FOnumTotal[2] <- 7
  expect_error(computeStageFactors(h, id = "FOid",
                                   by = c("FTid", "FOstratumName"),
                                   total = "FOnumTotal"),
               "differs within a group")
  h <- hauls
  h$FOnumTotal[3] <- NA
  expect_error(computeStageFactors(h, id = "FOid",
                                   by = c("FTid", "FOstratumName"),
                                   total = "FOnumTotal"), "missing in")
  h <- hauls
  h$FOauxVarValue[1] <- NA
  expect_error(computeStageFactors(h, id = "FOid",
                                   by = c("FTid", "FOstratumName"),
                                   total = "FOauxVarTot",
                                   value = "FOauxVarValue"),
               "missing for 1 sampled")
  h <- hauls
  h$sampled[3] <- FALSE
  expect_error(computeStageFactors(h, id = "FOid",
                                   by = c("FTid", "FOstratumName"),
                                   total = "FOnumTotal", sampled = "sampled"),
               "positive total but no sampled units")
  h <- hauls
  h$FOincProb[1] <- 1.5
  expect_error(computeStageFactors(h, id = "FOid", by = "FTid",
                                   value = "FOincProb", method = "prob"),
               "outside \\(0, 1\\]")
  expect_error(computeStageFactors(hauls, id = "FOid", by = "FTid",
                                   total = "nope"), "not found: nope")
})

# ---------------------------------------------------------------------------
# Constructors
# ---------------------------------------------------------------------------

test_that("raisingScheme fills in the stage defaults", {
  sc <- raisingScheme(FO = stageRaising("count"), FT = stageRaising("aux"),
                      VS = stageRaising("prob"))
  expect_s3_class(sc, "RDBESRaisingScheme")
  expect_equal(sc$stages$FO$total, "FOnumTotal")
  expect_equal(sc$stages$FO$value, 1)
  expect_equal(sc$stages$FO$strata, "FOstratumName")
  expect_equal(sc$stages$FT$total, "FTauxVarTot")
  expect_equal(sc$stages$FT$value, "FTauxVarValue")
  expect_equal(sc$stages$VS$value, "VSincProb")
  expect_equal(names(sc$stages), c("VS", "FT", "FO"))
  expect_output(print(sc), "SS: always 1")
})

test_that("stages not given are none, and DE none needs all stages raised", {
  sc <- raisingScheme(FO = stageRaising("count"),
                      DE = deRaising("ratio"))
  expect_equal(sc$stages$VS$method, "none")
  expect_error(raisingScheme(FO = stageRaising("count")),
               "every stage must be raised")
})

test_that("constructors reject wrong input", {
  expect_error(raisingScheme(SS = stageRaising("count")), "always 1")
  expect_error(raisingScheme(XX = stageRaising("count")), "Unknown stage")
  expect_error(raisingScheme(stageRaising("count")), "Name every stage")
  expect_error(stageRaising("ratio", total = "FTauxVarTot"),
               "needs both total and value")
  expect_error(stageRaising("count", total = "FTnumTotal"),
               "sets total and value itself")
  expect_error(stageRaising("external", total = "A", value = "B"),
               "needs an externalAux")
  expect_error(deRaising("ratio", value = "speciesLiveWeight", at = "FT"),
               "use at = \"SA\"")
  expect_error(raisingScheme(hierarchy = 14),
               "not an RDBES upper hierarchy")
  expect_error(deRaising("ratio", value = "x", at = "fo"), "stage table")
  expect_error(raisingScheme(DE = deRaising("ratio", value = "x",
                                              at = "LE")),
               "not a stage of hierarchy 1")
})

test_that("ratio and external accept columns, functions and user tables", {
  days <- function(u) as.numeric(as.Date(u$FTarvDate) -
                                   as.Date(u$FTdepDate)) + 1
  s1 <- stageRaising("ratio", total = "FTauxVarTot", value = days)
  expect_true(is.function(s1$value))
  logbook <- data.frame(FTunitName = c("T1", "T2"), landKg = c(5200, 3100))
  ea <- externalAux(logbook, by = "FTunitName", col = "landKg")
  expect_s3_class(ea, "RDBESExternalAux")
  s2 <- stageRaising("external", total = "FTauxVarTot", value = ea)
  sc <- raisingScheme(FT = s2, DE = deRaising("ratio"))
  expect_output(print(sc), "external 'landKg' by FTunitName")
  expect_error(externalAux(rbind(logbook, logbook), by = "FTunitName",
                           col = "landKg"), "Duplicate keys")
  expect_error(externalAux(logbook, by = "FTid", col = "landKg"),
               "not in data: FTid")
})

# ---------------------------------------------------------------------------
# End to end on simulated data
# ---------------------------------------------------------------------------

domainKey <- c("year", "quarter", "area", "metier6")
simulate <- function(...) suppressWarnings(simulateH1RatioData(...))
estimate <- function(obj, scheme, ...) {
  suppressWarnings(doRatioEstimation(obj, species = 127143, raising = scheme,
                                     checkCodes = FALSE, ...))
}
vsTruth <- function(res, truth) {
  m <- merge(res$estimates[, c(domainKey, "age", "N"), with = FALSE],
             truth[, c(domainKey, "age", "N"), with = FALSE],
             by = c(domainKey, "age"), all = TRUE,
             suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

test_that("on a census design every method reproduces the truth", {
  sim <- simulate(lowHierarchy = "C", vesselsInPopulation = 4)
  schemes <- list(
    count = raisingScheme(FO = stageRaising("count"),
                          FT = stageRaising("count"),
                          VS = stageRaising("count")),
    aux = raisingScheme(FO = stageRaising("aux"), FT = stageRaising("aux"),
                        VS = stageRaising("aux")),
    prob = raisingScheme(FO = stageRaising("prob"), FT = stageRaising("prob"),
                         VS = stageRaising("prob")),
    pooledDE = raisingScheme(DE = deRaising("ratio")),
    countDE = raisingScheme(FO = stageRaising("count"),
                            DE = deRaising("ratio")))
  for (nm in names(schemes)) {
    res <- estimate(sim$obj, schemes[[nm]])
    m <- vsTruth(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
    expect_s3_class(res$samples, "data.table")
  }
})

test_that("count raising with no DE ratio equals the hand-computed expansion", {
  sim <- simulate(lowHierarchy = "C", haulsPerTrip = 4,
                  sampledHaulsPerTrip = 2)
  sc <- raisingScheme(FO = stageRaising("count"), FT = stageRaising("count"),
                      VS = stageRaising("count"))
  res <- estimate(sim$obj, sc)
  sw <- res$samples
  # VS 8 / 4, FT 2 / 2, FO 4 / 2
  expect_true(all(sw[include == TRUE, wStages] == 4))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)], by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
  expect_true(any(grepl("selection method", res$log$message)))
})

test_that("aux raising adds the sampled weights up to the aux total", {
  sim <- simulate(lowHierarchy = "C", haulsPerTrip = 4,
                  sampledHaulsPerTrip = c(1, 3))
  sc <- raisingScheme(FO = stageRaising("aux"), FT = stageRaising("aux"),
                      VS = stageRaising("aux"))
  res <- estimate(sim$obj, sc)
  sw <- res$samples[include == TRUE]
  expect_equal(sum(sw$wStages * sw$xSample), unique(sim$obj$VS$VSauxVarTot))
})

test_that("counting hauls removes the bias of pooling under unequal sampling", {
  skip_on_cran()
  meanAge <- function(d) d[, sum(age * N) / sum(N)]
  scNone <- raisingScheme(DE = deRaising("ratio"))
  scCount <- raisingScheme(FO = stageRaising("count"),
                           DE = deRaising("ratio"))
  # Old vessels (3, 4) have all hauls sampled, young ones one haul per trip
  err <- data.table::rbindlist(lapply(1:10, function(sd) {
    sim <- simulate(seed = sd, haulsPerTrip = 6,
                    sampledHaulsPerTrip = c(1, 1, 1, 1, 6, 6, 6, 6),
                    vesselsInPopulation = 4, quarters = 1L,
                    areas = "27.4.a", vesselEffect = c(-0.5, -0.5, 0.5, 0.5))
    tr <- meanAge(sim$truthAge)
    data.table::data.table(
      none = meanAge(estimate(sim$obj, scNone)$estimates) - tr,
      count = meanAge(estimate(sim$obj, scCount)$estimates) - tr)
  }))
  expect_gt(mean(err$none), 0.5)
  expect_lt(abs(mean(err$count)), 0.15)
})

test_that("a DE ratio on trips keeps trips without the species in the sum", {
  sim <- simulate(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  zeroHauls <- obj$FO[FTid == 1, FOid]
  dropSA <- obj$SA[SSid %in% obj$SS[FOid %in% zeroHauls, SSid], SAid]
  obj$SA <- obj$SA[!SAid %in% dropSA]
  obj$BV <- obj$BV[!SAid %in% dropSA]
  # FO must be raised below a ratio on FT. All hauls are sampled: count = 1
  scFT <- raisingScheme(FO = stageRaising("count"),
                        DE = deRaising("ratio", value = "FTauxVarValue",
                                       at = "FT"))
  scSA <- raisingScheme(DE = deRaising("ratio"))
  rFT <- estimate(obj, scFT)
  rSA <- estimate(obj, scSA)
  expect_equal(rFT$ratios$DE$xWeightedStratum, sum(obj$FT$FTauxVarValue))
  expect_lt(rSA$ratios$DE$xWeightedStratum, sum(obj$FT$FTauxVarValue))
  expect_true(any(grepl("counted as zero catch", rFT$log$message)))
})

test_that("external auxiliary tables give the same result as the RDBES fields", {
  sim <- simulate(lowHierarchy = "C", haulsPerTrip = 4,
                  sampledHaulsPerTrip = c(1, 3))
  obj <- sim$obj
  scAux <- raisingScheme(FT = stageRaising("aux"), FO = stageRaising("count"),
                         DE = deRaising("ratio"))
  logbook <- data.frame(FTid = obj$FT$FTid, landKg = obj$FT$FTauxVarValue)
  stats <- data.frame(DEstratumName = obj$DE$DEstratumName,
                      landKg = obj$DE$DEauxVarTot)
  scExt <- raisingScheme(
    FT = stageRaising("external", total = "FTauxVarTot",
                      value = externalAux(logbook, by = "FTid",
                                          col = "landKg")),
    FO = stageRaising("count"),
    DE = deRaising("ratio", total = externalAux(stats,
                                                  by = "DEstratumName",
                                                  col = "landKg")))
  a <- estimate(obj, scAux)
  b <- estimate(obj, scExt)
  expect_equal(b$estimates$N, a$estimates$N)

  # unmatched external rows are logged
  extra <- rbind(logbook, data.frame(FTid = 999, landKg = 1))
  scExtra <- raisingScheme(
    FT = stageRaising("external", total = "FTauxVarTot",
                      value = externalAux(extra, by = "FTid",
                                          col = "landKg")),
    DE = deRaising("ratio"))
  expect_true(any(grepl("match no unit", estimate(obj, scExtra)$log$message)))

  # a sampled trip without an external value stops the run
  short <- logbook[-1, ]
  scShort <- raisingScheme(
    FT = stageRaising("external", total = "FTauxVarTot",
                      value = externalAux(short, by = "FTid",
                                          col = "landKg")),
    DE = deRaising("ratio"))
  expect_error(estimate(obj, scShort), "missing for 1 sampled")

  # keys must exist in the unit table
  scBadKey <- raisingScheme(
    FT = stageRaising("external", total = "FTauxVarTot",
                      value = externalAux(data.frame(nokey = 1, landKg = 1),
                                          by = "nokey", col = "landKg")),
    DE = deRaising("ratio"))
  expect_error(estimate(obj, scBadKey), "not in the unit table")
})

test_that("zeros stay in the sampled hauls, unusable hauls are non-response", {
  sim <- simulate(lowHierarchy = "C", haulsPerTrip = 4,
                  vesselsInPopulation = 4)
  obj <- sim$obj
  saOf <- function(foid) obj$SA[SSid %in% obj$SS[FOid == foid, SSid], SAid]
  # Trip 1 has hauls 1 to 4, all sampled:
  # haul 1: species observed and usable
  # haul 2: no sample of the species (not observed)
  sa2 <- saOf(2)
  obj$BV <- obj$BV[!SAid %in% sa2]
  obj$SA <- obj$SA[!SAid %in% sa2]
  # haul 3: recorded zero, written as generateZerosUsingSL() does
  sa3 <- saOf(3)
  obj$BV <- obj$BV[!SAid %in% sa3]
  obj$SA[SAid %in% sa3, `:=`(SAtotalWtMes = 0L, SAsampWtMes = 0L,
                             SAlowHierarchy = "D", SAsamp = "N")]
  # haul 4: species observed but the sample is unusable (lower hierarchy D)
  sa4 <- saOf(4)
  obj$BV <- obj$BV[!SAid %in% sa4]
  obj$SA[SAid %in% sa4, SAlowHierarchy := "D"]

  su <- getSampleUnits(obj, species = 127143)
  expect_equal(su[SAid %in% sa3, excludeReason],
               "recorded zero (weight 0)")
  expect_true(any(grepl("Recorded zeros", getRatioLog(su)$message)))
  expect_false(any(grepl("Excluded.*recorded zero",
                         getRatioLog(su)$message)))

  sc <- raisingScheme(FO = stageRaising("count"), DE = deRaising("ratio"))
  sw <- getStageWeights(obj, su, sc)
  # hauls 1, 2 and 3 are sampled, haul 4 is non-response: 4 / 3
  expect_equal(unique(sw[FTid == 1 & include == TRUE, f_FO]), 4 / 3)
  fo <- attr(sw, "unitWeights")$FO
  expect_true(all(c(1, 2, 3) %in% fo$FOid))
  expect_false(4 %in% fo$FOid)
  msg <- getRatioLog(sw)$message
  expect_true(any(grepl("all excluded: treated as not sampled", msg)))
  expect_true(any(grepl("with a recorded zero", msg)))
  expect_true(any(grepl("without a sample of the species", msg)))
  # other trips are unaffected
  expect_true(all(sw[FTid != 1 & include == TRUE, f_FO] == 1))
})

test_that("zeros in the 0.4.0 format of generateZerosUsingSL() are zeros", {
  # Since RDBEScore 0.4.0 a zero row has a sampled weight of 0 and no total
  # weight. A row of generateNAsUsingSL() has no weights at all.
  sim <- simulate(lowHierarchy = "C", haulsPerTrip = 3,
                  vesselsInPopulation = 4)
  obj <- sim$obj
  saOf <- function(foid) obj$SA[SSid %in% obj$SS[FOid == foid, SSid], SAid]
  sa2 <- saOf(2)
  obj$BV <- obj$BV[!SAid %in% sa2]
  obj$SA[SAid %in% sa2, `:=`(SAtotalWtLive = NA, SAtotalWtMes = NA,
                             SAsampWtLive = 0, SAsampWtMes = 0L,
                             SAlowHierarchy = "D", SAsamp = "N",
                             SAspecState = "Unknown")]
  sa3 <- saOf(3)
  obj$BV <- obj$BV[!SAid %in% sa3]
  obj$SA[SAid %in% sa3, `:=`(SAtotalWtLive = NA, SAtotalWtMes = NA,
                             SAsampWtLive = NA, SAsampWtMes = NA,
                             SAlowHierarchy = "D", SAsamp = "N")]
  su <- getSampleUnits(obj, species = 127143)
  expect_equal(su[SAid %in% sa2, excludeReason], "recorded zero (weight 0)")
  expect_false(su[SAid %in% sa3, excludeReason] == "recorded zero (weight 0)")
  sc <- raisingScheme(FO = stageRaising("count"), DE = deRaising("ratio"))
  sw <- getStageWeights(obj, su, sc)
  fo <- attr(sw, "unitWeights")$FO
  # haul 2 is a zero and stays sampled, haul 3 (an NA row) is non-response
  expect_true(2 %in% fo$FOid)
  expect_false(3 %in% fo$FOid)
  expect_equal(unique(sw[FTid == 1 & include == TRUE, f_FO]), 3 / 2)
})

test_that("rows made by generateZerosUsingSL() itself are read as zeros", {
  # Simulated data with a species list, plaice removed from haul 2, and the
  # zero row added by the package function, whatever its current format.
  sim <- simulate(lowHierarchy = "C", haulsPerTrip = 3,
                  vesselsInPopulation = 4)
  lst <- lapply(sim$obj[!vapply(sim$obj, is.null, logical(1))],
                as.data.frame)
  lst$SS$SLid <- 1L
  lst$SL <- data.frame(SLid = 1L, SLrecType = "SL", SLcou = "ZW",
                       SLinst = "4484", SLspeclistName = "Sim",
                       SLyear = 2024L, SLcatchFrac = "Lan")
  lst$IS <- data.frame(ISid = 1L, SLid = 1L, ISrecType = "IS",
                       IScommTaxon = 127143L, ISsppCode = 127143L)
  sa2 <- lst$SA$SAid[lst$SA$SSid %in% lst$SS$SSid[lst$SS$FOid == 2]]
  lst$SA <- lst$SA[!lst$SA$SAid %in% sa2, ]
  lst$BV <- lst$BV[!lst$BV$SAid %in% sa2, ]
  obj <- withCallingHandlers(
    createRDBESDataObject(lst, castToCorrectDataTypes = TRUE,
                          addMissingColumns = TRUE, strict = TRUE),
    warning = function(w) {
      if (grepl("bypasses", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    })
  z <- generateZerosUsingSL(obj)
  newSA <- setdiff(z$SA$SAid, obj$SA$SAid)
  expect_length(newSA, 1)
  su <- getSampleUnits(z, species = 127143)
  expect_equal(su[SAid %in% newSA, excludeReason], "recorded zero (weight 0)")
  sw <- getStageWeights(z, su, raisingScheme(FO = stageRaising("count"),
                                             DE = deRaising("ratio")))
  # haul 2 counts as sampled with zero catch: all 3 hauls of trip 1 sampled
  expect_true(2 %in% attr(sw, "unitWeights")$FO$FOid)
  expect_equal(unique(sw[FTid == 1 & include == TRUE, f_FO]), 1)
})

test_that("a haul without the species is a zero only if SS says it was looked for", {
  sim <- simulate(lowHierarchy = "C", haulsPerTrip = 4,
                  vesselsInPopulation = 4)
  obj <- sim$obj
  # haul 2 of trip 1 has no sample of the species
  sa2 <- obj$SA[SSid %in% obj$SS[FOid == 2, SSid], SAid]
  obj$BV <- obj$BV[!SAid %in% sa2]
  obj$SA <- obj$SA[!SAid %in% sa2]
  sc <- raisingScheme(FO = stageRaising("count"), DE = deRaising("ratio"))
  fTrip1 <- function(o) {
    sw <- getStageWeights(o, getSampleUnits(o, species = 127143), sc)
    list(f = unique(sw[FTid == 1 & include == TRUE, f_FO]),
         msg = getRatioLog(sw)$message)
  }
  # SSuseCalcZero "Y" and no SL/IS: zero, logged as an assumption
  r <- fTrip1(obj)
  expect_equal(r$f, 1)
  expect_true(any(grepl("no SL/IS in the object", r$msg)))
  # SSuseCalcZero "N": not looked for, non-response
  oN <- data.table::copy(obj)
  oN$SS[FOid == 2, SSuseCalcZero := "N"]
  r <- fTrip1(oN)
  expect_equal(r$f, 4 / 3)
  expect_true(any(grepl("SSuseCalcZero is not \"Y\": not looked for", r$msg)))
  # SSuseCalcZero "Y" with a species list
  withList <- function(o, taxon) {
    o$SS[, SLid := 1L]
    o$SL <- data.table::data.table(SLid = 1L, SLrecType = "SL", SLcou = "ZW",
                                   SLinst = "4484", SLspeclistName = "Sim",
                                   SLyear = 2024L, SLcatchFrac = "Lan")
    o$IS <- data.table::data.table(ISid = 1L, SLid = 1L, ISrecType = "IS",
                                   IScommTaxon = taxon, ISsppCode = taxon)
    data.table::setkey(o$SL, SLid)
    data.table::setkey(o$IS, ISid)
    o
  }
  r <- fTrip1(withList(data.table::copy(obj), 127143L))
  expect_equal(r$f, 1)
  expect_true(any(grepl("is on the species list", r$msg)))
  r <- fTrip1(withList(data.table::copy(obj), 126436L))
  expect_equal(r$f, 4 / 3)
  expect_true(any(grepl("not on the species list", r$msg)))
})

test_that("species are AphiaIDs, and rows without an FAO code are found", {
  sim <- simulate(lowHierarchy = "C", haulsPerTrip = 4,
                  vesselsInPopulation = 4)
  obj <- sim$obj
  expect_error(getSampleUnits(obj, species = "PLE"),
               "AphiaID.*PLE = 127143")
  saOf <- function(foid) obj$SA[SSid %in% obj$SS[FOid == foid, SSid], SAid]
  # haul 2: zero row as made by generateZerosUsingSL() (FAO code NA)
  sa2 <- saOf(2)
  obj$BV <- obj$BV[!SAid %in% sa2]
  obj$SA[SAid %in% sa2, `:=`(SAspeCodeFAO = NA_character_, SAtotalWtMes = 0L,
                             SAsampWtMes = 0L, SAlowHierarchy = "D",
                             SAsamp = "N")]
  # haul 3: NA row as made by generateNAsUsingSL() (FAO code "")
  sa3 <- saOf(3)
  obj$BV <- obj$BV[!SAid %in% sa3]
  obj$SA[SAid %in% sa3, `:=`(SAspeCodeFAO = "", SAtotalWtMes = NA_integer_,
                             SAsampWtMes = NA_integer_, SAsamp = "N")]
  su <- getSampleUnits(obj, species = 127143)
  expect_true(all(c(sa2, sa3) %in% su$SAid))
  sw <- getStageWeights(obj, su, raisingScheme(FO = stageRaising("count"),
                                               DE = deRaising("ratio")))
  # hauls 1, 2 (zero) and 4 are sampled, haul 3 (not looked for) is not
  expect_equal(unique(sw[FTid == 1 & include == TRUE, f_FO]), 4 / 3)
  expect_false(3 %in% attr(sw, "unitWeights")$FO$FOid)
})

test_that("a trip without usable hauls is non-response at the trip stage", {
  sim <- simulate(lowHierarchy = "C", haulsPerTrip = 2,
                  vesselsInPopulation = 4)
  obj <- sim$obj
  # all samples of trip 2 (vessel 1) are unusable
  saTrip2 <- obj$SA[SSid %in% obj$SS[FOid %in% obj$FO[FTid == 2, FOid],
                                     SSid], SAid]
  obj$BV <- obj$BV[!SAid %in% saTrip2]
  obj$SA[SAid %in% saTrip2, SAlowHierarchy := "D"]
  su <- getSampleUnits(obj, species = 127143)
  sc <- raisingScheme(FO = stageRaising("count"), FT = stageRaising("count"),
                      VS = stageRaising("count"))
  sw <- getStageWeights(obj, su, sc)
  # vessel 1 keeps 1 of its 2 trips: FTnumTotal 2 / 1
  expect_equal(unique(sw[FTid == 1 & include == TRUE, f_FT]), 2)
  expect_false(2 %in% attr(sw, "unitWeights")$FT$FTid)
  expect_true(any(grepl("FT units without any sampled FO unit left",
                        getRatioLog(sw)$message)))
})

test_that("stage-wise raising stops on missing totals and wrong strata", {
  sim <- simulate(lowHierarchy = "C")
  obj <- sim$obj
  obj$DE$DEauxVarTot <- NA_real_
  expect_error(estimate(obj, raisingScheme(DE = deRaising("ratio"))),
               "missing in")
  expect_error(estimate(sim$obj,
                        raisingScheme(FO = stageRaising("count",
                                                        strata = "nope"),
                                      DE = deRaising("ratio"))),
               "strata column 'nope' not found")
})

}) ## end capture.output
