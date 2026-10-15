capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 5 (DE - SD - OS - LE - (FT) - SS - SA),
# with simulated data from simulateH5RatioData() (helper-simulateH5.R).

simulate5 <- function(...) suppressWarnings(simulateH5RatioData(...))

estimate5 <- function(obj, scheme) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE)
}

vsTruth5 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

saOfLE <- function(obj, leid) {
  obj$SA[SSid %in% obj$SS[LEid %in% leid, SSid], SAid]
}

# ---------------------------------------------------------------------------
# Census designs reproduce the truth
# ---------------------------------------------------------------------------

test_that("on a census design every method reproduces the truth in H5", {
  sim <- simulate5(lowHierarchy = "C", vesselsInPopulation = 4)
  schemes <- list(
    count = raisingScheme(OS = stageRaising("count"),
                          LE = stageRaising("count"), hierarchy = 5),
    aux = raisingScheme(OS = stageRaising("aux"), LE = stageRaising("aux"),
                        hierarchy = 5),
    prob = raisingScheme(OS = stageRaising("prob"), LE = stageRaising("prob"),
                         hierarchy = 5),
    pooledDE = raisingScheme(DE = deRaising("ratio"), hierarchy = 5),
    countDE = raisingScheme(LE = stageRaising("count"),
                            DE = deRaising("ratio"), hierarchy = 5))
  for (nm in names(schemes)) {
    res <- estimate5(sim$obj, schemes[[nm]])
    m <- vsTruth5(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
  }
  expect_output(print(res), "hierarchy 5")
  expect_true(all(c("nOnshoreEvents", "nLandingEvents") %in%
                    names(res$domains)))
})

test_that("domains come from LEdate and LEarea when SA is blank", {
  sim <- simulate5(lowHierarchy = "C", vesselsInPopulation = 4)
  su <- getSampleUnits(sim$obj, species = 127143)
  le <- sim$obj$LE[match(su$LEid, LEid)]
  expect_equal(su$area, le$LEarea)
  expect_equal(su$metier6, le$LEmetier6)
  expect_equal(su$quarter,
               (as.integer(format(as.Date(le$LEdate), "%m")) - 1L) %/% 3L + 1L)
  expect_equal(attr(su, "hierarchy"), 5L)
})

# ---------------------------------------------------------------------------
# Sampled designs: factors per stage
# ---------------------------------------------------------------------------

test_that("count raising of auction days and landings equals the hand expansion", {
  # 4 onshore events of 8 in the population, 1 of 2 landing events sampled
  sim <- simulate5(lowHierarchy = "C", tripsPerVessel = 2,
                   sampledHaulsPerTrip = c(1, 0), vesselsInPopulation = 8)
  sc <- raisingScheme(OS = stageRaising("count"), LE = stageRaising("count"),
                      hierarchy = 5)
  res <- estimate5(sim$obj, sc)
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_OS == 2))
  expect_true(all(sw$f_LE == 2))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)], by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("OS design strata are used for the OS factor", {
  sim <- simulate5(lowHierarchy = "C", osStrata = c("A", "B"),
                   osInPopulation = 12, vesselsInPopulation = 4)
  sc <- raisingScheme(OS = stageRaising("count"), LE = stageRaising("count"),
                      hierarchy = 5)
  sw <- getStageWeights(sim$obj, getSampleUnits(sim$obj, 127143), sc)
  f <- attr(sw, "unitWeights")$OS
  expect_true(all(c("A", "B") %in% sim$obj$OS$OSstratumName))
  # each stratum: 2 sampled of 6
  expect_true(all(f$fStage == 3))
})

test_that("aux raising of landing events adds up to the OS aux total", {
  sim <- simulate5(lowHierarchy = "C", tripsPerVessel = 3,
                   sampledHaulsPerTrip = c(1, 1, 0), vesselsInPopulation = 4)
  sc <- raisingScheme(OS = stageRaising("aux"), LE = stageRaising("aux"),
                      hierarchy = 5)
  sw <- estimate5(sim$obj, sc)$samples[include == TRUE]
  expect_equal(sum(sw$wStages * sw$xSample), unique(sim$obj$OS$OSauxVarTot))
})

test_that("VD fields can be used at LE", {
  sim <- simulate5(lowHierarchy = "C", vesselsInPopulation = 4)
  sc <- raisingScheme(LE = stageRaising("ratio", total = "LEauxVarTot",
                                        value = "VDlen"),
                      DE = deRaising("ratio"), hierarchy = 5)
  sw <- getStageWeights(sim$obj, getSampleUnits(sim$obj, 127143), sc)
  le <- sim$obj$LE
  vd <- sim$obj$VD
  le[, VDlen := vd$VDlen[match(VDid, vd$VDid)]]
  hand <- le[, .(f = LEauxVarTot[1] / sum(VDlen)), by = OSid]
  f <- attr(sw, "unitWeights")$LE
  expect_equal(f$fStage, hand$f[match(f$OSid, hand$OSid)])
})

# ---------------------------------------------------------------------------
# Optional FT stage under LE
# ---------------------------------------------------------------------------

test_that("FT under LE is a stage when SS links to FT", {
  sim <- simulate5(lowHierarchy = "C", useFT = TRUE, haulsPerTrip = 2,
                   vesselsInPopulation = 4)
  obj <- sim$obj
  expect_equal(RDBEScore:::ratioStageChain(obj)$stages, c("OS", "LE", "FT"))
  sc <- raisingScheme(OS = stageRaising("count"), LE = stageRaising("count"),
                      FT = stageRaising("count"), hierarchy = 5)
  expect_equal(names(sc$stages), c("OS", "LE", "FT"))
  res <- estimate5(obj, sc)
  m <- vsTruth5(res, sim$truthAge)
  expect_equal(m$N.est, m$N.true, tolerance = 1e-6)
  expect_true("nTrips" %in% names(res$domains))

  # FT not named: pooled, logged, and only allowed with a DE ratio
  scDE <- raisingScheme(LE = stageRaising("count"),
                         DE = deRaising("ratio"), hierarchy = 5)
  res2 <- estimate5(obj, scDE)
  expect_true(any(grepl("not in the scheme, pooled", res2$log$message)))
  scNone <- raisingScheme(OS = stageRaising("count"),
                          LE = stageRaising("count"), hierarchy = 5)
  expect_error(estimate5(obj, scNone), "does not raise")
})

test_that("FT in the scheme but not in the data stops, and so do mixed links", {
  sim <- simulate5(lowHierarchy = "C", vesselsInPopulation = 4)
  sc <- raisingScheme(FT = stageRaising("count"), DE = deRaising("ratio"),
                      hierarchy = 5)
  expect_error(estimate5(sim$obj, sc), "data do not use: FT")

  simFT <- simulate5(lowHierarchy = "C", useFT = TRUE, haulsPerTrip = 2,
                     vesselsInPopulation = 4)
  obj <- simFT$obj
  first <- obj$SS$SSid[1]
  obj$SS[SSid == first, `:=`(LEid = obj$FT[FTid == obj$SS[SSid == first,
                                                           FTid], LEid],
                             FTid = NA_integer_)]
  expect_error(getSampleUnits(obj, species = 127143), "link to both LE")
})

# ---------------------------------------------------------------------------
# Zeros and non-response at LE
# ---------------------------------------------------------------------------

test_that("a landing without the species is a zero only with SSuseCalcZero Y", {
  sim <- simulate5(lowHierarchy = "C", tripsPerVessel = 2,
                   vesselsInPopulation = 4)
  obj <- sim$obj
  sa1 <- saOfLE(obj, 1)
  obj$BV <- obj$BV[!SAid %in% sa1]
  obj$SA <- obj$SA[!SAid %in% sa1]
  sc <- raisingScheme(LE = stageRaising("count"), DE = deRaising("ratio"),
                      hierarchy = 5)
  fOS1 <- function(o) {
    sw <- getStageWeights(o, getSampleUnits(o, species = 127143), sc)
    list(f = unique(sw[OSid == 1 & include == TRUE, f_LE]),
         msg = getRatioLog(sw)$message)
  }
  r <- fOS1(obj)
  expect_equal(r$f, 1)
  expect_true(any(grepl("counted as zero catch", r$msg)))
  oN <- data.table::copy(obj)
  oN$SS[LEid == 1, SSuseCalcZero := "N"]
  r <- fOS1(oN)
  expect_equal(r$f, 2)
  expect_true(any(grepl("not looked for", r$msg)))
})

test_that("an onshore event without usable landings is non-response", {
  sim <- simulate5(lowHierarchy = "C", tripsPerVessel = 2,
                   vesselsInPopulation = 8)
  obj <- sim$obj
  saOS1 <- saOfLE(obj, obj$LE[OSid == 1, LEid])
  obj$BV <- obj$BV[!SAid %in% saOS1]
  obj$SA[SAid %in% saOS1, SAlowHierarchy := "D"]
  sc <- raisingScheme(OS = stageRaising("count"), LE = stageRaising("count"),
                      hierarchy = 5)
  sw <- getStageWeights(obj, getSampleUnits(obj, species = 127143), sc)
  # 8 onshore events in the population, 3 sampled left
  expect_true(all(sw[include == TRUE, f_OS] == 8 / 3))
  expect_true(any(grepl("OS units without any sampled LE unit left",
                        getRatioLog(sw)$message)))
})

# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------

test_that("scheme and data must be the same hierarchy", {
  sim <- simulate5(lowHierarchy = "C", vesselsInPopulation = 4)
  expect_error(estimate5(sim$obj, raisingScheme(DE = deRaising("ratio"))),
               "scheme is for hierarchy 1 but the data are hierarchy 5")
  expect_error(raisingScheme(DE = deRaising("ratio", value = "x",
                                              at = "FO"), hierarchy = 5),
               "not a stage of hierarchy 5")
  expect_error(raisingScheme(FO = stageRaising("count"), hierarchy = 5),
               "Unknown stage")
})

test_that("H5Example without sample weights gives no included samples", {
  sp <- H5Example$SA$SAspeCode[1]
  su <- getSampleUnits(H5Example, species = sp, strict = FALSE)
  expect_true(all(!su$include))
  expect_true(all(grepl("missing SAtotalWtMes", su$excludeReason)))
})

}) ## end capture.output
