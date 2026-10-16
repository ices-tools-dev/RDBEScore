# AI-assisted: Yes
# Review: pending
# Notes/scope:
capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 2 (DE - SD - FT - FO - SS - SA), with
# simulated data from simulateH2RatioData() (helper-simulateH2.R).

simulate2 <- function(...) suppressWarnings(simulateH2RatioData(...))

estimate2 <- function(obj, scheme, ...) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE, ...)
}

vsTruth2 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

saOfFO <- function(obj, foid) {
  obj$SA[SSid %in% obj$SS[FOid %in% foid, SSid], SAid]
}

# ---------------------------------------------------------------------------
# Census designs reproduce the truth
# ---------------------------------------------------------------------------

test_that("on a census design every method reproduces the truth in H2", {
  sim <- simulate2(lowHierarchy = "C", haulsPerTrip = 3)
  obj <- sim$obj
  expect_equal(RDBEScore:::ratioStageChain(obj)$stages, c("FT", "FO"))
  schemes <- list(
    count = raisingScheme(FT = stageRaising("count"),
                          FO = stageRaising("count"), hierarchy = 2),
    aux = raisingScheme(FT = stageRaising("aux"), FO = stageRaising("aux"),
                        hierarchy = 2),
    prob = raisingScheme(FT = stageRaising("prob"), FO = stageRaising("prob"),
                         hierarchy = 2),
    pooledDE = raisingScheme(DE = deRaising("ratio"), hierarchy = 2),
    countDE = raisingScheme(FO = stageRaising("count"),
                            DE = deRaising("ratio"), hierarchy = 2))
  for (nm in names(schemes)) {
    res <- estimate2(obj, schemes[[nm]])
    m <- vsTruth2(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
  }
  expect_output(print(res), "hierarchy 2")
  expect_true(all(c("nTrips", "nHauls") %in% names(res$domains)))
  expect_false("nVessels" %in% names(res$domains))
  expect_equal(names(res$stageUnits), c("FT", "FO"))
  expect_true(all(c("f_FT", "f_FO") %in% names(res$samples)))
  expect_false("f_VS" %in% names(res$samples))
})

test_that("domains come from FO, as in hierarchy 1", {
  sim <- simulate2(lowHierarchy = "C")
  su <- getSampleUnits(sim$obj, species = 127143)
  fo <- sim$obj$FO[match(su$FOid, FOid)]
  expect_equal(su$area, fo$FOarea)
  expect_equal(su$metier6, fo$FOmetier6)
  expect_equal(su$quarter,
               (as.integer(format(as.Date(fo$FOendDate), "%m")) - 1L) %/%
                 3L + 1L)
  expect_equal(attr(su, "hierarchy"), 2L)
  expect_false("VSid" %in% names(su))
})

# ---------------------------------------------------------------------------
# Sampled designs: factors per stage
# ---------------------------------------------------------------------------

test_that("count raising of trips and hauls equals the hand expansion", {
  # 8 trips sampled of 24, 1 of 3 hauls sampled in every trip
  sim <- simulate2(lowHierarchy = "C", haulsPerTrip = 3,
                   sampledHaulsPerTrip = 1, tripsInPopulation = 24)
  sc <- raisingScheme(FT = stageRaising("count"), FO = stageRaising("count"),
                      hierarchy = 2)
  res <- estimate2(sim$obj, sc)
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_FT == 3))
  expect_true(all(sw$f_FO == 3))
  expect_true(all(sw$wStages == 9))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)],
                by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("FT design strata are used for the FT factor", {
  sim <- simulate2(lowHierarchy = "C", ftStrata = c("A", "B"),
                   tripsInPopulation = 24)
  sc <- raisingScheme(FT = stageRaising("count"), FO = stageRaising("count"),
                      hierarchy = 2)
  sw <- getStageWeights(sim$obj, getSampleUnits(sim$obj, 127143), sc)
  f <- attr(sw, "unitWeights")$FT
  expect_true(all(c("A", "B") %in% sim$obj$FT$FTstratumName))
  # each stratum: 4 sampled of 12
  expect_true(all(f$fStage == 3))
})

test_that("aux raising adds up to the FT aux total", {
  sim <- simulate2(lowHierarchy = "C", haulsPerTrip = 3,
                   sampledHaulsPerTrip = c(1, 2, 3), tripsInPopulation = 16)
  sc <- raisingScheme(FT = stageRaising("aux"), FO = stageRaising("aux"),
                      hierarchy = 2)
  sw <- estimate2(sim$obj, sc)$samples[include == TRUE]
  expect_equal(sum(sw$wStages * sw$xSample),
               unique(sim$obj$FT$FTauxVarTot))
})

test_that("uneven haul sampling is corrected by counting hauls", {
  # Trips of the young-fish vessels (1, 2) have 1 of 6 hauls sampled, trips
  # of the old-fish vessels (3, 4) all 6. sampledHaulsPerTrip is per trip.
  sim <- simulate2(lowHierarchy = "C", nVessels = 4, tripsPerVessel = 2,
                   haulsPerTrip = 6,
                   sampledHaulsPerTrip = c(1, 1, 1, 1, 6, 6, 6, 6),
                   vesselEffect = c(-0.6, -0.6, 0.6, 0.6))
  meanAge <- function(d) d[, sum(age * N) / sum(N)]
  truth <- meanAge(sim$truthAge)
  pooled <- estimate2(sim$obj, raisingScheme(DE = deRaising("ratio"),
                                             hierarchy = 2))
  counted <- estimate2(sim$obj, raisingScheme(FO = stageRaising("count"),
                                              DE = deRaising("ratio"),
                                              hierarchy = 2))
  expect_lt(abs(meanAge(counted$estimates) - truth),
            abs(meanAge(pooled$estimates) - truth))
})

test_that("VD fields can be used at FT", {
  sim <- simulate2(lowHierarchy = "C", tripsInPopulation = 16)
  sc <- raisingScheme(FT = stageRaising("ratio", total = "FTauxVarTot",
                                        value = "VDlen"),
                      DE = deRaising("ratio"), hierarchy = 2)
  sw <- getStageWeights(sim$obj, getSampleUnits(sim$obj, 127143), sc)
  ft <- sim$obj$FT
  vd <- sim$obj$VD
  ft[, VDlen := vd$VDlen[match(VDid, vd$VDid)]]
  hand <- ft[, .(f = FTauxVarTot[1] / sum(VDlen)), by = FTstratumName]
  f <- attr(sw, "unitWeights")$FT
  expect_equal(unique(f$fStage), hand$f)
})

test_that("totals and a DE ratio measured on FT work in H2", {
  sim <- simulate2(lowHierarchy = "C", haulsPerTrip = 2)
  obj <- sim$obj
  ft <- obj$FT[, .(quarter = (as.integer(format(as.Date(FTarvDate), "%m")) -
                                1L) %/% 3L + 1L, FTauxVarValue)]
  tot <- ft[, .(kg = sum(FTauxVarValue)), by = quarter]
  sc <- raisingScheme(FO = stageRaising("count"),
                      DE = deRaising("ratio", value = "FTauxVarValue",
                                     at = "FT"),
                      totals = totalsRaising(
                        externalAux(tot, by = "quarter", col = "kg"),
                        value = "FTauxVarValue", at = "FT"),
                      hierarchy = 2)
  res <- estimate2(obj, sc)
  # census: every ratio is 1 and the truth is reproduced
  expect_equal(res$ratios$DE$rDE, 1, tolerance = 1e-9)
  expect_true(all(abs(res$ratios$totals$rTotals - 1) < 1e-9))
  m <- vsTruth2(res, sim$truthAge)
  expect_equal(m$N.est, m$N.true, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# Zeros and non-response at FO
# ---------------------------------------------------------------------------

test_that("a haul without the species is a zero only with SSuseCalcZero Y", {
  sim <- simulate2(lowHierarchy = "C", haulsPerTrip = 2)
  obj <- sim$obj
  sa1 <- saOfFO(obj, 1)
  obj$BV <- obj$BV[!SAid %in% sa1]
  obj$SA <- obj$SA[!SAid %in% sa1]
  sc <- raisingScheme(FO = stageRaising("count"), DE = deRaising("ratio"),
                      hierarchy = 2)
  fTrip1 <- function(o) {
    sw <- getStageWeights(o, getSampleUnits(o, species = 127143), sc)
    list(f = unique(sw[FTid == 1 & include == TRUE, f_FO]),
         msg = getRatioLog(sw)$message)
  }
  r <- fTrip1(obj)
  expect_equal(r$f, 1)
  expect_true(any(grepl("counted as zero catch", r$msg)))
  oN <- data.table::copy(obj)
  oN$SS[FOid == 1, SSuseCalcZero := "N"]
  r <- fTrip1(oN)
  expect_equal(r$f, 2)
  expect_true(any(grepl("not looked for", r$msg)))
})

test_that("a trip without usable hauls is non-response", {
  sim <- simulate2(lowHierarchy = "C", haulsPerTrip = 2,
                   tripsInPopulation = 16)
  obj <- sim$obj
  saT1 <- saOfFO(obj, obj$FO[FTid == 1, FOid])
  obj$BV <- obj$BV[!SAid %in% saT1]
  obj$SA[SAid %in% saT1, SAlowHierarchy := "D"]
  sc <- raisingScheme(FT = stageRaising("count"), FO = stageRaising("count"),
                      hierarchy = 2)
  sw <- getStageWeights(obj, getSampleUnits(obj, species = 127143), sc)
  # 16 trips in the population, 7 sampled left
  expect_true(all(sw[include == TRUE, f_FT] == 16 / 7))
  expect_true(any(grepl("FT units without any sampled FO unit left",
                        getRatioLog(sw)$message)))
})

# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------

test_that("scheme and data must be the same hierarchy in H2", {
  sim <- simulate2(lowHierarchy = "C")
  expect_error(estimate2(sim$obj, raisingScheme(DE = deRaising("ratio"))),
               "scheme is for hierarchy 1 but the data are hierarchy 2")
  expect_error(raisingScheme(VS = stageRaising("count"), hierarchy = 2),
               "Unknown stage")
  expect_error(raisingScheme(DE = deRaising("ratio", value = "x", at = "VS"),
                             hierarchy = 2),
               "not a stage of hierarchy 2")
  expect_error(raisingScheme(FO = stageRaising("count"), hierarchy = 2),
               "these stages are \"none\": FT")
})

}) ## end capture.output
