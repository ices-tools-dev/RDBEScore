# AI-assisted: Yes
# Review: pending
# Notes/scope:
capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 13 (DE - SD - FO - SS - SA), with
# simulated data from simulateH13RatioData() (helper-simulateH13.R).

simulate13 <- function(...) suppressWarnings(simulateH13RatioData(...))

estimate13 <- function(obj, scheme, ...) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE, ...)
}

vsTruth13 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

test_that("on a census design every method reproduces the truth in H13", {
  sim <- simulate13(lowHierarchy = "C", haulsPerTrip = 3)
  obj <- sim$obj
  expect_equal(RDBEScore:::ratioStageChain(obj)$stages, "FO")
  schemes <- list(
    count = raisingScheme(FO = stageRaising("count"), hierarchy = 13),
    aux = raisingScheme(FO = stageRaising("aux"), hierarchy = 13),
    prob = raisingScheme(FO = stageRaising("prob"), hierarchy = 13),
    pooledDE = raisingScheme(DE = deRaising("ratio"), hierarchy = 13))
  for (nm in names(schemes)) {
    res <- estimate13(obj, schemes[[nm]])
    m <- vsTruth13(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
  }
  expect_true("nHauls" %in% names(res$domains))
  expect_false(any(c("nVessels", "nTrips") %in% names(res$domains)))
  expect_equal(names(res$stageUnits), "FO")
})

test_that("domains come from FO in H13", {
  sim <- simulate13(lowHierarchy = "C")
  su <- getSampleUnits(sim$obj, species = 127143)
  fo <- sim$obj$FO[match(su$FOid, FOid)]
  expect_equal(su$area, fo$FOarea)
  expect_equal(su$metier6, fo$FOmetier6)
  expect_equal(su$quarter,
               (as.integer(format(as.Date(fo$FOendDate), "%m")) - 1L) %/%
                 3L + 1L)
  expect_equal(attr(su, "hierarchy"), 13L)
  expect_false(any(c("VSid", "FTid") %in% names(su)))
})

test_that("count raising of hauls equals N / n per FO stratum", {
  # 24 hauls listed, 12 per stratum, of 24 in the population. Trips sample
  # 1 and 2 of their 3 hauls in turn: 8 hauls sampled in A, 4 in B.
  sim <- simulate13(lowHierarchy = "C", haulsPerTrip = 3,
                    sampledHaulsPerTrip = c(1, 2), foStrata = c("A", "B"))
  fo <- sim$obj$FO
  expect_equal(fo[FOsamp == "Y", .N, by = FOstratumName][order(FOstratumName),
                                                          N], c(8, 4))
  sc <- raisingScheme(FO = stageRaising("count"), hierarchy = 13)
  res <- estimate13(sim$obj, sc)
  sw <- res$samples[include == TRUE]
  sw[, stratum := fo$FOstratumName[match(FOid, fo$FOid)]]
  expect_true(all(sw[stratum == "A", f_FO] == 12 / 8))
  expect_true(all(sw[stratum == "B", f_FO] == 12 / 4))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)],
                by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("aux raising adds up to the FO aux total", {
  sim <- simulate13(lowHierarchy = "C", haulsPerTrip = 3,
                    sampledHaulsPerTrip = 1, haulsInPopulation = 48)
  sc <- raisingScheme(FO = stageRaising("aux"), hierarchy = 13)
  sw <- estimate13(sim$obj, sc)$samples[include == TRUE]
  expect_equal(sum(sw$wStages * sw$xSample),
               unique(sim$obj$FO$FOauxVarTot))
})

test_that("a haul without the species is a zero only if SS says so in H13", {
  sim <- simulate13(lowHierarchy = "C", haulsPerTrip = 3)
  obj <- sim$obj
  sa2 <- obj$SA[SSid %in% obj$SS[FOid == 2, SSid], SAid]
  obj$SA <- obj$SA[!SAid %in% sa2]
  obj$BV <- obj$BV[!SAid %in% sa2]
  sc <- raisingScheme(FO = stageRaising("count"), hierarchy = 13)
  # SSuseCalcZero = "Y": haul 2 is sampled with zero catch, 24 of 24
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_true(2 %in% attr(sw, "unitWeights")$FO$FOid)
  expect_equal(unique(sw[include == TRUE, f_FO]), 1)
  # SSuseCalcZero = "N": haul 2 is non-response, 24 of 23
  obj$SS[FOid == 2, SSuseCalcZero := "N"]
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_false(2 %in% attr(sw, "unitWeights")$FO$FOid)
  expect_equal(unique(sw[include == TRUE, f_FO]), 24 / 23)
})

test_that("stages that are not in hierarchy 13 stop", {
  expect_error(raisingScheme(FT = stageRaising("count"), hierarchy = 13),
               "FT")
  expect_error(raisingScheme(VS = stageRaising("count"), hierarchy = 13),
               "VS")
})

})
