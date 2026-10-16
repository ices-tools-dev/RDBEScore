# AI-assisted: Yes
# Review: pending
# Notes/scope:
capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 11 (DE - SD - LO - TE - FT - LE - SS - SA,
# with LE a link table that is not raised), with simulated data from
# simulateH11RatioData() (helper-simulateRelabel.R).

simulate11 <- function(...) suppressWarnings(simulateH11RatioData(...))

estimate11 <- function(obj, scheme, ...) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE, ...)
}

vsTruth11 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

allStages11 <- function(method) {
  raisingScheme(LO = stageRaising(method), TE = stageRaising(method),
                FT = stageRaising(method), hierarchy = 11)
}

test_that("on a census design every method reproduces the truth in H11", {
  sim <- simulate11(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  expect_equal(RDBEScore:::ratioStageChain(obj)$stages,
               c("LO", "TE", "FT", "LE"))
  schemes <- list(count = allStages11("count"), aux = allStages11("aux"),
                  prob = allStages11("prob"),
                  pooledDE = raisingScheme(DE = deRaising("ratio"),
                                           hierarchy = 11))
  for (nm in names(schemes)) {
    res <- estimate11(obj, schemes[[nm]])
    m <- vsTruth11(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
    expect_true(all(res$samples[include == TRUE, f_LE] == 1))
  }
  expect_true(all(c("nLocations", "nTemporalEvents", "nTrips") %in%
                    names(res$domains)))
})

test_that("LE is a link in H11: it cannot be raised and is not scaled", {
  expect_error(raisingScheme(LE = stageRaising("count"), hierarchy = 11),
               "not a sampling unit")
  # with DE = "none" the scheme needs LO, TE and FT only
  expect_s3_class(allStages11("count"), "RDBESRaisingScheme")
  # LE design values in the data are not used
  sim <- simulate11(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  res1 <- estimate11(obj, allStages11("count"))
  obj$LE[, `:=`(LEnumTotal = 5L, LEnumSamp = 1L)]
  res2 <- estimate11(obj, allStages11("count"))
  expect_true(all(res2$samples[include == TRUE, f_LE] == 1))
  expect_equal(sum(res2$estimates$N), sum(res1$estimates$N))
})

test_that("domains come from LE in H11, else from FT", {
  sim <- simulate11(lowHierarchy = "C")
  obj <- sim$obj
  su <- getSampleUnits(obj, species = 127143)
  le <- obj$LE[match(su$LEid, LEid)]
  expect_equal(su$area, le$LEarea)
  expect_equal(su$quarter,
               (as.integer(format(as.Date(le$LEdate), "%m")) - 1L) %/%
                 3L + 1L)
  obj$LE[, LEdate := NA_character_]
  su2 <- getSampleUnits(obj, species = 127143)
  ft <- obj$FT[match(su2$FTid, FTid)]
  expect_equal(su2$quarter,
               (as.integer(format(as.Date(ft$FTarvDate), "%m")) - 1L) %/%
                 3L + 1L)
})

test_that("count raising of LO, TE and FT equals N / n per stage", {
  # 4 of 8 locations, all temporal events, 1 of 3 trips per TE
  sim <- simulate11(lowHierarchy = "C", vesselsInPopulation = 8,
                    haulsPerTrip = 3, sampledHaulsPerTrip = 1)
  res <- estimate11(sim$obj, allStages11("count"))
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_LO == 2))
  expect_true(all(sw$f_TE == 1))
  expect_true(all(sw$f_FT == 3))
  expect_true(all(sw$wStages == 6))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)],
                by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("a trip whose LE has no species is a zero only if SS says so", {
  # 3 trips per TE, all sampled. Trip (FT) 1 loses its sample.
  sim <- simulate11(lowHierarchy = "C", vesselsInPopulation = 4,
                    haulsPerTrip = 3)
  obj <- sim$obj
  sa1 <- obj$SA[SSid %in% obj$SS[LEid == 1, SSid], SAid]
  obj$SA <- obj$SA[!SAid %in% sa1]
  obj$BV <- obj$BV[!SAid %in% sa1]
  te1 <- obj$FT[FTid == 1, TEid]
  sc <- allStages11("count")
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_true(1 %in% attr(sw, "unitWeights")$FT$FTid)
  expect_equal(unique(sw[include == TRUE & TEid == te1, f_FT]), 1)
  obj$SS[LEid == 1, SSuseCalcZero := "N"]
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_false(1 %in% attr(sw, "unitWeights")$FT$FTid)
  expect_equal(unique(sw[include == TRUE & TEid == te1, f_FT]), 3 / 2)
})

})
