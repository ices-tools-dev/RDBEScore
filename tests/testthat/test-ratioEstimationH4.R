# AI-assisted: Yes
# Review: pending
# Notes/scope:
capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 4 (DE - SD - OS - FT - LE - SS - SA), with
# simulated data from simulateH4RatioData() (helper-simulateRelabel.R).

simulate4 <- function(...) suppressWarnings(simulateH4RatioData(...))

estimate4 <- function(obj, scheme, ...) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE, ...)
}

vsTruth4 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

allStages4 <- function(method) {
  raisingScheme(OS = stageRaising(method), FT = stageRaising(method),
                LE = stageRaising(method), hierarchy = 4)
}

test_that("on a census design every method reproduces the truth in H4", {
  sim <- simulate4(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  expect_equal(RDBEScore:::ratioStageChain(obj)$stages, c("OS", "FT", "LE"))
  schemes <- list(count = allStages4("count"), aux = allStages4("aux"),
                  prob = allStages4("prob"),
                  pooledDE = raisingScheme(DE = deRaising("ratio"),
                                           hierarchy = 4))
  for (nm in names(schemes)) {
    res <- estimate4(obj, schemes[[nm]])
    m <- vsTruth4(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
  }
  expect_true(all(c("nOnshoreEvents", "nTrips", "nLandingEvents") %in%
                    names(res$domains)))
})

test_that("domains come from LE in H4", {
  sim <- simulate4(lowHierarchy = "C")
  su <- getSampleUnits(sim$obj, species = 127143)
  le <- sim$obj$LE[match(su$LEid, LEid)]
  expect_equal(su$area, le$LEarea)
  expect_equal(su$metier6, le$LEmetier6)
  expect_equal(su$quarter,
               (as.integer(format(as.Date(le$LEdate), "%m")) - 1L) %/%
                 3L + 1L)
})

test_that("count raising of OS, FT and LE equals N / n per stage", {
  # 4 of 8 onshore events, all trips, 1 of 3 landing events per trip
  sim <- simulate4(lowHierarchy = "C", vesselsInPopulation = 8,
                   haulsPerTrip = 3, sampledHaulsPerTrip = 1)
  res <- estimate4(sim$obj, allStages4("count"))
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_OS == 2))
  expect_true(all(sw$f_FT == 1))
  expect_true(all(sw$f_LE == 3))
  expect_true(all(sw$wStages == 6))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)],
                by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("a landing event without the species is a zero only if SS says so", {
  sim <- simulate4(lowHierarchy = "C", vesselsInPopulation = 4,
                   haulsPerTrip = 3)
  obj <- sim$obj
  sa1 <- obj$SA[SSid %in% obj$SS[LEid == 1, SSid], SAid]
  obj$SA <- obj$SA[!SAid %in% sa1]
  obj$BV <- obj$BV[!SAid %in% sa1]
  sc <- allStages4("count")
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_true(1 %in% attr(sw, "unitWeights")$LE$LEid)
  expect_equal(unique(sw[include == TRUE & FTid == 1, f_LE]), 1)
  obj$SS[LEid == 1, SSuseCalcZero := "N"]
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_false(1 %in% attr(sw, "unitWeights")$LE$LEid)
  expect_equal(unique(sw[include == TRUE & FTid == 1, f_LE]), 3 / 2)
})

test_that("stages that are not in hierarchy 4 stop", {
  expect_error(raisingScheme(VS = stageRaising("count"), hierarchy = 4),
               "VS")
  expect_error(raisingScheme(FO = stageRaising("count"), hierarchy = 4),
               "FO")
})

})
