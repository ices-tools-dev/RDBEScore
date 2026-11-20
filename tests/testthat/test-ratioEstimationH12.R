# AI-assisted: Yes
# Review: pending
# Notes/scope:
capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 12 (DE - SD - LO - TE - LE - SS - SA), with
# simulated data from simulateH12RatioData() (helper-simulateRelabel.R).

simulate12 <- function(...) suppressWarnings(simulateH12RatioData(...))

estimate12 <- function(obj, scheme, ...) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE, ...)
}

vsTruth12 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

allStages12 <- function(method) {
  raisingScheme(LO = stageRaising(method), TE = stageRaising(method),
                LE = stageRaising(method), hierarchy = 12)
}

test_that("on a census design every method reproduces the truth in H12", {
  sim <- simulate12(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  expect_equal(RDBEScore:::ratioStageChain(obj)$stages, c("LO", "TE", "LE"))
  schemes <- list(count = allStages12("count"), aux = allStages12("aux"),
                  prob = allStages12("prob"),
                  pooledDE = raisingScheme(DE = deRaising("ratio"),
                                           hierarchy = 12))
  for (nm in names(schemes)) {
    res <- estimate12(obj, schemes[[nm]])
    m <- vsTruth12(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
  }
  expect_true(all(c("nLocations", "nTemporalEvents", "nLandingEvents") %in%
                    names(res$domains)))
})

test_that("domains come from LE in H12", {
  sim <- simulate12(lowHierarchy = "C")
  su <- getSampleUnits(sim$obj, species = 127143)
  le <- sim$obj$LE[match(su$LEid, LEid)]
  expect_equal(su$area, le$LEarea)
  expect_equal(su$quarter,
               (as.integer(format(as.Date(le$LEdate), "%m")) - 1L) %/%
                 3L + 1L)
})

test_that("count raising of LO, TE and LE equals N / n per stage", {
  # 4 of 8 locations, all temporal events, 1 of 3 landing events per TE
  sim <- simulate12(lowHierarchy = "C", vesselsInPopulation = 8,
                    haulsPerTrip = 3, sampledHaulsPerTrip = 1)
  res <- estimate12(sim$obj, allStages12("count"))
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_LO == 2))
  expect_true(all(sw$f_TE == 1))
  expect_true(all(sw$f_LE == 3))
  expect_true(all(sw$wStages == 6))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)],
                by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("aux raising adds up to the LO aux total", {
  sim <- simulate12(lowHierarchy = "C", vesselsInPopulation = 8,
                    haulsPerTrip = 3, sampledHaulsPerTrip = 1)
  sw <- estimate12(sim$obj, allStages12("aux"))$samples[include == TRUE]
  expect_equal(sum(sw$wStages * sw$xSample),
               unique(sim$obj$LO$LOauxVarTot))
})

})
