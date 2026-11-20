# AI-assisted: Yes
# Review: pending
# Notes/scope:
capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 3 (DE - SD - TE - VS - FT - FO - SS - SA),
# with simulated data from simulateH3RatioData() (helper-simulateTE.R).

simulate3 <- function(...) suppressWarnings(simulateH3RatioData(...))

estimate3 <- function(obj, scheme, ...) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE, ...)
}

vsTruth3 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

allStages3 <- function(method) {
  raisingScheme(TE = stageRaising(method), VS = stageRaising(method),
                FT = stageRaising(method), FO = stageRaising(method),
                hierarchy = 3)
}

test_that("on a census design every method reproduces the truth in H3", {
  sim <- simulate3(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  expect_equal(RDBEScore:::ratioStageChain(obj)$stages,
               c("TE", "VS", "FT", "FO"))
  schemes <- list(count = allStages3("count"), aux = allStages3("aux"),
                  prob = allStages3("prob"),
                  pooledDE = raisingScheme(DE = deRaising("ratio"),
                                           hierarchy = 3))
  for (nm in names(schemes)) {
    res <- estimate3(obj, schemes[[nm]])
    m <- vsTruth3(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
  }
  expect_true(all(c("nTemporalEvents", "nVessels", "nTrips", "nHauls") %in%
                    names(res$domains)))
  expect_equal(names(res$stageUnits), c("TE", "VS", "FT", "FO"))
})

test_that("count raising of TE and VS equals N / n per stage", {
  # 2 of 6 temporal events sampled; 2 vessels sampled of 4 in each TE
  sim <- simulate3(lowHierarchy = "C", nTE = 2, teInPopulation = 6,
                   vesselsInPopulation = 8)
  expect_true(all(sim$obj$VS$VSnumTotal == 4))
  expect_true(all(sim$obj$VS$VSnumSamp == 2))
  res <- estimate3(sim$obj, allStages3("count"))
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_TE == 3))
  expect_true(all(sw$f_VS == 2))
  expect_true(all(sw$wStages == 6))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)],
                by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("aux raising adds up to the TE aux total", {
  sim <- simulate3(lowHierarchy = "C", nTE = 2, teInPopulation = 6,
                   vesselsInPopulation = 8)
  sw <- estimate3(sim$obj, allStages3("aux"))$samples[include == TRUE]
  expect_equal(sum(sw$wStages * sw$xSample),
               unique(sim$obj$TE$TEauxVarTot))
})

test_that("stages that are not in hierarchy 3 stop", {
  expect_error(raisingScheme(OS = stageRaising("count"), hierarchy = 3),
               "OS")
  expect_error(raisingScheme(LE = stageRaising("count"), hierarchy = 3),
               "LE")
})

})
