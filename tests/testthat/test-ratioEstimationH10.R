# AI-assisted: Yes
# Review: pending
# Notes/scope:
capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 10 (DE - SD - VS - TE - FT - FO - SS - SA),
# with simulated data from simulateH10RatioData() (helper-simulateTE.R).

simulate10 <- function(...) suppressWarnings(simulateH10RatioData(...))

estimate10 <- function(obj, scheme, ...) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE, ...)
}

vsTruth10 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

allStages10 <- function(method) {
  raisingScheme(VS = stageRaising(method), TE = stageRaising(method),
                FT = stageRaising(method), FO = stageRaising(method),
                hierarchy = 10)
}

test_that("on a census design every method reproduces the truth in H10", {
  sim <- simulate10(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  expect_equal(RDBEScore:::ratioStageChain(obj)$stages,
               c("VS", "TE", "FT", "FO"))
  schemes <- list(count = allStages10("count"), aux = allStages10("aux"),
                  prob = allStages10("prob"),
                  pooledDE = raisingScheme(DE = deRaising("ratio"),
                                           hierarchy = 10))
  for (nm in names(schemes)) {
    res <- estimate10(obj, schemes[[nm]])
    m <- vsTruth10(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
  }
  expect_equal(names(res$stageUnits), c("VS", "TE", "FT", "FO"))
  expect_true("nTemporalEvents" %in% names(res$domains))
})

test_that("TE sits between vessels and trips in H10", {
  # Each vessel has 2 trips, in quarters 1 and 2: 2 temporal events of 1
  # trip each. 2 of 4 quarters per vessel sampled, 4 of 8 vessels.
  sim <- simulate10(lowHierarchy = "C", teInPopulationPerVessel = 4)
  obj <- sim$obj
  expect_true(all(!is.na(obj$TE$VSid)))
  expect_true(all(!is.na(obj$FT$TEid)))
  res <- estimate10(obj, allStages10("count"))
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_VS == 2))
  expect_true(all(sw$f_TE == 2))
  expect_true(all(sw$f_FT == 1))
  expect_true(all(sw$wStages == 4))
  # every sample's TE belongs to its vessel
  expect_equal(obj$TE$VSid[match(sw$TEid, obj$TE$TEid)], sw$VSid)
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)],
                by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("aux raising adds up to the VS aux total in H10", {
  sim <- simulate10(lowHierarchy = "C", teInPopulationPerVessel = 4)
  sw <- estimate10(sim$obj, allStages10("aux"))$samples[include == TRUE]
  expect_equal(sum(sw$wStages * sw$xSample),
               unique(sim$obj$VS$VSauxVarTot))
})

})
