# AI-assisted: Yes
# Review: pending
# Notes/scope:
capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 8 (DE - SD - TE - VS - LE - SS - SA), with
# simulated data from simulateH8RatioData() (helper-simulateTE.R) and the
# package data H8ExampleEE1.

simulate8 <- function(...) suppressWarnings(simulateH8RatioData(...))

estimate8 <- function(obj, scheme, ...) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE, ...)
}

vsTruth8 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

allStages8 <- function(method) {
  raisingScheme(TE = stageRaising(method), VS = stageRaising(method),
                LE = stageRaising(method), hierarchy = 8)
}

test_that("on a census design every method reproduces the truth in H8", {
  sim <- simulate8(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  expect_equal(RDBEScore:::ratioStageChain(obj)$stages, c("TE", "VS", "LE"))
  schemes <- list(count = allStages8("count"), aux = allStages8("aux"),
                  prob = allStages8("prob"),
                  pooledDE = raisingScheme(DE = deRaising("ratio"),
                                           hierarchy = 8))
  for (nm in names(schemes)) {
    res <- estimate8(obj, schemes[[nm]])
    m <- vsTruth8(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
  }
  expect_true(all(c("nTemporalEvents", "nVessels", "nLandingEvents") %in%
                    names(res$domains)))
})

test_that("domains come from LE in H8", {
  sim <- simulate8(lowHierarchy = "C")
  su <- getSampleUnits(sim$obj, species = 127143)
  le <- sim$obj$LE[match(su$LEid, LEid)]
  expect_equal(su$area, le$LEarea)
  expect_equal(su$metier6, le$LEmetier6)
  expect_equal(su$quarter,
               (as.integer(format(as.Date(le$LEdate), "%m")) - 1L) %/%
                 3L + 1L)
})

test_that("count raising of TE, VS and LE equals N / n per stage", {
  # 2 of 4 temporal events, 2 of 4 vessels per TE, 1 of 2 landing events
  # per vessel
  sim <- simulate8(lowHierarchy = "C", nTE = 2, teInPopulation = 4,
                   vesselsInPopulation = 8, sampledHaulsPerTrip = c(1, 0))
  res <- estimate8(sim$obj, allStages8("count"))
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_TE == 2))
  expect_true(all(sw$f_VS == 2))
  expect_true(all(sw$f_LE == 2))
  expect_true(all(sw$wStages == 8))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)],
                by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("a landing event without the species is a zero only if SS says so", {
  sim <- simulate8(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  sa1 <- obj$SA[SSid %in% obj$SS[LEid == 1, SSid], SAid]
  obj$SA <- obj$SA[!SAid %in% sa1]
  obj$BV <- obj$BV[!SAid %in% sa1]
  sc <- allStages8("count")
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_true(1 %in% attr(sw, "unitWeights")$LE$LEid)
  expect_equal(unique(sw[include == TRUE & VSid == 1, f_LE]), 1)
  obj$SS[LEid == 1, SSuseCalcZero := "N"]
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_false(1 %in% attr(sw, "unitWeights")$LE$LEid)
  expect_equal(unique(sw[include == TRUE & VSid == 1, f_LE]), 2)
})

test_that("H8ExampleEE1 runs with counts per stage", {
  H8DataObj <- H8ExampleEE1
  sc <- allStages8("count")
  expect_no_warning(
    res <- doRatioEstimation(H8DataObj, species = 126425, raising = sc,
                             checkCodes = FALSE))
  sw <- res$samples[include == TRUE]
  expect_equal(nrow(sw), nrow(H8DataObj$SA))
  te <- H8DataObj$TE[match(sw$TEid, TEid)]
  expect_equal(sw$f_TE, te$TEnumTotal / te$TEnumSamp)
  # "count" divides by the sampled vessels found in the data, per TE and
  # stratum. In this data some TEs have 2 VS records that both say
  # VSnumSamp = 1, so the recorded VSnumSamp is not used.
  vs <- data.table::copy(H8DataObj$VS)
  vs[, nFound := .N, by = .(TEid, VSstratumName)]
  expect_true(any(vs$nFound != vs$VSnumSamp))
  vs <- vs[match(sw$VSid, VSid)]
  expect_equal(sw$f_VS, vs$VSnumTotal / vs$nFound)
  expect_equal(sort(unique(res$domains$quarter)),
               sort(unique((as.integer(format(as.Date(H8DataObj$LE$LEdate),
                                               "%m")) - 1L) %/% 3L + 1L)))
})

})
