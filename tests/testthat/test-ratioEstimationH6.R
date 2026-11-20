# AI-assisted: Yes
# Review: pending
# Notes/scope:
capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 6 (DE - SD - OS - FT - FO - SS - SA, with
# LE optional under FT as information), with simulated data from
# simulateH6RatioData() (helper-simulateRelabel.R).

simulate6 <- function(...) suppressWarnings(simulateH6RatioData(...))

estimate6 <- function(obj, scheme, ...) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE, ...)
}

vsTruth6 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

allStages6 <- function(method) {
  raisingScheme(OS = stageRaising(method), FT = stageRaising(method),
                FO = stageRaising(method), hierarchy = 6)
}

test_that("on a census design every method reproduces the truth in H6", {
  sim <- simulate6(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  expect_equal(RDBEScore:::ratioStageChain(obj)$stages, c("OS", "FT", "FO"))
  schemes <- list(count = allStages6("count"), aux = allStages6("aux"),
                  prob = allStages6("prob"),
                  pooledDE = raisingScheme(DE = deRaising("ratio"),
                                           hierarchy = 6))
  for (nm in names(schemes)) {
    res <- estimate6(obj, schemes[[nm]])
    m <- vsTruth6(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
  }
  expect_true(all(c("nOnshoreEvents", "nTrips", "nFishingOperations") %in%
                    names(res$domains)))
  expect_equal(names(res$stageUnits), c("OS", "FT", "FO"))
})

test_that("domains come from FO in H6, and from LE when FO is blank", {
  sim <- simulate6(lowHierarchy = "C")
  obj <- sim$obj
  su <- getSampleUnits(obj, species = 127143)
  fo <- obj$FO[match(su$FOid, FOid)]
  expect_equal(su$area, fo$FOarea)
  expect_equal(su$metier6, fo$FOmetier6)
  expect_equal(su$quarter,
               (as.integer(format(as.Date(fo$FOendDate), "%m")) - 1L) %/%
                 3L + 1L)
  # FO area blank on one trip: the area of its LE is used
  ft1 <- su$FTid[1]
  obj$FO[FTid == ft1, FOarea := NA_character_]
  obj$LE[FTid == ft1, LEarea := "27.4.c"]
  su2 <- getSampleUnits(obj, species = 127143)
  expect_equal(unique(su2[FTid == ft1, area]), "27.4.c")
  expect_equal(su2[FTid != ft1, area], su[FTid != ft1, area])
})

test_that("H6 works without LE, and gives the same estimates", {
  withLE <- simulate6(lowHierarchy = "C")
  noLE <- simulate6(lowHierarchy = "C", useLE = FALSE)
  expect_null(noLE$obj$LE)
  sc <- allStages6("count")
  a <- estimate6(withLE$obj, sc)
  b <- estimate6(noLE$obj, sc)
  expect_equal(a$estimates, b$estimates)
})

test_that("more than one LE per trip is logged and the first LE is used", {
  sim <- simulate6(lowHierarchy = "C")
  obj <- sim$obj
  ft1 <- obj$FT$FTid[1]
  obj$FO[FTid == ft1, FOarea := NA_character_]
  extra <- data.table::copy(obj$LE[FTid == ft1])
  extra[, `:=`(LEid = max(obj$LE$LEid) + 1L, LEarea = "27.4.c")]
  obj$LE <- data.table::setkey(rbind(obj$LE, extra), LEid)
  su <- getSampleUnits(obj, species = 127143)
  expect_true(any(grepl("More than one LE per FTid",
                        getRatioLog(su)$message)))
  first <- obj$LE[FTid == ft1][1, LEarea]
  expect_equal(su[FTid == ft1, unique(area)], first)
})

test_that("count raising of OS, FT and FO equals N / n per stage", {
  # 4 of 8 onshore events, all trips, 1 of 3 fishing operations per trip
  sim <- simulate6(lowHierarchy = "C", vesselsInPopulation = 8,
                   haulsPerTrip = 3, sampledHaulsPerTrip = 1)
  res <- estimate6(sim$obj, allStages6("count"))
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_OS == 2))
  expect_true(all(sw$f_FT == 1))
  expect_true(all(sw$f_FO == 3))
  expect_true(all(sw$wStages == 6))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)],
                by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("with one FO per trip the FO factor is 1", {
  sim <- simulate6(lowHierarchy = "C", vesselsInPopulation = 8)
  expect_true(all(sim$obj$FO$FOaggLev == "T"))
  res <- estimate6(sim$obj, allStages6("count"))
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_FO == 1))
  expect_true(all(sw$f_OS == 2))
})

test_that("an FO without the species is a zero only if SS says so", {
  sim <- simulate6(lowHierarchy = "C", vesselsInPopulation = 4,
                   haulsPerTrip = 3)
  obj <- sim$obj
  sa1 <- obj$SA[SSid %in% obj$SS[FOid == 1, SSid], SAid]
  obj$SA <- obj$SA[!SAid %in% sa1]
  obj$BV <- obj$BV[!SAid %in% sa1]
  sc <- allStages6("count")
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_true(1 %in% attr(sw, "unitWeights")$FO$FOid)
  expect_equal(unique(sw[include == TRUE & FTid == 1, f_FO]), 1)
  obj$SS[FOid == 1, SSuseCalcZero := "N"]
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_false(1 %in% attr(sw, "unitWeights")$FO$FOid)
  expect_equal(unique(sw[include == TRUE & FTid == 1, f_FO]), 3 / 2)
})

test_that("stages that are not in hierarchy 6 stop", {
  expect_error(raisingScheme(VS = stageRaising("count"), hierarchy = 6),
               "VS")
  expect_error(raisingScheme(LE = stageRaising("count"), hierarchy = 6),
               "LE")
})

})
