# AI-assisted: Yes
# Review: pending
# Notes/scope:
capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 7 (DE - SD - OS - SS - SA, LE under SS as
# information), with simulated data from simulateH7RatioData()
# (helper-simulateH7.R) and the package data H7Example.

simulate7 <- function(...) suppressWarnings(simulateH7RatioData(...))

estimate7 <- function(obj, scheme, ...) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE, ...)
}

vsTruth7 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

test_that("on a census design every method reproduces the truth in H7", {
  sim <- simulate7(lowHierarchy = "C")
  obj <- sim$obj
  expect_equal(RDBEScore:::ratioStageChain(obj)$stages, "OS")
  schemes <- list(
    count = raisingScheme(OS = stageRaising("count"), hierarchy = 7),
    aux = raisingScheme(OS = stageRaising("aux"), hierarchy = 7),
    prob = raisingScheme(OS = stageRaising("prob"), hierarchy = 7),
    pooledDE = raisingScheme(DE = deRaising("ratio"), hierarchy = 7))
  for (nm in names(schemes)) {
    res <- estimate7(obj, schemes[[nm]])
    m <- vsTruth7(res, sim$truthAge)
    expect_equal(m$N.est, m$N.true, tolerance = 1e-6, label = nm)
  }
  expect_true("nOnshoreEvents" %in% names(res$domains))
  expect_equal(names(res$stageUnits), "OS")
})

test_that("domains come from LE under SS, and SA wins when filled", {
  sim <- simulate7(lowHierarchy = "C")
  obj <- sim$obj
  su <- getSampleUnits(obj, species = 127143)
  le <- obj$LE[match(su$SSid, SSid)]
  expect_equal(su$area, le$LEarea)
  expect_equal(su$metier6, le$LEmetier6)
  expect_equal(su$quarter,
               (as.integer(format(as.Date(le$LEdate), "%m")) - 1L) %/%
                 3L + 1L)
  expect_false("LEid" %in% names(su))
  # an area on SA is used before the LE area
  obj$SA[SAid == su$SAid[1], SAarea := "27.4.c"]
  su2 <- getSampleUnits(obj, species = 127143)
  expect_equal(su2[SAid == su$SAid[1], area], "27.4.c")
  expect_equal(su2[SAid != su$SAid[1], area], su[SAid != su$SAid[1], area])
})

test_that("without LE the domains come from SA and OS", {
  sim <- simulate7(lowHierarchy = "C", useLE = FALSE)
  obj <- sim$obj
  su <- getSampleUnits(obj, species = 127143)
  sa <- obj$SA[match(su$SAid, SAid)]
  os <- obj$OS[match(su$OSid, OSid)]
  expect_equal(su$area, sa$SAarea)
  expect_equal(su$quarter,
               (as.integer(format(as.Date(os$OSsamDate), "%m")) - 1L) %/%
                 3L + 1L)
})

test_that("more than one LE per SS is logged and the first LE is used", {
  sim <- simulate7(lowHierarchy = "C")
  obj <- sim$obj
  extra <- data.table::copy(obj$LE[SSid == obj$SS$SSid[1]])
  extra[, `:=`(LEid = max(obj$LE$LEid) + 1L, LEarea = "27.4.c")]
  obj$LE <- data.table::setkey(rbind(obj$LE, extra), LEid)
  su <- getSampleUnits(obj, species = 127143)
  expect_true(any(grepl("More than one LE per SSid",
                        getRatioLog(su)$message)))
  first <- obj$LE[SSid == obj$SS$SSid[1]][1, LEarea]
  expect_equal(su[SSid == obj$SS$SSid[1], unique(area)], first)
})

test_that("count raising of onshore events equals N / n", {
  # 8 onshore events listed, 4 sampled, 16 in the population
  sim <- simulate7(lowHierarchy = "C", sampledHaulsPerTrip = c(1, 0),
                   osInPopulation = 16)
  os <- sim$obj$OS
  expect_equal(os[OSsamp == "Y", .N], 4)
  sc <- raisingScheme(OS = stageRaising("count"), hierarchy = 7)
  res <- estimate7(sim$obj, sc)
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_OS == 16 / 4))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)],
                by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("an onshore event without the species is a zero only if SS says so", {
  sim <- simulate7(lowHierarchy = "C")
  obj <- sim$obj
  sa2 <- obj$SA[SSid %in% obj$SS[OSid == 2, SSid], SAid]
  obj$SA <- obj$SA[!SAid %in% sa2]
  obj$BV <- obj$BV[!SAid %in% sa2]
  sc <- raisingScheme(OS = stageRaising("count"), hierarchy = 7)
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_true(2 %in% attr(sw, "unitWeights")$OS$OSid)
  expect_equal(unique(sw[include == TRUE, f_OS]), 1)
  obj$SS[OSid == 2, SSuseCalcZero := "N"]
  sw <- getStageWeights(obj, getSampleUnits(obj, 127143), sc)
  expect_false(2 %in% attr(sw, "unitWeights")$OS$OSid)
  expect_equal(unique(sw[include == TRUE, f_OS]), 8 / 7)
})

test_that("stages that are not in hierarchy 7 stop", {
  expect_error(raisingScheme(FT = stageRaising("count"), hierarchy = 7),
               "FT")
  expect_error(raisingScheme(LE = stageRaising("count"), hierarchy = 7),
               "LE")
})

test_that("H7Example takes its domains from LE", {
  H7DataObj <- H7Example
  su <- getSampleUnits(H7DataObj, species = 151308,
                       withinSample = sampleRaising("none"))
  le <- H7DataObj$LE[match(su$SSid, SSid)]
  expect_equal(su$area, le$LEarea)
  expect_equal(su$metier6, le$LEmetier6)
  expect_false(anyNA(su$quarter))
})

})
