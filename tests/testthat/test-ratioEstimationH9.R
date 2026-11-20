# AI-assisted: Yes
# Review: pending
# Notes/scope:
capture.output({  ## suppresses printing of console output when running test()

# Stage-wise raising on hierarchy 9 (DE - SD - LO - TE - SS - SA, with LE
# linked to SA and SS as information), with simulated data from
# simulateH9RatioData() (helper-simulateRelabel.R).

simulate9 <- function(...) suppressWarnings(simulateH9RatioData(...))

estimate9 <- function(obj, scheme, ...) {
  doRatioEstimation(obj, species = 127143, raising = scheme,
                    checkCodes = FALSE, ...)
}

vsTruth9 <- function(res, truth) {
  key <- c("year", "quarter", "area", "metier6")
  m <- merge(res$estimates[, c(key, "age", "N"), with = FALSE],
             truth[, c(key, "age", "N"), with = FALSE],
             by = c(key, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

allStages9 <- function(method) {
  raisingScheme(LO = stageRaising(method), TE = stageRaising(method),
                hierarchy = 9)
}

test_that("on a census design every method reproduces the truth in H9", {
  for (link in c("SA", "SS")) {
    sim <- simulate9(lowHierarchy = "C", vesselsInPopulation = 4,
                     leLink = link)
    obj <- sim$obj
    expect_equal(RDBEScore:::ratioStageChain(obj)$stages, c("LO", "TE"))
    schemes <- list(count = allStages9("count"), aux = allStages9("aux"),
                    prob = allStages9("prob"),
                    pooledDE = raisingScheme(DE = deRaising("ratio"),
                                             hierarchy = 9))
    for (nm in names(schemes)) {
      res <- estimate9(obj, schemes[[nm]])
      m <- vsTruth9(res, sim$truthAge)
      expect_equal(m$N.est, m$N.true, tolerance = 1e-6,
                   label = paste(link, nm))
    }
  }
  expect_true(all(c("nLocations", "nTemporalEvents") %in%
                    names(res$domains)))
})

test_that("domains come from the LE of the sample, else of its SS", {
  sim <- simulate9(lowHierarchy = "C")
  obj <- sim$obj
  su <- getSampleUnits(obj, species = 127143)
  le <- obj$LE[match(su$SAid, SAid)]
  expect_equal(su$area, le$LEarea)
  expect_equal(su$quarter,
               (as.integer(format(as.Date(le$LEdate), "%m")) - 1L) %/%
                 3L + 1L)
  # an LE linked by SAid wins over one linked only by SS
  first <- su$SAid[1]
  extra <- data.table::copy(obj$LE[SAid == first])
  extra[, `:=`(LEid = max(obj$LE$LEid) + 1L, SAid = NA, LEarea = "27.4.c")]
  obj$LE <- data.table::setkey(rbind(obj$LE, extra), LEid)
  su2 <- getSampleUnits(obj, species = 127143)
  expect_equal(su2[SAid == first, area], su[SAid == first, area])
})

test_that("an LE on the top-level SA gives the domain of its sub-samples", {
  sim <- simulate9(lowHierarchy = "C", subsample = TRUE)
  obj <- sim$obj
  su <- getSampleUnits(obj, species = 127143)
  expect_true(any(su$SAid != su$topSAid))
  obj$LE <- obj$LE[SAid %in% unique(su$topSAid)]
  su2 <- getSampleUnits(obj, species = 127143)
  le <- obj$LE[match(su2$topSAid, SAid)]
  expect_equal(su2$area, le$LEarea)
})

test_that("more than one LE per SS is logged when LE links by SS only", {
  sim <- simulate9(lowHierarchy = "C", leLink = "SS")
  obj <- sim$obj
  extra <- data.table::copy(obj$LE[1])
  extra[, `:=`(LEid = max(obj$LE$LEid) + 1L, LEarea = "27.4.c")]
  obj$LE <- data.table::setkey(rbind(obj$LE, extra), LEid)
  su <- getSampleUnits(obj, species = 127143)
  expect_true(any(grepl("More than one LE per SSid",
                        getRatioLog(su)$message)))
})

test_that("without LE there is no date in H9, and that is logged", {
  sim <- simulate9(lowHierarchy = "C")
  obj <- sim$obj
  obj$LE <- obj$LE[0]
  su <- getSampleUnits(obj, species = 127143)
  expect_true(all(is.na(su$quarter)))
  expect_true(all(su$year == obj$DE$DEyear))
  expect_true(any(grepl("No date", getRatioLog(su)$message)))
})

test_that("count raising of LO and TE equals N / n per stage", {
  # 4 of 8 locations, 1 of 2 temporal events per location
  sim <- simulate9(lowHierarchy = "C", vesselsInPopulation = 8,
                   sampledHaulsPerTrip = c(1, 0))
  expect_true(all(sim$obj$TE$TEnumSamp == 1))
  res <- estimate9(sim$obj, allStages9("count"))
  sw <- res$samples[include == TRUE]
  expect_true(all(sw$f_LO == 2))
  expect_true(all(sw$f_TE == 2))
  expect_true(all(sw$wStages == 4))
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages)],
                by = "SAid")[, sum(w * yN)]
  expect_equal(sum(res$estimates$N), hand)
})

test_that("stages that are not in hierarchy 9 stop", {
  expect_error(raisingScheme(LE = stageRaising("count"), hierarchy = 9),
               "LE")
})

})
