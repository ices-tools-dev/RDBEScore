capture.output({  ## suppresses printing of console output when running test()

# Within-sample factor (sampleRaising) and the auxiliary x measured on the
# samples ("speciesLiveWeight", "fishWeight", "fishCount").

simS <- function(...) suppressWarnings(simulateH1RatioData(...))
estS <- function(obj, scheme) {
  suppressWarnings(doRatioEstimation(obj, species = 127143, raising = scheme,
                                     checkCodes = FALSE))
}
unitsS <- function(obj, method) {
  suppressWarnings(getSampleUnits(obj, species = 127143,
                                  withinSample = sampleRaising(method)))
}
# Fish per sample in the simulated BV: weight (g) and count
fishOfSA <- function(obj) {
  bv <- obj$BV[BVtypeMeas == "WeightLive"]
  bv[, .(n = .N, wt = sum(as.numeric(BVvalueMeas))), by = SAid]
}

test_that("sampleRaising() takes weight, count or none", {
  expect_equal(sampleRaising()$method, "weight")
  expect_equal(sampleRaising("none")$method, "none")
  expect_error(sampleRaising("prob"))
  expect_error(raisingScheme(SA = stageRaising("count")), "sampleRaising")
  sc <- raisingScheme(SA = sampleRaising("none"), DE = deRaising("ratio"))
  expect_equal(sc$SA$method, "none")
  expect_output(print(sc), "SA \\(within sample\\): none")
})

test_that("weight is the default and keeps x as the total live weight", {
  sim <- simS(lowHierarchy = "C", boxFish = 20)
  su <- unitsS(sim$obj, "weight")
  sa <- sim$obj$SA[match(su$SAid, SAid)]
  expect_equal(su$raiseFactor, sa$SAtotalWtMes / sa$SAsampWtMes)
  expect_equal(su$x, sa$SAtotalWtMes / 1000)
  expect_equal(attr(su, "withinSample"), "weight")
})

test_that("none gives factor 1 and x = sampled live weight", {
  sim <- simS(lowHierarchy = "C", boxFish = 20)
  su <- unitsS(sim$obj, "none")
  sa <- sim$obj$SA[match(su$SAid, SAid)]
  expect_true(all(su$raiseFactor == 1))
  expect_equal(su$x, sa$SAsampWtMes / 1000)
  ex <- suppressWarnings(expandWithinSample(sim$obj, su, target = "Age"))
  # f = 1: the numbers are the fish measured
  expect_equal(ex[, sum(yN)], sum(fishOfSA(sim$obj)$n))
})

test_that("count gives SAnumTotal / SAnumSamp and x = f x sampled weight", {
  sim <- simS(lowHierarchy = "C", boxFish = 20)
  obj <- sim$obj
  nHaul <- sim$fish[, .N, by = FOid]
  # In the simulator SSid equals FOid
  obj$SA <- data.table::copy(obj$SA)
  obj$SA[, SAnumTotal := nHaul$N[match(SSid, nHaul$FOid)]]
  obj$SA[, SAnumSamp := 20]
  su <- unitsS(obj, "count")
  sa <- obj$SA[match(su$SAid, SAid)]
  expect_equal(su$raiseFactor, sa$SAnumTotal / 20)
  expect_equal(su$x, su$raiseFactor * sa$SAsampWtMes / 1000)
  # without the counts the samples are excluded with a clear reason
  su2 <- unitsS(sim$obj, "count")
  expect_true(all(!su2$include))
  expect_true(all(grepl("SAnumTotal or SAnumSamp", su2$excludeReason)))
})

test_that("sample units and scheme must use the same within-sample factor", {
  sim <- simS(lowHierarchy = "C")
  su <- unitsS(sim$obj, "none")
  expect_error(getStageWeights(sim$obj, su, raisingScheme(DE = deRaising("ratio"))),
               "withinSample")
})

test_that("fishWeight with factor none is direct raising on the fish weights", {
  sim <- simS(lowHierarchy = "C", boxFish = 20)
  obj <- sim$obj
  sc <- raisingScheme(SA = sampleRaising("none"),
                      DE = deRaising("ratio", value = "fishWeight"))
  res <- estS(obj, sc)
  fs <- fishOfSA(obj)
  sumKg <- sum(fs$wt) / 1000
  expect_equal(attr(res$raised, "deRatios")$sumWX, sumKg)
  # N_a = T x n_a / sum of fish weights (kg)
  ages <- obj$BV[BVtypeMeas == "Age", .(n = .N),
                 by = .(age = as.numeric(BVvalueMeas))]
  hand <- ages$n * obj$DE$DEauxVarTot / sumKg
  est <- res$estimates[, .(N = sum(N)), by = age]
  m <- merge(est, data.table::data.table(age = ages$age, hand = hand),
             by = "age")
  expect_equal(m$N, m$hand)
})

test_that("fishCount uses the number of fish as x", {
  sim <- simS(lowHierarchy = "C", boxFish = 20)
  sc <- raisingScheme(SA = sampleRaising("none"),
                      DE = deRaising("ratio", value = "fishCount"))
  res <- estS(sim$obj, sc)
  expect_equal(attr(res$raised, "deRatios")$sumWX,
               sum(fishOfSA(sim$obj)$n))
  # the estimated numbers then add up to the DE total
  expect_equal(sum(res$estimates$N), sim$obj$DE$DEauxVarTot)
})

test_that("fishWeight gives fish without a weight the sample mean weight", {
  sim <- simS(lowHierarchy = "C", boxFish = 20)
  obj <- sim$obj
  sa1 <- obj$SA$SAid[1]
  noW <- obj$BV[SAid == sa1 & BVtypeMeas == "WeightLive"][1:5]
  obj$BV <- obj$BV[!BVid %in% noW$BVid]
  sc <- raisingScheme(SA = sampleRaising("none"),
                      DE = deRaising("ratio", value = "fishWeight"))
  res <- estS(obj, sc)
  expect_true(any(grepl("mean weight of the weighed fish", res$log$message)))
  fs <- fishOfSA(obj)
  nAll <- obj$BV[BVtypeMeas == "Age", .N, by = SAid]
  fs[, n := nAll$N[match(SAid, nAll$SAid)]]
  fs[SAid == sa1, wt := wt * 20 / 15]
  expect_equal(attr(res$raised, "deRatios")$sumWX, sum(fs$wt) / 1000)
})

test_that("values measured on the samples need at = SA", {
  expect_error(deRaising("ratio", value = "fishWeight", at = "FT"),
               "at = \"SA\"")
  expect_error(deRaising("ratio", value = "speciesLiveWeight", at = "FO"),
               "at = \"SA\"")
})

test_that("speciesLiveWeight without SAsampWtMes stops with factor none", {
  sim <- simS(lowHierarchy = "C")
  obj <- sim$obj
  obj$SA[1, SAsampWtMes := NA]
  sc <- raisingScheme(SA = sampleRaising("none"), DE = deRaising("ratio"))
  expect_error(estS(obj, sc), "SAsampWtMes")
})

}) ## end capture.output
