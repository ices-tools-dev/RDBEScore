capture.output({  ## suppresses printing of console output when running test()

# Raising to totals you supply (totalsRaising), with your own strata.
# Simulated data only: the simulator's CL table holds the landed live weight
# of every haul by quarter, area, métier and size category.

simT <- function(...) suppressWarnings(simulateH1RatioData(...))
estT <- function(obj, scheme, ...) {
  suppressWarnings(doRatioEstimation(obj, species = 127143, raising = scheme,
                                     checkCodes = FALSE, ...))
}
keyT <- c("year", "quarter", "area", "metier6")
# CL landings (kg) per quarter x area x métier, optionally by size category
clTotals <- function(obj, bySize = FALSE) {
  cl <- data.table::as.data.table(obj$CL)
  by <- c("CLquar", "CLarea", "CLmetier6", if (bySize) "CLsizeCat")
  d <- cl[, .(kg = sum(CLoffWeight)), by = by]
  data.table::setnames(d, by, c("quarter", "area", "metier6",
                                if (bySize) "SAcommCat"))
  d
}
totalsOf <- function(d, cols = setdiff(names(d), "kg")) {
  totalsRaising(externalAux(d, by = cols, col = "kg"))
}

test_that("totalsRaising() checks its arguments", {
  d <- data.frame(quarter = 1:2, kg = c(10, 20))
  expect_error(totalsRaising(d), "externalAux")
  tr <- totalsRaising(externalAux(d, by = "quarter", col = "kg"))
  expect_equal(tr$strata, "quarter")
  expect_equal(tr$at, "SA")
  expect_error(totalsRaising(externalAux(d, by = "quarter", col = "kg"),
                             strata = "area"), "key columns")
  d2 <- data.frame(SAcommCat = c("1", "2"), kg = c(10, 20))
  expect_error(totalsRaising(externalAux(d2, by = "SAcommCat", col = "kg"),
                             value = "FTauxVarValue", at = "FT"),
               "need at = \"SA\"")
  expect_error(totalsRaising(externalAux(d, by = "quarter", col = "kg"),
                             value = "fishWeight", at = "FT"), "at = \"SA\"")
  expect_error(raisingScheme(totals = deRaising("ratio")), "totalsRaising")
  expect_error(raisingScheme(totals = totalsRaising(
    externalAux(d, by = "quarter", col = "kg"), value = "x", at = "OS")),
    "not a stage of hierarchy 1")
  sc <- raisingScheme(totals = tr)
  expect_output(print(sc), "totals: ratio")
})

test_that("with totals, DE none and unraised stages are allowed", {
  d <- data.frame(quarter = 1:2, kg = c(10, 20))
  expect_silent(raisingScheme(totals = totalsOf(d)))
  expect_error(raisingScheme(), "every stage must be raised")
})

test_that("on a census design the totals reproduce the truth", {
  sim <- simT(lowHierarchy = "C", vesselsInPopulation = 4)
  sc <- raisingScheme(totals = totalsOf(clTotals(sim$obj)))
  res <- estT(sim$obj, sc)
  m <- merge(res$estimates[, c(keyT, "age", "N"), with = FALSE],
             sim$truthAge[, c(keyT, "age", "N"), with = FALSE],
             by = c(keyT, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  expect_equal(m$N.est, m$N.true, tolerance = 1e-6)
  expect_false("DEstratumName" %in% names(res$byStratum))
  expect_true(all(abs(res$ratios$totals$rTotals - 1) < 1e-9))
})

test_that("the totals ratio is T_g / sum(w x) per totals stratum", {
  sim <- simT(lowHierarchy = "C", haulsPerTrip = 3, sampledHaulsPerTrip = 1)
  tot <- clTotals(sim$obj)[, .(kg = sum(kg)), by = quarter]
  sc <- raisingScheme(totals = totalsOf(tot))
  res <- estT(sim$obj, sc, domainVars = "quarter")
  sw <- res$samples[include == TRUE]
  hand <- merge(res$fishBySample, sw[, .(SAid, w = wStages, x = xSample, quarter)], by = "SAid")
  hand <- hand[, .(y = sum(w * yN), sx = sum(unique(data.table::data.table(
    SAid, w, x))[, w * x])), by = quarter]
  hand <- merge(hand, tot, by = "quarter")
  est <- res$estimates[, .(N = sum(N)), by = quarter][
    res$domains[, .(quarter, X = liveWeightEst_kg)], on = "quarter"]
  est <- merge(est, hand, by = "quarter")
  expect_equal(est$N, est$y * est$kg / est$sx)
  # the estimated live weight adds up to the totals
  expect_equal(est$X, est$kg)
})

test_that("DE then totals equals totals alone when strata nest in DE", {
  sim <- simT(lowHierarchy = "C", haulsPerTrip = 3, sampledHaulsPerTrip = 1)
  tr <- totalsOf(clTotals(sim$obj))
  a <- estT(sim$obj, raisingScheme(FO = stageRaising("count"), totals = tr))
  b <- estT(sim$obj, raisingScheme(FO = stageRaising("count"),
                                   DE = deRaising("ratio"), totals = tr))
  expect_equal(a$estimates$N, b$estimates$N)
  expect_false(isTRUE(all.equal(b$ratios$DE$rDE, 1)))
})

test_that("DE ratios weight the DE strata within a totals stratum", {
  sim <- simT(lowHierarchy = "C", haulsPerTrip = 3, sampledHaulsPerTrip = 1)
  obj <- sim$obj
  # Two DE strata: vessels 1-2 in A, 3-4 in B, each with its own DE total
  de2 <- data.table::copy(obj$DE)
  de2[, `:=`(DEid = 2L, DEstratumName = "B")]
  obj$DE <- rbind(obj$DE, de2)
  obj$DE[DEid == 1L, DEstratumName := "A"]
  sd2 <- data.table::copy(obj$SD)
  sd2[, `:=`(SDid = 2L, DEid = 2L)]
  obj$SD <- rbind(obj$SD, sd2)
  data.table::setkey(obj$DE, DEid)
  data.table::setkey(obj$SD, SDid)
  obj$VS <- data.table::copy(obj$VS)
  obj$VS[VSid %in% 3:4, SDid := 2L]
  vKg <- obj$VS[, .(kg = sum(VSauxVarValue)), by = SDid]
  obj$DE[, DEauxVarTot := vKg$kg[match(DEid, vKg$SDid)] * c(2, 1)]
  tot <- clTotals(obj)[, .(kg = sum(kg)), by = quarter]
  sc <- raisingScheme(DE = deRaising("ratio"), totals = totalsOf(tot))
  res <- estT(obj, sc, domainVars = "quarter")
  # hand: N_g = T_g * sum(r_h w y) / sum(r_h w x)
  sw <- res$samples[include == TRUE]
  rH <- res$ratios$DE
  sw[, rh := rH$rDE[match(DEstratumName, rH$DEstratumName)]]
  y <- merge(res$fishBySample, sw[, .(SAid, quarter, rh, w = wStages)], by = "SAid")[
    , .(y = sum(rh * w * yN)), by = quarter]
  x <- sw[, .(sx = sum(rh * wStages * xSample)), by = quarter]
  hand <- merge(merge(y, x, by = "quarter"), tot, by = "quarter")
  est <- merge(res$estimates[, .(N = sum(N)), by = quarter], hand,
               by = "quarter")
  expect_equal(est$N, est$kg * est$y / est$sx)
  # and it differs from raising to the totals without the DE step
  plain <- estT(obj, raisingScheme(totals = totalsOf(tot)),
                domainVars = "quarter")
  expect_false(isTRUE(all.equal(sum(plain$estimates$N), sum(est$N))))
})

test_that("strata with samples but no total stop, totals without samples are logged", {
  sim <- simT(lowHierarchy = "C")
  tot <- clTotals(sim$obj)[, .(kg = sum(kg)), by = quarter]
  expect_error(estT(sim$obj, raisingScheme(totals = totalsOf(tot[quarter == 1]))),
               "no row in the totals table: 2")
  extra <- rbind(tot, data.table::data.table(quarter = 4L, kg = 500))
  res <- estT(sim$obj, raisingScheme(totals = totalsOf(extra)))
  expect_true(any(grepl("total but no sampled units, not raised \\(sum of ",
                        res$log$message)))
})

test_that("totals strata can be SA columns such as the size category", {
  sim <- simT(lowHierarchy = "C", stratifiedSA = TRUE, boxFish = 15)
  obj <- sim$obj
  sc <- raisingScheme(SA = sampleRaising("none"),
                      totals = totalsRaising(
                        externalAux(clTotals(obj, bySize = TRUE),
                                    by = c("quarter", "area", "metier6",
                                           "SAcommCat"), col = "kg"),
                        value = "fishWeight"))
  res <- estT(obj, sc)
  # Direct raising by size category: N_a = sum_c n_ac * W_c / sum of the
  # weights of the measured fish in c (W_c in kg, fish weights in g)
  bv <- data.table::as.data.table(obj$BV)
  fish <- data.table::dcast(bv, SAid + BVfishId ~ BVtypeMeas,
                            value.var = "BVvalueMeas")
  fish[, `:=`(age = as.numeric(Age), wt = as.numeric(WeightLive))]
  sa <- obj$SA[, .(SAid, SSid, SAcommCat)]
  fo <- obj$FO[, .(FOid, area = FOarea, metier6 = FOmetier6,
                   quarter = (as.integer(format(as.Date(FOendDate), "%m")) -
                                1L) %/% 3L + 1L)]
  fish <- merge(merge(fish, sa, by = "SAid"),
                fo, by.x = "SSid", by.y = "FOid")
  cat <- fish[, .(sumW = sum(wt)), by = .(quarter, area, metier6, SAcommCat)]
  nac <- fish[, .(n = .N), by = .(quarter, area, metier6, SAcommCat, age)]
  hand <- merge(merge(nac, cat, by = c("quarter", "area", "metier6",
                                       "SAcommCat")),
                clTotals(obj, bySize = TRUE),
                by = c("quarter", "area", "metier6", "SAcommCat"))
  hand <- hand[, .(N = sum(n * kg * 1000 / sumW)),
               by = .(quarter, area, metier6, age)]
  est <- merge(res$estimates[, .(quarter, area, metier6, age, N)], hand,
               by = c("quarter", "area", "metier6", "age"),
               suffixes = c(".est", ".hand"))
  expect_equal(nrow(est), nrow(hand))
  expect_equal(est$N.est, est$N.hand)
  tr <- res$ratios$totals
  expect_true("SAcommCat" %in% names(tr))
})

test_that("totals at a stage keep units without the species in the sum", {
  sim <- simT(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  zeroHauls <- obj$FO[FTid == 1, FOid]
  dropSA <- obj$SA[SSid %in% obj$SS[FOid %in% zeroHauls, SSid], SAid]
  obj$SA <- obj$SA[!SAid %in% dropSA]
  obj$BV <- obj$BV[!SAid %in% dropSA]
  ft <- obj$FT[, .(quarter = (as.integer(format(as.Date(FTarvDate), "%m")) -
                                1L) %/% 3L + 1L, FTauxVarValue)]
  tot <- ft[, .(kg = 2 * sum(FTauxVarValue)), by = quarter]
  sc <- raisingScheme(FO = stageRaising("count"),
                      totals = totalsRaising(
                        externalAux(tot, by = "quarter", col = "kg"),
                        value = "FTauxVarValue", at = "FT"))
  res <- estT(obj, sc)
  tr <- res$ratios$totals
  hand <- ft[, .(sx = sum(FTauxVarValue)), by = quarter]
  tr <- merge(tr, hand[, .(quarter = as.character(quarter), sx)],
              by = "quarter")
  expect_equal(tr$xWeightedStratum, tr$sx)
  expect_equal(tr$rTotals, rep(2, nrow(tr)))
})

test_that("a stage unit with more than one stratum value stops", {
  sim <- simT(lowHierarchy = "C", vesselsInPopulation = 4)
  obj <- sim$obj
  obj$FO <- data.table::copy(obj$FO)
  obj$FO[FOseqNum == 2, FOarea := "27.4.c"]
  tot <- data.table::data.table(area = c("27.4.a", "27.4.b", "27.4.c"),
                                kg = 1000)
  sc <- raisingScheme(FO = stageRaising("count"),
                      totals = totalsRaising(
                        externalAux(tot, by = "area", col = "kg"),
                        value = "FTauxVarValue", at = "FT"))
  expect_error(estT(obj, sc), "more than one value of 'area'")
})

test_that("x on a stage needs every stage below it raised", {
  expect_error(raisingScheme(DE = deRaising("ratio", value = "FTauxVarValue",
                                            at = "FT")),
               "FO is \"none\"")
  d <- data.frame(quarter = 1:2, kg = c(10, 20))
  expect_error(raisingScheme(totals = totalsRaising(
    externalAux(d, by = "quarter", col = "kg"), value = "VSauxVarValue",
    at = "VS"), FT = stageRaising("count")), "FO is \"none\"")
  expect_silent(raisingScheme(FO = stageRaising("count"),
                              DE = deRaising("ratio", value = "FTauxVarValue",
                                             at = "FT")))
})

test_that("totals work in hierarchy 5, on samples and on landing events", {
  sim <- suppressWarnings(simulateH5RatioData(lowHierarchy = "C",
                                              vesselsInPopulation = 4))
  obj <- sim$obj
  sc <- raisingScheme(totals = totalsOf(clTotals(obj)), hierarchy = 5)
  res <- estT(obj, sc)
  m <- merge(res$estimates[, c(keyT, "age", "N"), with = FALSE],
             sim$truthAge[, c(keyT, "age", "N"), with = FALSE],
             by = c(keyT, "age"), all = TRUE, suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  expect_equal(m$N.est, m$N.true, tolerance = 1e-6)
  le <- obj$LE[, .(quarter = (as.integer(format(as.Date(LEdate), "%m")) -
                                1L) %/% 3L + 1L, LEauxVarValue)]
  tot <- le[, .(kg = sum(LEauxVarValue)), by = quarter]
  sc2 <- raisingScheme(totals = totalsRaising(
    externalAux(tot, by = "quarter", col = "kg"), value = "LEauxVarValue",
    at = "LE"), hierarchy = 5)
  res2 <- estT(obj, sc2)
  expect_equal(res2$ratios$totals$rTotals,
               rep(1, nrow(tot)), tolerance = 1e-9)
})
# ---------------------------------------------------------------------------
# raiseToDE() and raiseToTotals() as separate steps
# ---------------------------------------------------------------------------

test_that("raiseToDE() then raiseToTotals() equals doRatioEstimation()", {
  sim <- simT(lowHierarchy = "C", haulsPerTrip = 3, sampledHaulsPerTrip = 1)
  obj <- sim$obj
  sc <- raisingScheme(FO = stageRaising("count"), DE = deRaising("ratio"),
                      totals = totalsOf(clTotals(obj)))
  su <- getSampleUnits(obj, species = 127143)
  ex <- expandWithinSample(obj, su, target = "Age")
  sw <- getStageWeights(obj, su, sc)
  rDE <- raiseToDE(ex, sw, obj)
  # raiseToDE() leaves the totals alone: rows per DE stratum, DE ratio only
  expect_true("DEstratumName" %in% names(rDE))
  expect_null(attr(rDE, "totalsRatios"))
  expect_true(all(rDE$rApplied == attr(rDE, "deRatios")$rDE))
  expect_true(any(grepl("applied by raiseToTotals",
                        getRatioLog(rDE)$message)))
  rT <- raiseToTotals(rDE, ex, sw, obj)
  expect_false("DEstratumName" %in% names(rT))
  expect_false(is.null(attr(rT, "totalsRatios")))
  sr <- attr(rT, "sampleRatios")
  expect_equal(names(sr), c("SAid", "rDE", "rTotals"))
  # the log holds both steps
  expect_true(all(c("raiseToDE", "raiseToTotals") %in%
                    sub(" .*", "", getRatioLog(rT)$step)))
  res <- estT(obj, sc)
  expect_equal(summariseRatioEstimates(rT), res$estimates)
  expect_equal(summariseRatioDomains(rT), res$domains)
  expect_equal(attr(rT, "totalsRatios"), res$ratios$totals)
})

test_that("raiseToTotals() checks its input", {
  sim <- simT(lowHierarchy = "C")
  obj <- sim$obj
  scNo <- raisingScheme(DE = deRaising("ratio"))
  su <- getSampleUnits(obj, species = 127143)
  ex <- expandWithinSample(obj, su, target = "Age")
  sw <- getStageWeights(obj, su, scNo)
  rDE <- raiseToDE(ex, sw, obj)
  expect_error(raiseToTotals(rDE, ex, sw, obj), "has no totals")
  sc <- raisingScheme(DE = deRaising("ratio"),
                      totals = totalsOf(clTotals(obj)))
  expect_error(raiseToTotals(data.table::data.table(a = 1), ex, sw, obj,
                             scheme = sc), "output of raiseToDE")
  # other samples than the ones raiseToDE() saw
  ex2 <- data.table::copy(ex)
  attr(rDE, "sampleRatios") <- attr(rDE, "sampleRatios")[-1]
  expect_error(raiseToTotals(rDE, ex2, sw, obj, scheme = sc),
               "does not match these samples")
})

}) ## end capture.output
