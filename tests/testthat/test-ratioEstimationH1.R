capture.output({  ## suppresses printing of console output when running test()

domainKey <- c("year", "quarter", "area", "metier6")

compareToTruth <- function(estimates, truth, classCol = "age") {
  m <- merge(estimates[, c(domainKey, classCol, "N"), with = FALSE], truth,
             by = c(domainKey, classCol), all = TRUE,
             suffixes = c(".est", ".true"))
  m[is.na(N.est), N.est := 0]
  m[is.na(N.true), N.true := 0]
  m
}

simulate <- function(...) suppressWarnings(simulateH1RatioData(...))

# Direct raising: samples pooled, ratio to the DE total
pooled <- raisingScheme(DE = deRaising("ratio"))

# ---------------------------------------------------------------------------
# Census designs reproduce the truth exactly
# ---------------------------------------------------------------------------

test_that("a census design reproduces numbers and mean weight at age", {
  sim <- simulate(lowHierarchy = "C")
  res <- doRatioEstimation(sim$obj, species = 127143, target = "Age",
                           raising = pooled, checkCodes = FALSE)
  expect_s3_class(res, "RDBESRatioEstimate")
  m <- compareToTruth(res$estimates, sim$truthAge)
  expect_equal(m$N.est, m$N.true)
  mw <- merge(res$estimates, sim$truthAge, by = c(domainKey, "age"))
  expect_equal(mw$meanWeight_g, mw$W / mw$N.y)
  expect_equal(unique(res$domains$SOP), rep(1, 4), tolerance = 1e-5)
})


test_that("numbers at length from LH B on a census design reproduce the truth", {
  sim <- simulate(lowHierarchy = "B")
  res <- doRatioEstimation(sim$obj, species = 127143, raising = pooled,
                           target = "Length",
                           checkCodes = FALSE)
  m <- compareToTruth(res$estimates, sim$truthLength,
                      classCol = "lengthClass_mm")
  expect_equal(m$N.est, m$N.true)
})

test_that("domain totals over quarters equal the annual estimate on a census design", {
  sim <- simulate(lowHierarchy = "C")
  byQ <- doRatioEstimation(sim$obj, species = 127143, raising = pooled,
                           checkCodes = FALSE)
  byY <- doRatioEstimation(sim$obj, species = 127143, raising = pooled,
                           checkCodes = FALSE,
                           domainVars = c("year", "area", "metier6"))
  sq <- byQ$estimates[, .(N = sum(N)), by = .(year, area, metier6, age)]
  m <- merge(sq, byY$estimates[, .(year, area, metier6, age, N)],
             by = c("year", "area", "metier6", "age"))
  expect_equal(m$N.x, m$N.y)
})

# ---------------------------------------------------------------------------
# Arithmetic of the steps
# ---------------------------------------------------------------------------

test_that("sub-sample chain raising factor is the product of W/w along the chain", {
  sim <- simulate(lowHierarchy = "C", subsample = TRUE)
  su <- getSampleUnits(sim$obj, species = 127143)
  sa <- sim$obj$SA
  lowest <- su[1]
  child <- sa[SAid == lowest$SAid]
  parent <- sa[SSid == child$SSid & SAseqNum == child$SAparSequNum]
  expected <- (parent$SAtotalWtMes / parent$SAsampWtMes) *
    (child$SAtotalWtMes / child$SAsampWtMes)
  expect_equal(lowest$fSample, expected)
  expect_equal(lowest$topSAid, parent$SAid)
  expect_equal(lowest$SAlevel, 2L)
  # x is the live weight of the top-level fraction
  expect_equal(lowest$xSample, parent$SAtotalWtMes / 1000)
  # one row per lowest-level SA: SA records that are nobody's parent
  parents <- sa[sa, on = .(SSid, SAseqNum = SAparSequNum), x.SAid,
                nomatch = NULL]
  expect_equal(sort(su$SAid), sort(setdiff(sa$SAid, parents)))
})

test_that("LH A sample ALK expansion matches a hand calculation", {
  sim <- simulate(lowHierarchy = "A")
  su <- getSampleUnits(sim$obj, species = 127143)
  ex <- expandWithinSample(sim$obj, su, target = "Age")
  sid <- su$SAid[1]
  fm <- sim$obj$FM[SAid == sid]
  bv <- sim$obj$BV[FMid %in% fm$FMid & BVtypeMeas == "Age"]
  bv <- merge(bv, fm[, .(FMid, FMclassMeas, FMnumAtUnit)], by = "FMid")
  bv[, mL := .N, by = FMid]
  hand <- bv[, .(yN = sum(FMnumAtUnit / mL)),
             by = .(class = as.numeric(BVvalueMeas))]
  hand[, yN := yN * su$fSample[1]]
  got <- ex[SAid == sid, .(class, yN)]
  m <- merge(hand, got, by = "class")
  expect_equal(nrow(m), nrow(hand))
  expect_equal(m$yN.x, m$yN.y)
  # total numbers at age equal the FM total of the sample
  expect_equal(sum(got$yN), sum(fm$FMnumAtUnit) * su$fSample[1])
})

test_that("pooled ALK fill keeps all measured fish when classes lack aged fish", {
  sim <- simulate(lowHierarchy = "A", nPerLengthClass = 1, seed = 3)
  obj <- sim$obj
  # remove the aged fish of one length class in the first sample
  fm1 <- obj$FM[SAid == obj$SA$SAid[1]][order(FMclassMeas)][3]
  obj$BV <- obj$BV[FMid != fm1$FMid]
  su <- getSampleUnits(obj, species = 127143)
  own <- expandWithinSample(obj, su, target = "Age", ageRoute = "sample")
  fill <- expandWithinSample(obj, su, target = "Age",
                             ageRoute = "sampleFillPooled")
  sid <- su$SAid[1]
  fmTot <- sum(obj$FM[SAid == sid, FMnumAtUnit]) * su[SAid == sid, fSample]
  expect_lt(sum(own[SAid == sid, yN]), fmTot)
  expect_equal(sum(fill[SAid == sid, yN]), fmTot)
  expect_true(any(grepl("lost", getRatioLog(own)$message)))
})

# ---------------------------------------------------------------------------
# Exclusions, stops and the log
# ---------------------------------------------------------------------------

test_that("LH D and not-sampled SAs are excluded from numerator and auxiliary total", {
  sim <- simulate(lowHierarchy = c("C", "D"))
  obj <- sim$obj
  obj$SA[SAid == 1, SAsamp := "N"]
  su <- getSampleUnits(obj, species = 127143)
  expect_true(all(su[SAlowHierarchy == "D", include] == FALSE))
  expect_false(su[SAid == 1, include])
  res <- doRatioEstimation(obj, species = 127143, raising = pooled,
                           checkCodes = FALSE)
  used <- unique(res$fishBySample$SAid)
  expect_false(any(used %in% su[include == FALSE, SAid]))
  expect_equal(sum(res$byStratum[, .(sumX = liveWeightSampled_kg[1]),
                              by = c(domainKey, "DEstratumName")]$sumX),
               su[SAid %in% used, sum(xSample)])
  expect_true(any(res$log$level == "exclusion"))
})

test_that("lowest-level SAs without fish records are excluded and logged", {
  # LH C without BV
  sim <- simulate(lowHierarchy = "C")
  obj <- sim$obj
  obj$BV <- obj$BV[SAid != 1 | is.na(SAid)]
  su <- getSampleUnits(obj, species = 127143)
  expect_false(su[SAid == 1, include])
  expect_equal(su[SAid == 1, excludeReason],
               "lower hierarchy C but no BV records")
  expect_true(all(su[SAid != 1, include]))
  expect_true(any(grepl("no BV records", getRatioLog(su)$message)))

  # LH B without FM
  sim <- simulate(lowHierarchy = "B")
  obj <- sim$obj
  obj$FM <- obj$FM[SAid != 1]
  su <- getSampleUnits(obj, species = 127143)
  expect_equal(su[SAid == 1, excludeReason],
               "lower hierarchy A or B but no FM records")
  res <- doRatioEstimation(obj, species = 127143, raising = pooled,
                           target = "Length",
                           checkCodes = FALSE)
  expect_false(1 %in% res$fishBySample$SAid)
})


test_that("gutted BV weights need gutToLive, and then match the live truth", {
  sim <- simulate(lowHierarchy = "C", gutted = TRUE)
  expect_error(doRatioEstimation(sim$obj, species = 127143, raising = pooled,
                                 checkCodes = FALSE), "gutToLive")
  res <- doRatioEstimation(sim$obj, species = 127143, raising = pooled,
                           checkCodes = FALSE,
                           gutToLive = 1 / 0.88)
  mw <- merge(res$estimates, sim$truthAge, by = c(domainKey, "age"))
  expect_equal(mw$meanWeight_g, mw$W / mw$N.y, tolerance = 1e-3)
  expect_equal(unique(res$domains$SOP), rep(1, 4), tolerance = 1e-3)
})

test_that("non-probabilistic selection is logged as an assumption, not stopped", {
  sim <- simulate(lowHierarchy = "C",
                  selectMeth = c(VS = "NPJS", FT = "NPQSRSWOR", FO = "CENSUS"))
  res <- doRatioEstimation(sim$obj, species = 127143, raising = pooled,
                           checkCodes = FALSE)
  a <- res$log[level == "assumption", message]
  expect_true(any(grepl("VS selection is Non-probabilistic", a)))
  expect_true(any(grepl("FT selection is Quasi-probabilistic", a)))
  expect_equal(unique(res$samples$VSselectionClass), "Non-probabilistic")
})


test_that("doRatioEstimation needs a raising scheme", {
  sim <- simulate()
  expect_error(doRatioEstimation(sim$obj, species = 127143, checkCodes = FALSE),
               "raisingScheme")
})

test_that("only RDBES hierarchies and landings are accepted", {
  # an object labelled with a hierarchy that does not exist
  obj <- H8ExampleEE1
  obj$DE <- data.table::copy(obj$DE)[, DEhierarchy := 14L]
  expect_error(getSampleUnits(obj, species = 126425, strict = FALSE),
               "Hierarchy 14 is not an RDBES upper hierarchy")
  sim <- simulate()
  expect_error(getSampleUnits(sim$obj, species = 127143, catchCat = "Dis"),
               "Lan")
})

test_that("classifySelectionMethod maps the RDBES codes", {
  expect_equal(classifySelectionMethod(c("SRSWOR", "CENSUS", "NPQSYSS",
                                         "NPCS", "FIXED", "XX", NA)),
               c("Probabilistic", "Census", "Quasi-probabilistic",
                 "Non-probabilistic", "Other", "Unrecognised", NA))
})

# ---------------------------------------------------------------------------
# Sampled design: estimates close to the truth
# ---------------------------------------------------------------------------

test_that("pooled and counted hauls on a sampled design are close to the truth", {
  args <- list(boxFish = 40, haulsPerTrip = 4, sampledHaulsPerTrip = 2,
               nVessels = 8, tripsPerVessel = 4, stratifiedSA = TRUE,
               seed = 11)
  sim <- do.call(simulate, args)
  schemes <- list(pooled,
                  raisingScheme(FO = stageRaising("count"),
                                DE = deRaising("ratio")))
  for (sc in schemes) {
    res <- doRatioEstimation(sim$obj, species = 127143, raising = sc,
                             checkCodes = FALSE)
    m <- compareToTruth(res$estimates, sim$truthAge)
    expect_lt(abs(sum(m$N.est) / sum(m$N.true) - 1), 0.05)
    expect_equal(unique(res$domains$SOP), rep(1, 4), tolerance = 0.02)
  }
})

# ---------------------------------------------------------------------------
# Live ICES code lists (need vocab.ices.dk)
# ---------------------------------------------------------------------------

skipIfNoVocab <- function() {
  skip_on_cran()
  ok <- tryCatch({
    getRDBESCodeList("YesNoFields")
    TRUE
  }, error = function(e) FALSE)
  if (!ok) skip("vocab.ices.dk not reachable")
}

test_that("getRDBESCodeList returns ICES code lists", {
  skipIfNoVocab()
  sm <- getRDBESCodeList("SelectionMethod")
  expect_true(all(c("SRSWOR", "CENSUS", "NPCS") %in% sm$Key))
  expect_named(sm, c("Key", "Description", "Deprecated"))
  expect_error(getRDBESCodeList("NotACodeType"), "not found")
})

test_that("checkRDBESCodes flags values that are not in the ICES lists", {
  skipIfNoVocab()
  sim <- simulate()
  obj <- sim$obj
  expect_equal(nrow(checkRDBESCodes(obj)[valid == FALSE]), 0)
  obj$SA[SAid == 1, SAselectMeth := "RANDOMISH"]
  chk <- checkRDBESCodes(obj, tables = "SA")
  expect_equal(chk[valid == FALSE, value], "RANDOMISH")
  expect_warning(doRatioEstimation(obj, species = 127143, raising = pooled),
                 "RANDOMISH")
})

test_that("the result holds one table per row unit, without attributes", {
  sim <- simulate(haulsPerTrip = 3, sampledHaulsPerTrip = 1)
  res <- doRatioEstimation(sim$obj, species = 127143,
                           raising = raisingScheme(FO = stageRaising("count"),
                                                   DE = deRaising("ratio")),
                           checkCodes = FALSE)
  expect_equal(names(res), c("estimates", "domains", "ratios", "samples",
                             "stageUnits", "fishBySample", "byStratum",
                             "codeCheck", "log", "settings"))
  key <- c("year", "quarter", "area", "metier6")
  # estimates: one row per domain x age, no per-domain columns
  expect_equal(anyDuplicated(res$estimates[, c(key, "age"), with = FALSE]), 0L)
  expect_false(any(c("SOP", "liveWeightEst_kg", "nSamples") %in%
                     names(res$estimates)))
  # domains: one row per domain, the factor applied is estimated / sampled
  d <- res$domains
  expect_equal(nrow(d), nrow(unique(res$estimates[, ..key])))
  expect_equal(d$rApplied, d$liveWeightEst_kg / d$liveWeightSampled_kg)
  expect_equal(d$rApplied, rep(res$ratios$DE$rDE, nrow(d)))
  expect_equal(names(res$ratios$DE),
               c("DEstratumName", "rDE", "total", "xWeightedStratum"))
  expect_null(res$ratios$totals)
  # samples: key columns first, no step attributes
  expect_equal(names(res$samples)[1:3], c("SAid", "topSAid", "SAlevel"))
  expect_lt(match("wStages", names(res$samples)),
            match("VSselectMeth", names(res$samples)))
  expect_true(all(c("fStage", "wCumulative") %in% names(res$stageUnits$FO)))
  expect_true(all(c("yW_g", "yL_mm") %in% names(res$fishBySample)))
  expect_true(all(c("W_g", "L_mm", "DEstratumName", "rApplied") %in%
                    names(res$byStratum)))
  expect_null(attr(res$samples, "ratioLog"))
  expect_null(attr(res$byStratum, "deRatios"))
  expect_equal(names(res$stageUnits), c("VS", "FT", "FO"))
  expect_output(print(res), "fishBySample")
})

}) ## end capture.output
