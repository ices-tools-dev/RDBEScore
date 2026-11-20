# AI-assisted: Yes
# Review: rix133
# Review comments on pull request #293: the deprecated pooled H8 estimation
# (addCLtoLowerCS() + doBVestimCANUM()) is the reference for
# doRatioEstimation() with the equivalent scheme, and the ICES code lists are
# packaged so that estimation does not need vocab.ices.dk.

library(data.table)

h8Strata <- function(quarter) {
  months <- if (quarter == 1) month.name[1:3] else month.name[10:12]
  list(cs = list(LEarea = "27.3.d.28.1", LEmetier6 = "OTM_SPF_16-31_0_0",
                 TEstratumName = months, SAspeCodeFAO = "SPR"),
       cl = list(CLarea = "27.3.d.28.1", CLquar = quarter,
                 CLmetier6 = "OTM_SPF_16-31_0_0", CLspecFAO = "SPR"))
}

# The former vignette example: pooled fish raised to the CL landings
h8Legacy <- function(quarter, classUnits, classBreaks) {
  st <- h8Strata(quarter)
  biol <- suppressWarnings(addCLtoLowerCS(H8ExampleEE1, st$cs, st$cl,
                                          combineStrata = TRUE,
                                          lowerHierarchy = "C",
                                          CLfields = "CLoffWeight"))
  old <- withoutDeprecation(doBVestimCANUM(biol, "sumCLoffWeight",
                                           classUnits = classUnits,
                                           classBreaks = classBreaks))
  old[, .(quarter = quarter,
          class = as.numeric(sub("[-+].*$", "", as.character(Group))),
          N.old = totNum, W.old = WeightgMean, L.old = LengthmmMean)]
}

# The same estimator with doRatioEstimation(): no raising within the sample
# or between stages, and the measured fish weight raised to the CL landings
h8New <- function(target, ...) {
  clTot <- H8ExampleEE1$CL[CLspecFAO == "SPR",
                           .(landedKg = sum(as.numeric(CLoffWeight))),
                           by = .(quarter = CLquar, area = CLarea,
                                  metier6 = CLmetier6)]
  scheme <- raisingScheme(
    SA = sampleRaising("none"),
    totals = totalsRaising(externalAux(clTot,
                                       by = c("quarter", "area", "metier6"),
                                       col = "landedKg"),
                           value = "fishWeight"),
    hierarchy = 8)
  res <- doRatioEstimation(H8ExampleEE1, species = 126425, raising = scheme,
                           target = target,
                           domainVars = c("quarter", "area", "metier6"), ...)
  res$estimates[area == "27.3.d.28.1" & metier6 == "OTM_SPF_16-31_0_0"]
}

test_that("the deprecated pooled estimation functions are still exported", {
  expect_true("addCLtoLowerCS" %in% getNamespaceExports("RDBEScore"))
  expect_true("doBVestimCANUM" %in% getNamespaceExports("RDBEScore"))
})

test_that("doRatioEstimation reproduces the former H8 numbers at age", {
  # classBreaks = 0:8 keeps age 0, as doRatioEstimation() does
  legacy <- rbind(h8Legacy(1, "Ageyear", 0:8), h8Legacy(4, "Ageyear", 0:8))
  new <- h8New("Age", plusGroup = 8)[quarter %in% c(1, 4)]
  cmp <- merge(legacy, new[, .(quarter, class = age, N, meanWeight_g,
                               meanLength_mm)],
               by = c("quarter", "class"), all = TRUE)
  expect_false(anyNA(cmp[, .(N.old, N)]))
  expect_equal(cmp$N, cmp$N.old, tolerance = 1e-8)
  expect_equal(cmp$meanWeight_g, cmp$W.old, tolerance = 1e-8)
  expect_equal(cmp$meanLength_mm, cmp$L.old, tolerance = 1e-8)
})

test_that("doRatioEstimation reproduces the former H8 numbers at length", {
  # The legacy classes 70-80, ..., 120-130 and 130+ equal 10 mm classes
  # only when no fish is below 70 mm or 140 mm and above
  st <- h8Strata(1)
  biol <- suppressWarnings(addCLtoLowerCS(H8ExampleEE1, st$cs, st$cl))
  q1 <- as.numeric(biol[BVvalUnitScale == "Lengthmm", BVvalueMeas])
  expect_true(length(q1) > 0 && min(q1) >= 70 && max(q1) < 140)

  legacy <- h8Legacy(1, "Lengthmm", seq(70, 130, 10))
  new <- h8New("Length", classWidth = 10)[quarter == 1]
  cmp <- merge(legacy, new[, .(quarter, class = lengthClass_mm, N,
                               meanWeight_g, meanLength_mm)],
               by = c("quarter", "class"), all = TRUE)
  expect_false(anyNA(cmp[, .(N.old, N)]))
  expect_equal(cmp$N, cmp$N.old, tolerance = 1e-8)
  expect_equal(cmp$meanWeight_g, cmp$W.old, tolerance = 1e-8)
  expect_equal(cmp$meanLength_mm, cmp$L.old, tolerance = 1e-8)
})

test_that("the packaged ICES code lists cover every checked code type", {
  for (ct in unique(ratioFieldCodeTypes()$codeType)) {
    cl <- getRDBESCodeList(ct)
    expect_true(nrow(cl) > 0, info = ct)
    expect_equal(attr(cl, "source"), "package snapshot", info = ct)
    expect_s3_class(attr(cl, "retrieved"), "Date")
    expect_false(is.na(attr(cl, "retrieved")), info = ct)
  }
  expect_error(getRDBESCodeList("NotACodeType"), "not found")
})

test_that("the code check is on by default and never stops the estimation", {
  expect_identical(formals(doRatioEstimation)$checkCodes, TRUE)
  local_mocked_bindings(checkRDBESCodes = function(...) stop("lists broken"))
  expect_warning(est <- h8New("Age", plusGroup = 8), "lists broken")
  expect_true(nrow(est) > 0)
})
