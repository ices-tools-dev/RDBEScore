capture.output({  ## suppresses printing of console output when running test()

# Every column of every element of a doRatioEstimation() result must be
# explained in ratioOutputGuide(), so a new column without a description
# fails here.

undocumented <- function(res) {
  g <- ratioOutputGuide()
  strata <- res$settings$raising$totals$strata
  tables <- list(estimates = res$estimates, domains = res$domains,
                 "ratios$DE" = res$ratios$DE,
                 "ratios$totals" = res$ratios$totals,
                 samples = res$samples, fishBySample = res$fishBySample,
                 byStratum = res$byStratum, log = res$log)
  for (st in names(res$stageUnits)) tables[[paste0("stageUnits$", st)]] <-
    res$stageUnits[[st]]
  out <- character(0)
  for (el in names(tables)) {
    if (is.null(tables[[el]])) next
    guideEl <- sub("^stageUnits\\$[A-Z]{2}$", "stageUnits", el)
    gEl <- g[element == guideEl]
    pats <- vapply(gEl$column, guideColumnPattern, "")
    for (cl in names(tables[[el]])) {
      if (el == "ratios$totals" && cl %in% strata) next
      if (!any(vapply(pats, grepl, logical(1), x = cl))) {
        out <- c(out, paste0(el, ": ", cl))
      }
    }
  }
  out
}

test_that("every output column is in ratioOutputGuide() (H1, with totals)", {
  obj <- H1StageRaisingExample
  cl <- obj$CL[, .(kg = sum(CLoffWeight)),
               by = .(quarter = CLquar, area = CLarea, metier6 = CLmetier6)]
  sc <- raisingScheme(
    FO = stageRaising("count"), DE = deRaising("ratio"),
    totals = totalsRaising(externalAux(cl, by = c("quarter", "area",
                                                  "metier6"), col = "kg")))
  for (target in c("Age", "Length")) {
    res <- doRatioEstimation(obj, species = 127143, raising = sc,
                             target = target, checkCodes = FALSE)
    expect_equal(undocumented(res), character(0), label = target)
  }
  res0 <- doRatioEstimation(obj, species = 127143,
                            raising = raisingScheme(FO = stageRaising("count"),
                                                    DE = deRaising("ratio")),
                            checkCodes = FALSE)
  expect_equal(undocumented(res0), character(0))
})

test_that("every output column is in ratioOutputGuide() (H5 with FT)", {
  sim <- suppressWarnings(simulateH5RatioData(useFT = TRUE, haulsPerTrip = 3))
  res <- doRatioEstimation(sim$obj, species = 127143,
                           raising = raisingScheme(OS = stageRaising("count"),
                                                   LE = stageRaising("count"),
                                                   FT = stageRaising("count"),
                                                   DE = deRaising("ratio"),
                                                   hierarchy = 5),
                           checkCodes = FALSE)
  expect_equal(undocumented(res), character(0))
})

test_that("ratioOutputGuide() filters by element and checks names", {
  g <- ratioOutputGuide("domains")
  expect_equal(unique(g$element), "domains")
  expect_true(all(c("element", "column", "rowUnit", "unit", "meaning") %in%
                    names(g)))
  expect_error(ratioOutputGuide("raised"), "Unknown element")
})

}) ## end capture.output
