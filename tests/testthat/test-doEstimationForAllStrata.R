capture.output({  ## suppresses printing of console output when running test()

  generateTestData <- function() {
    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")

    # Filter our data for WGRDBES-EST TEST 1, 1965, H1
    myValues <- c(1965, 1, "National Routine", "DE_stratum1_H1", 1019159)
    myFields <- c("DEyear", "DEhierarchy", "DEsampScheme", "DEstratumName",
                  "SAspeCode")
    myH1RawObject <- filterRDBESDataObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues)
    myH1RawObject <- findAndKillOrphans(myH1RawObject)

    # SRSWOR on each level, with the inclusion probabilities
    for (tb in c("VS", "FT", "FO", "SS", "SA")) {
      myH1RawObject[[tb]][[paste0(tb, "selectMeth")]] <- "SRSWOR"
      myH1RawObject[[tb]][[paste0(tb, "incProb")]] <-
        myH1RawObject[[tb]][[paste0(tb, "numSamp")]] /
        myH1RawObject[[tb]][[paste0(tb, "numTotal")]]
    }

    # random sample measurements (the test data don't include these)
    set.seed(1234)
    myH1RawObject[["SA"]]$SAsampWtLive <-
      round(runif(n = nrow(myH1RawObject[["SA"]]), min = 1, max = 100))
    myH1RawObject[["SA"]]$SAsampWtMes <-
      round(runif(n = nrow(myH1RawObject[["SA"]]), min = 1, max = 100))
    myH1RawObject
  }

  # Horvitz-Thompson point estimate: y / product of the inclusion probabilities
  htTotal <- function(x, targetValue) {
    p <- x$SA[, .(SSid, y = get(targetValue), pSA = SAincProb)]
    p <- x$SS[, .(SSid, FOid, pSS = SSincProb)][p, on = "SSid"]
    p <- x$FO[, .(FOid, FTid, pFO = FOincProb)][p, on = "FOid"]
    p <- x$FT[, .(FTid, VSid, pFT = FTincProb)][p, on = "FTid"]
    p <- x$VS[, .(VSid, pVS = VSincProb)][p, on = "VSid"]
    p[, sum(y / (pSA * pSS * pFO * pFT * pVS))]
  }

  test_that("doEstimationForAllStrata gives the Horvitz-Thompson total for H1", {
    x <- generateTestData()
    # only 16 of the 243 hauls have an SA row for the species; the others
    # are taken as zero catch
    expect_error(doEstimationForAllStrata(x, "SAsampWtLive"),
                 "no child units down to table SA")
    for (targetValue in c("SAsampWtLive", "SAsampWtMes")) {
      res <- doEstimationForAllStrata(x, targetValue, zeroIfNoChildren = "SS")
      expect_equal(res[res$recType == "DE", "est.total"],
                   htTotal(x, targetValue))
    }
  })

  test_that("doEstimationForAllStrata gives Lohr's stratum estimates for agstrat", {
    x <- Pckg_SDAResources_agstrat_H1
    # SAsamp is "N" on every row of the example object although all values
    # are present
    x$SA <- data.table::copy(x$SA)
    x$SA[, SAsamp := "Y"]
    res <- doEstimationForAllStrata(x, "SAsampWtMes")
    vs <- res[res$recType == "VS", ]
    vs <- vs[match(c("NC", "NE", "S", "W"), vs$stratumName), ]
    expect_equal(round(vs$est.total), c(316731380, 21478558, 292037391,
                                        279488706))
    expect_equal(round(vs$se.total), c(16977399, 3992889, 26154840, 39416342))
    expect_equal(round(res[res$recType == "DE", "est.total"]), 909736035)
  })

  test_that("doEstimationForAllStrata gives the H8ExampleEE1 totals per month", {
    x <- H8ExampleEE1
    x$SA <- data.table::copy(x$SA)
    # grammes to kg
    x$SA[, SAsampWtMes := SAsampWtMes / 1000]
    # two weeks have 2 VS rows but VSnumSamp = 1
    res <- suppressWarnings(doEstimationForAllStrata(x, "SAsampWtMes",
                                                     strictSampleSize = FALSE))
    te <- res[res$recType == "TE", ]
    te <- te[match(c("January", "February", "March"), te$stratumName), ]
    expect_equal(round(te$est.total), c(118992, 209271, 32996))
    # weeks with a single vessel: the within week variance is not estimable
    expect_true(all(is.na(te$se.total)))

    # treating the weeks as sampled with replacement drops the fpc
    # (2 of 4 weeks) of the between week variance
    res <- suppressWarnings(doEstimationForAllStrata(x, "SAsampWtMes",
                                                     strictSampleSize = FALSE,
                                                     varianceAsWR = "TE"))
    te <- res[res$recType == "TE", ]
    te <- te[match(c("January", "February", "March"), te$stratumName), ]
    expect_equal(te$var.within, c(0, 0, 0))
    expect_equal(te$se.total * sqrt(1 - 2 / 4), c(55853, 97071, 7213),
                 tolerance = 1e-4)
  })

  test_that("doEstimationForAllStrata checks its input", {
    expect_error(doEstimationForAllStrata(as.data.frame(H8ExampleEE1$SA),
                                          "SAsampWtMes"),
                 "no longer used for estimation")
    expect_error(doEstimationForAllStrata(H8ExampleEE1, "BVvalueMeas"),
                 "must be SA fields")
  })

}) ## end capture.output
