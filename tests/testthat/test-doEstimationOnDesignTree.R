capture.output({  ## suppresses printing of console output when running test()

  # Text book examples converted to RDBESDataObjects (see ?Pckg_SDAResources_agsrs_H1
  # etc.). Expected values are those of Lohr, Sampling: Design and Analysis,
  # and of the survey package documentation; all of them are reproduced with
  # survey::svytotal / svymean / svyratio on the original data.

  # The SDAResources example objects have SAsamp == "N" on every SA row
  # although all values are present; treat them as sampled
  markSampled <- function(x) {
    x$SA <- data.table::copy(x$SA)
    x$SA[, SAsamp := "Y"]
    x
  }
  rootOf <- function(res, variable = "SAsampWtMes") {
    res$nodes[node == "ROOT" & var == variable]
  }
  estimateSA <- function(x, field = "SAsampWtMes", na = "stop", ...) {
    tree <- createDesignTree(x, "SA", ...)
    doEstimationOnDesignTree(tree, getLeafValuesSA(tree, x, field, na = na))
  }
  # mean per element = total of y / estimated number of elements
  meanPerElement <- function(x, field = "SAsampWtMes", ...) {
    tree <- createDesignTree(x, "SA", ...)
    leaves <- getLeafValuesSA(tree, x, field)
    count <- data.table::data.table(node = tree[table == "SA", node],
                                    var = "elements", t = 1, v = 0)
    res <- doEstimationOnDesignTree(tree, rbind(leaves, count))
    doRatioEstimationToTotal(res, field, "elements", X = 1)
  }

  test_that("simple random sampling: Lohr agsrs, total acres92", {
    r <- rootOf(estimateSA(markSampled(Pckg_SDAResources_agsrs_H1)))
    expect_equal(round(r$t), 916927110)
    expect_equal(round(r$se), 58169381)
  })

  test_that("stratified random sampling: Lohr agstrat, total acres92", {
    res <- estimateSA(markSampled(Pckg_SDAResources_agstrat_H1))
    r <- rootOf(res)
    expect_equal(round(r$t), 909736035)
    expect_equal(round(r$se), 50417248)
    st <- res$strata[table == "VS"]
    data.table::setkey(st, stratum)
    expect_equal(round(st[c("NC", "NE", "S", "W"), t]),
                 c(316731380, 21478558, 292037391, 279488706))
    expect_equal(round(st[c("NC", "NE", "S", "W"), se]),
                 c(16977399, 3992889, 26154840, 39416342))
  })

  test_that("ratio estimator at a sampling stage: Lohr agsrs, acres92 / acres87", {
    skip_if_not_installed("SDAResources")
    agsrs <- SDAResources::agsrs
    x <- markSampled(Pckg_SDAResources_agsrs_H1)
    x$VS <- data.table::copy(x$VS)
    # VSunitName ends with the row number of the county in agsrs
    row <- as.integer(sub(".* ", "", x$VS$VSunitName))
    x$VS[, `:=`(VSauxVarValue = agsrs$acres87[row],
                VSauxVarTot = 964470625, VSauxVarName = "acres87")]
    r <- rootOf(estimateSA(x, ratio = c(VS = "aux")))
    expect_equal(round(r$t), 951513191)
    expect_equal(round(r$se), 5546162)
  })

  test_that("one-stage cluster sampling: Lohr gpa", {
    # gpa is stored * 100 in the RDBESDataObject
    x <- markSampled(Pckg_SDAResources_gpa_H1)
    r <- rootOf(estimateSA(x))
    expect_equal(r$t / 100, 1130.4)
    expect_equal(round(r$se / 100, 3), 65.466)
    m <- meanPerElement(x)
    expect_equal(m$est / 100, 2.826)
    expect_equal(round(m$se / 100, 5), 0.16366)
  })

  test_that("one-stage cluster sampling: Lohr algebra", {
    x <- markSampled(Pckg_SDAResources_algebra_H1)
    r <- rootOf(estimateSA(x))
    expect_equal(r$t, 291533)
    expect_equal(round(r$se, 2), 19892.74)
    m <- meanPerElement(x)
    expect_equal(round(m$est, 5), 62.56856)
    expect_equal(round(m$se, 5), 1.49158)
  })

  test_that("two-stage cluster sampling: Lohr schools, mathlevel", {
    x <- markSampled(Pckg_SDAResources_schools_H1)
    # the object has VSnumTotalClusters = 100, the population has 75 schools
    x$VS <- data.table::copy(x$VS)
    x$VS[, VSnumTotalClusters := 75L]
    r <- rootOf(estimateSA(x))
    expect_equal(round(r$t), 22242)
    expect_equal(round(r$se, 1), 2017.5)
    m <- meanPerElement(x)
    expect_equal(round(m$est, 4), 1.2877)
    expect_equal(round(m$se, 4), 0.0516)
  })

  test_that("one-stage cluster sampling: survey apiclus1, enroll", {
    # the object uses the design weights 757/15, not the calibrated pw
    r <- rootOf(estimateSA(Pckg_survey_apiclus1_H1))
    expect_equal(round(r$t), 5076846)
    expect_equal(round(r$se), 1389984)
  })

  test_that("two-stage sampling: survey apiclus2, enroll", {
    # svytotal(~enroll, dclus2, na.rm = TRUE): the schools with NA enroll
    # (SAsamp == "N") stay in the design with y = 0
    for (x in list(Pckg_survey_apiclus2_H1, Pckg_survey_apiclus2_v2_H1)) {
      x$SA <- data.table::copy(x$SA)
      x$SA[SAsamp == "N", SAsamp := "Y"]
      # FO is a dummy census table with FOnumSamp = 10 but one row per FT
      r <- rootOf(suppressWarnings(estimateSA(x, na = "zero",
                                              strictSampleSize = FALSE)))
      expect_equal(round(r$t), 2639273)
      expect_equal(round(r$se), 799638)
    }
  })

  test_that("stratified random sampling: survey apistrat, enroll", {
    x <- markSampled(Pckg_survey_apistrat_H1)
    # the object swaps the population sizes of strata M (1018) and H (755)
    x$VS <- data.table::copy(x$VS)
    x$VS[VSstratumName == "M", VSnumTotal := 1018L]
    x$VS[VSstratumName == "H", VSnumTotal := 755L]
    r <- rootOf(estimateSA(x))
    expect_equal(round(r$t), 3687178)
    expect_equal(round(r$se), 114642)
  })

  test_that("a single stage gives the same results as estimMC", {
    for (m in c("SRSWOR", "SRSWR", "UPSWR")) {
      x <- markSampled(Pckg_SDAResources_agsrs_H1)
      x$VS <- data.table::copy(x$VS)
      x$VS[, VSselectMeth := m]
      y <- as.numeric(x$SA$SAsampWtMes)
      if (m == "UPSWR") {
        # any positive selection probabilities will do; SA and VS are 1:1
        x$VS[, VSselProb := (y + 1) / sum(y + 1) * 300 / 3078]
      }
      res <- estimateSA(x)
      tree <- res$tree
      yVS <- res$nodes[var == "SAsampWtMes" & table == "VS"]
      yVS <- yVS$t[match(paste0("VS:", x$VS$VSid), yVS$node)]
      yVS[is.na(yVS)] <- 0
      mc <- estimMC(yVS, as.numeric(x$VS$VSnumSamp),
                    as.numeric(x$VS$VSnumTotal), m,
                    selProb = x$VS$VSselProb)
      expect_equal(rootOf(res)$t, mc$est.total)
      expect_equal(rootOf(res)$v, mc$var.total)
    }
  })

  test_that("a census at every stage has zero variance", {
    x <- markSampled(Pckg_SDAResources_agsrs_H1)
    x$VS <- data.table::copy(x$VS)
    x$VS[, `:=`(VSselectMeth = "CENSUS", VSnumTotal = 300L)]
    r <- rootOf(estimateSA(x))
    expect_equal(r$t, sum(as.numeric(x$SA$SAsampWtMes)))
    expect_equal(r$v, 0)
  })

  test_that("one sampled unit per stratum gives NA unless varianceAsWR", {
    # H8: weeks (TE) are SRSWOR and several weeks have one vessel (VS)
    x <- H8ExampleEE1
    res <- suppressWarnings(estimateSA(x, strictSampleSize = FALSE))
    expect_true(is.na(rootOf(res)$v))
    res <- suppressWarnings(estimateSA(x, strictSampleSize = FALSE,
                                       varianceAsWR = "TE"))
    expect_true(is.finite(rootOf(res)$v))
  })

  test_that("whether a variance is estimable depends on the design only", {
    # one box (SA) per landing: no between box variance, also for the ages
    # absent from a box (zero values are not stored)
    x <- H8ExampleEE1
    tree <- suppressWarnings(createDesignTree(x, "BV", bvAssess = "Age",
                                              strictSampleSize = FALSE))
    res <- doEstimationOnDesignTree(tree, getLeafValuesBV(tree, x, "Age"))
    sa <- res$strata[table == "SA"]
    expect_true(any(sa$t == 0))
    expect_true(all(is.na(sa$vb)))
    expect_true(all(is.na(res$nodes[node == "ROOT", v])))
  })

  test_that("the SA weight ratio gives the numbers at age of doEstimationRatio", {
    x <- H8ExampleEE1
    tree <- suppressWarnings(createDesignTree(x, "BV", bvAssess = "Age",
                                              ratio = c(SA = "weight"),
                                              strictSampleSize = FALSE))
    res <- doEstimationOnDesignTree(tree, getLeafValuesBV(tree, x, "Age"))
    ours <- res$strata[table == "SA" & t > 0]
    # SS and SA are 1:1 in H8ExampleEE1
    ours[, SAid := as.numeric(sub("^SS:", "", parent))]
    ours[, Age := sub("^N\\|Age=", "", var)]
    ref <- suppressWarnings(doEstimationRatio(x, "AgeComp", "Weight"))
    cmp <- merge(ours[, .(SAid, Age, t)],
                 ref[, .(SAid, Age = as.character(Age), NumbersAtAge)],
                 by = c("SAid", "Age"), all = TRUE)
    expect_equal(nrow(cmp), nrow(ref))
    expect_equal(cmp$t, cmp$NumbersAtAge)
  })

  test_that("lower hierarchy A: FM and BV under FM match Horvitz-Thompson sums", {
    x <- filterRDBESDataObject(H1Example, "DEsampScheme", "National Routine",
                               killOrphans = TRUE)
    # independent calculation: product of numTotal / numSamp from SA up to VS
    # (all stages are SRS in these data)
    wOf <- function(d, tb) {
      as.numeric(d[[paste0(tb, "numTotal")]]) / as.numeric(d[[paste0(tb, "numSamp")]])
    }
    sa <- x$SA[, .(SAid, SSid, wSA = wOf(x$SA, "SA"))]
    ss <- x$SS[, .(SSid, FOid, wSS = wOf(x$SS, "SS"))]
    fo <- x$FO[, .(FOid, FTid, wFO = wOf(x$FO, "FO"))]
    ft <- x$FT[, .(FTid, VSid, wFT = wOf(x$FT, "FT"))]
    vs <- x$VS[, .(VSid, wVS = wOf(x$VS, "VS"))]
    p <- vs[ft[fo[ss[sa, on = "SSid"], on = "FOid"], on = "FTid"], on = "VSid"]
    p[, wPath := wSA * wSS * wFO * wFT * wVS]
    wFM <- p$wPath[match(x$FM$SAid, p$SAid)]

    treeFM <- createDesignTree(x, "FM")
    resFM <- doEstimationOnDesignTree(treeFM, getLeafValuesFM(treeFM, x))
    expect_equal(resFM$nodes[node == "ROOT", sum(t)],
                 sum(as.numeric(x$FM$FMnumAtUnit) * wFM))

    # within each FM class the aged fish are raised to BVnumTotal
    treeBV <- createDesignTree(x, "BV", bvAssess = "Age")
    resBV <- doEstimationOnDesignTree(treeBV, getLeafValuesBV(treeBV, x, "Age"))
    bvFM <- unique(x$BV[x$BV$BVtypeAssess == "Age", .(FMid, BVnumTotal)])
    expect_equal(resBV$nodes[node == "ROOT", sum(t)],
                 sum(as.numeric(bvFM$BVnumTotal) *
                       wFM[match(bvFM$FMid, x$FM$FMid)]))
  })

  test_that("doEstimationOnDesignTree stops on missing values and dead branches", {
    x <- H8ExampleEE1
    tree <- suppressWarnings(createDesignTree(x, "SA", strictSampleSize = FALSE))
    leaves <- getLeafValuesSA(tree, x, "SAsampWtMes")
    leaves[1, t := NA]
    expect_error(doEstimationOnDesignTree(tree, leaves), "NA leaf values")

    # an SA without aged fish: its numbers at age are missing, not zero
    x$BV <- x$BV[SAid != 1]
    tree <- suppressWarnings(createDesignTree(x, "BV", bvAssess = "Age",
                                              strictSampleSize = FALSE))
    expect_error(doEstimationOnDesignTree(tree, getLeafValuesBV(tree, x, "Age")),
                 "no child units down to table BV")
  })

}) ## end capture.output
