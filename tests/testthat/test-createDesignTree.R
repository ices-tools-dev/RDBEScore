capture.output({  ## suppresses printing of console output when running test()

  # The SDAResources example objects have SAsamp == "N" on every SA row
  # although all values are present; treat them as sampled
  markSampled <- function(x) {
    x$SA <- data.table::copy(x$SA)
    x$SA[, SAsamp := "Y"]
    x
  }

  # two weeks in H8ExampleEE1 have 2 VS rows but VSnumSamp = 1
  h8Tree <- function(x = H8ExampleEE1, ...) {
    suppressWarnings(createDesignTree(x, ..., strictSampleSize = FALSE))
  }

  test_that("createDesignTree builds the H8 tree down to BV", {
    tree <- h8Tree(lowestTable = "BV", bvAssess = "Age")
    expect_equal(unique(tree$table),
                 c("DE", "SD", "TE", "VS", "LE", "SS", "SA", "BV"))
    expect_equal(tree[, unique(depth), by = table]$V1, 1:8)
    # one BV unit per aged fish
    expect_equal(nrow(tree[table == "BV"]),
                 sum(H8ExampleEE1$BV$BVtypeAssess == "Age"))
    expect_true(all(tree[table == "BV", parent] %in% tree[table == "SA", node]))
    expect_equal(tree[table == "SA", unique(w)], tree[table == "SA", N / n])
  })

  test_that("createDesignTree inserts cluster stages", {
    x <- markSampled(Pckg_SDAResources_gpa_H1)
    tree <- createDesignTree(x, "SA")
    clusters <- tree[table == "VS_cluster"]
    expect_equal(nrow(clusters), 5)
    expect_equal(unique(clusters$method), "SRSWOR")
    expect_equal(unique(clusters$w), 100 / 5)
    expect_true(all(tree[table == "VS", parent] %in% clusters$node))
    expect_equal(unique(tree[table == "VS", method]), "CENSUS")
  })

  test_that("createDesignTree links sub-sampled SA rows to their parent SA", {
    x <- H8ExampleEE1
    x$SA <- data.table::copy(x$SA)
    sub <- data.table::copy(x$SA[SAid == 1])
    sub[, `:=`(SAid = 999, SAseqNum = 99, SAparSequNum = 1)]
    x$SA <- rbind(x$SA, sub)
    data.table::setkey(x$SA, SAid)
    tree <- h8Tree(x, lowestTable = "SA")
    expect_equal(tree[node == "SA:999", parent], "SA:1")
    expect_equal(tree[node == "SA:1", parent], "SS:1")
  })

  test_that("createDesignTree stops on non-response unless told otherwise", {
    x <- H8ExampleEE1
    x$SA <- data.table::copy(x$SA)
    x$SA[SAid == 2, SAsamp := "N"]
    x$BV <- x$BV[SAid != 2]
    expect_error(h8Tree(x, lowestTable = "SA"), "not sampled")
    tree <- h8Tree(x, lowestTable = "SA", nonResponse = "respondents")
    expect_false("SA:2" %in% tree$node)
  })

  test_that("createDesignTree stops on selection methods without estimator", {
    x <- H8ExampleEE1
    x$LE <- data.table::copy(x$LE)
    x$LE[, LEselectMeth := "NPQSRSWOR"]
    expect_error(h8Tree(x, lowestTable = "SA"), "NPQSRSWOR")
    tree <- h8Tree(x, lowestTable = "SA", methodMap = c(NPQSRSWOR = "SRSWOR"))
    expect_equal(unique(tree[table == "LE", method]), "SRSWOR")
    expect_equal(unique(tree[table == "LE", methodOriginal]), "NPQSRSWOR")
  })

  test_that("createDesignTree checks the sample sizes", {
    expect_error(createDesignTree(H8ExampleEE1, "SA"), "differs from numSamp")
    expect_warning(createDesignTree(H8ExampleEE1, "SA",
                                    strictSampleSize = FALSE),
                   "differs from numSamp")
  })

  test_that("createDesignTree stops when SRS design variables are missing", {
    # the number of clutches in the population is not known
    expect_error(createDesignTree(Pckg_SDAResources_coots_multistage_H1, "SA",
                                  strictSampleSize = FALSE),
                 "without numTotal")
  })

  test_that("createDesignTree checks the ratio auxiliary values", {
    expect_error(createDesignTree(markSampled(Pckg_SDAResources_agsrs_H1), "SA",
                                  ratio = c(VS = "aux")),
                 "missing auxiliary values")
    expect_error(createDesignTree(H8ExampleEE1, "SA", ratio = c(VS = "weight")),
                 "only defined for SA")
  })

  test_that("createDesignTree applies varianceAsWR only to the given tables", {
    tree <- h8Tree(lowestTable = "SA", varianceAsWR = "TE")
    expect_equal(unique(tree[table == "TE", fpc]), 1)
    expect_equal(unique(tree[table == "TE", cw]), 0)
    expect_true(all(tree[table == "LE", fpc] < 1))
    expect_error(h8Tree(lowestTable = "SA", varianceAsWR = "FO"),
                 "not in the tree")
  })

  test_that("createDesignTree needs an RDBESDataObject", {
    expect_error(createDesignTree(list(DE = H8ExampleEE1$DE)),
                 "class RDBESDataObject")
  })

}) ## end capture.output
