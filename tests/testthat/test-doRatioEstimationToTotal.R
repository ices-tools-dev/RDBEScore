capture.output({  ## suppresses printing of console output when running test()

  markSampled <- function(x) {
    x$SA <- data.table::copy(x$SA)
    x$SA[, SAsamp := "Y"]
    x
  }

  test_that("ratio to a known total: Lohr agsrs, acres92 / acres87", {
    skip_if_not_installed("SDAResources")
    agsrs <- SDAResources::agsrs
    x <- markSampled(Pckg_SDAResources_agsrs_H1)
    tree <- createDesignTree(x, "SA")
    saNodes <- tree[table == "SA", node]
    # VSunitName ends with the row number of the county in agsrs
    row <- as.integer(sub(".* ", "", getAncestorValue(tree, saNodes, x,
                                                      "VSunitName")))
    leaves <- rbind(getLeafValuesSA(tree, x, "SAsampWtMes"),
                    data.table::data.table(node = saNodes, var = "acres87",
                                           t = agsrs$acres87[row], v = 0))
    res <- doEstimationOnDesignTree(tree, leaves[t != 0])
    r <- doRatioEstimationToTotal(res, "SAsampWtMes", "acres87", X = 964470625)
    expect_equal(round(r$est), 951513191)
    expect_equal(round(r$se), 5546162)
    expect_equal(round(r$R, 4), 0.9866)
  })

  test_that("ratio to CL landings in H8 reproduces the landings and is finite", {
    x <- H8ExampleEE1
    tree <- suppressWarnings(createDesignTree(x, "BV", bvAssess = "Age",
                                              ratio = c(SA = "weight"),
                                              strictSampleSize = FALSE,
                                              varianceAsWR = "TE"))
    leaves <- rbind(getLeafValuesBV(tree, x, "Age"),
                    getLeafValuesSA(tree, x, "SAsampWtMes"))
    res <- doEstimationOnDesignTree(tree, leaves)
    q1 <- c("January", "February", "March")
    quarter <- function(nodes) {
      ifelse(getAncestorValue(tree, nodes, x, "TEstratumName") %in% q1,
             "Q1", "Q4")
    }
    cl <- x$CL[CLmetier6 == "OTM_SPF_16-31_0_0" & CLquar %in% c(1, 4),
               .(X = sum(CLoffWeight) * 1000), by = CLquar]
    X <- c(Q1 = cl[CLquar == 1, X], Q4 = cl[CLquar == 4, X])

    # the auxiliary variable itself is raised exactly to the known totals
    xx <- doRatioEstimationToTotal(res, "SAsampWtMes", "SAsampWtMes", X,
                                   domainOf = quarter)
    expect_equal(xx$est, unname(X[xx$domain]))
    expect_true(all(xx$se < 1e-6 * xx$est))

    ages <- grep("^N\\|Age=", unique(res$nodes$var), value = TRUE)
    r <- doRatioEstimationToTotal(res, ages, "SAsampWtMes", X, domainOf = quarter)
    expect_equal(nrow(r), 2 * length(ages))
    expect_true(all(is.finite(r$se)))
    # numbers at age scale with the ratio of known to estimated landings
    expect_equal(r$est, r$t_y * r$X / r$t_x)
  })

  test_that("doRatioEstimationToTotal checks its input", {
    x <- H8ExampleEE1
    tree <- suppressWarnings(createDesignTree(x, "SA", strictSampleSize = FALSE))
    res <- doEstimationOnDesignTree(tree, getLeafValuesSA(tree, x, "SAsampWtMes"))
    expect_error(doRatioEstimationToTotal(res, "N|Age=3", "SAsampWtMes", 1),
                 "not in the estimates")
    expect_error(doRatioEstimationToTotal(res, "SAsampWtMes", "SAsampWtMes",
                                          c(Q1 = 1), domainOf = function(n) {
                                            rep("Q2", length(n))
                                          }),
                 "No known total")
  })

}) ## end capture.output
