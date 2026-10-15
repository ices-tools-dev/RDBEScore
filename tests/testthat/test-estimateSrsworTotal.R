# AI-assisted: Yes
# Review: Pending
# Notes/scope: Closed-form SRSWOR calculations, packaged agriculture examples,
# input preservation and explicit rejection of unsupported sampling designs.

makeSrsworEstFixture <- function() {
  object <- data.table::data.table(
    DEid = 1, SDid = 1, VSid = 1:4,
    su1table = "VS", su1numSamp = 4, su1numTotal = 8,
    su1stratumName = "U", su1stratification = "N", su1clustering = "N",
    su1selectMeth = "SRSWOR", su1incProb = 0.5, su1selProb = NA_real_,
    su1samp = "Y", VSresponse = c(3, 4, 4, 5)
  )
  class(object) <- c("RDBESEstObject", class(object))
  object
}

test_that("SRSWOR total, variance and intervals agree with a hand calculation", {
  object <- makeSrsworEstFixture()
  before <- data.table::copy(object)
  # mean=4, sample variance=2/3, total=8*4, V=64*.5*(2/3)/4.
  result <- estimateSrsworTotal(object, "VSresponse")
  expect_equal(result$estimates$est.total, 32)
  expect_equal(result$estimates$var.total, 16 / 3)
  expect_equal(result$estimates$se.total, sqrt(16 / 3))
  expect_equal(result$estimates$lower, 32 - stats::qnorm(0.975) * sqrt(16 / 3))
  expect_equal(result$estimates$upper, 32 + stats::qnorm(0.975) * sqrt(16 / 3))
  expect_length(result$replicates, 0)
  expect_identical(object, before)

  set.seed(278)
  boot <- estimateSrsworTotal(object, "VSresponse", nBoot = 100)
  set.seed(278)
  repeated <- estimateSrsworTotal(object, "VSresponse", nBoot = 100)
  expect_identical(boot, repeated)
  expect_equal(boot$estimates$est.total, c(32, 32))
  expect_equal(boot$estimates$var.total[2], stats::var(boot$replicates))
  expect_identical(object, before)
})

test_that("the RDBES stage mapping preserves a complete three-stage design", {
  object <- data.table::data.table(
    DEid = 1, SDid = 1,
    VSid = rep(1:2, each = 4), FTid = rep(1:4, each = 2), FOid = 1:8,
    FOresponse = c(1, 3, 4, 8, 2, 6, 10, 20)
  )
  for (stage in 1:3) {
    prefix <- paste0("su", stage)
    object[[paste0(prefix, "table")]] <- c("VS", "FT", "FO")[stage]
    object[[paste0(prefix, "stratification")]] <- "N"
    object[[paste0(prefix, "stratumName")]] <- "U"
    object[[paste0(prefix, "selectMeth")]] <- "SRSWOR"
    object[[paste0(prefix, "numSamp")]] <- 2
    object[[paste0(prefix, "numTotal")]] <- 4
  }
  class(object) <- c("RDBESEstObject", class(object))
  result <- estimateSrsworTotal(object, "FOresponse")
  # Leaf totals 8,24,16,60 expand to 64,152 and finally 432.
  # Leaf variances 8,32,32,200 propagate to 592,4336 and finally 25344.
  expect_equal(result$estimates$est.total, 432)
  expect_equal(result$estimates$var.total, 25344)
  reordered <- object[c(8, 2, 5, 1, 7, 4, 6, 3), ]
  expect_equal(estimateSrsworTotal(reordered, "FOresponse")$estimates,
               result$estimates)

  # Population sizes can vary by parent. Under FT unit 4, take N=6 rather
  # than N=4. Its expanded total is 90 and variance 600. The top two totals
  # are then 64,212, with variances 592,12216, giving T=552,V=69424.
  changed <- data.table::copy(object)
  changed$su3numTotal[changed$FTid == 4] <- 6
  result <- estimateSrsworTotal(changed, "FOresponse")
  expect_equal(result$estimates$est.total, 552)
  expect_equal(result$estimates$var.total, 69424)
})

test_that("agriculture strata agree with independent complete-data references", {
  H1DataObj <- Pckg_SDAResources_agstrat_H1
  original <- data.table::copy(H1DataObj)
  H1EstObj <- createRDBESEstObject(H1DataObj, 1, stopTable = "SA")
  estBefore <- data.table::copy(H1EstObj)
  result <- estimateSrsworTotal(H1EstObj, "SAsampWtMes")
  # Independent keyed joins SA -> SS -> FO -> FT -> VS followed by the four
  # stratum formulas N_h*mean(y_h), N_h^2*(1-n_h/N_h)*var(y_h)/n_h.
  expect_equal(result$estimates$est.total, 909736035.391957, tolerance = 1e-12)
  expect_equal(result$estimates$var.total, 2541898921288812, tolerance = 1e-12)
  expect_equal(H1DataObj, original)
  expect_identical(H1EstObj, estBefore)
})

test_that("documented artificial census links require explicit identification", {
  H1DataObj <- Pckg_SDAResources_agsrs_H1
  H1EstObj <- createRDBESEstObject(H1DataObj, 1, stopTable = "SA")
  before <- data.table::copy(H1EstObj)
  expect_error(estimateSrsworTotal(H1EstObj, "SAsampWtMes"),
               "Observed sampled-unit count.*declared n.*FO")
  expect_warning(result <- estimateSrsworTotal(H1EstObj, "SAsampWtMes",
                                               censusLinks = "FO"),
                 "artificial census link FO")
  y <- H1DataObj$SA$SAsampWtMes
  expect_equal(result$estimates$est.total, 3078 * mean(y))
  expect_equal(result$estimates$var.total,
               3078^2 * (1 - 300 / 3078) * stats::var(y) / 300)
  fo <- result$design[result$design$table == "FO", ]
  expect_true(all(fo$n == 1 & fo$N == 1 & fo$originaln == 10 & fo$originalN == 10))
  expect_identical(H1EstObj, before)
})

test_that("missing responses are omitted with design counts preserved", {
  object <- makeSrsworEstFixture()
  object$VSresponse[2] <- NA_real_
  before <- data.table::copy(object)
  expect_warning(result <- estimateSrsworTotal(object, "VSresponse", nBoot = 10),
                 "1 missing response.*omitted")
  expect_equal(result$estimates$est.total, c(24, 24))
  expect_true(is.na(result$estimates$var.total[1]))
  expect_true(all(is.finite(result$replicates)))
  expect_true(all(result$design$n == 4))
  expect_identical(object, before)
  object$VSresponse <- rep(NA_real_, 4)
  expect_error(estimateSrsworTotal(object, "VSresponse"), "no observed responses")
})

test_that("non-census singleton zero variance is visible and can be rejected", {
  object <- makeSrsworEstFixture()[1, ]
  object$su1numSamp <- 1
  object$su1incProb <- 1 / 8
  expect_warning(result <- estimateSrsworTotal(object, "VSresponse", nBoot = 5),
                 "singleton.*zero variance")
  expect_equal(result$estimates$est.total, c(24, 24))
  expect_equal(result$estimates$var.total, c(0, 0))
  expect_error(estimateSrsworTotal(object, "VSresponse", singleton = "fail"),
               "singleton.*variance cannot be estimated")
})

test_that("unsupported designs and inconsistent counts fail before estimation", {
  object <- makeSrsworEstFixture()
  for (method in c("SRSWR", "UPSWOR", "UPSWR")) {
    changed <- data.table::copy(object)
    changed$su1selectMeth <- method
    expect_error(estimateSrsworTotal(changed, "VSresponse"),
                 paste0("Unsupported selection method at VS: ", method))
  }
  changed <- data.table::copy(object)
  changed$su1numSamp <- 3
  expect_error(estimateSrsworTotal(changed, "VSresponse"), "Observed sampled-unit count")
  changed <- data.table::copy(object)
  changed$su1incProb[1] <- 0.25
  expect_error(estimateSrsworTotal(changed, "VSresponse"), "Inclusion probabilities")
  changed <- data.table::copy(object)
  changed$su1clustering <- "Y"
  expect_error(estimateSrsworTotal(changed, "VSresponse"), "clustering")
  changed <- data.table::copy(object)
  changed$VSresponse[1] <- Inf
  expect_error(estimateSrsworTotal(changed, "VSresponse"), "finite")
  changed <- data.table::copy(object)
  changed$VSid[2] <- changed$VSid[1]
  expect_error(estimateSrsworTotal(changed, "VSresponse"), "Observed sampled-unit count")
})

test_that("H8 replacement stage is rejected without changing the packaged data", {
  H8DataObj <- H8ExampleEE1
  before <- data.table::copy(H8DataObj)
  H8EstObj <- createRDBESEstObject(H8DataObj, 8, stopTable = "SA")
  expect_error(estimateSrsworTotal(H8EstObj, "SAsampWtMes"),
               "Unsupported selection method at VS: SRSWR")
  expect_equal(H8DataObj, before)
})

test_that("bootstrap and interval controls are validated", {
  object <- makeSrsworEstFixture()
  for (nBoot in list(1, -1, NA_real_, Inf, 2.5, c(0, 10), "10")) {
    expect_error(estimateSrsworTotal(object, "VSresponse", nBoot = nBoot), "nBoot")
  }
  for (level in list(0, 1, -0.5, NA_real_, Inf, c(0.9, 0.95), "0.95")) {
    expect_error(estimateSrsworTotal(object, "VSresponse", confidenceLevel = level),
                 "confidenceLevel")
  }
  census <- data.table::copy(object)
  census$su1numTotal <- 4
  census$su1incProb <- 1
  census$su1selectMeth <- "CENSUS"
  result <- estimateSrsworTotal(census, "VSresponse",
                                confidenceLevel = 1 - .Machine$double.eps / 2)
  expect_equal(result$estimates$lower, 16)
  expect_equal(result$estimates$upper, 16)
})
