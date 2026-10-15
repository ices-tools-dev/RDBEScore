# AI-assisted: Yes
# Review: Pending
# Notes/scope: Hand-calculated two- and three-stage totals and variances, plus
# exhaustive enumeration of all 128 half-sample outcomes in a three-stage design.

makeThreeStageSrswor <- function() {
  list(
    data.frame(key = c("a", "b"), parent = ".root", stratum = "all",
               n = 2, N = 4),
    data.frame(key = c("a1", "a2", "b1", "b2"),
               parent = rep(c("a", "b"), each = 2), stratum = "all",
               n = 2, N = 4),
    data.frame(key = paste0("u", 1:8),
               parent = rep(c("a1", "a2", "b1", "b2"), each = 2),
               stratum = "all", n = 2, N = 4)
  )
}

test_that("two-stage expansion includes estimated child variances", {
  stages <- makeThreeStageSrswor()[1:2]
  # Child totals are 8 and 24. Their variances are 8 and 32.
  # The first-stage variance is 512 + 2 * (8 + 32) = 592.
  expect_equal(calculateSrsworTotal(stages, c(1, 3, 4, 8)),
               c(total = 64, variance = 592))
})

test_that("three-stage analytical expansion matches a hand calculation", {
  stages <- makeThreeStageSrswor()
  response <- c(1, 3, 4, 8, 2, 6, 10, 20)
  # Last-stage totals: 8, 24, 16, 60. Variances: 8, 32, 32, 200.
  # Second-stage totals: 64, 152. Variances: 592, 4336.
  # First-stage variance: 15488 + 2 * (592 + 4336) = 25344.
  expect_equal(calculateSrsworTotal(stages, response),
               c(total = 432, variance = 25344))
})

test_that("lower-stage strata combine within each parent before expansion", {
  stages <- list(
    data.frame(key = c("a", "b"), parent = ".root", stratum = "all",
               n = 2, N = 4),
    data.frame(key = paste0("u", 1:6), parent = rep(c("a", "b"), each = 3),
               stratum = rep(c("x", "x", "z"), 2),
               n = c(2, 2, 1, 2, 2, 1), N = c(4, 4, 1, 2, 2, 1))
  )
  # Parent totals are 8 + 5 = 13 and 8 + 7 = 15.
  # Parent variances are 8 and 0. Root variance is 8 + 2 * 8 = 24.
  expect_equal(calculateSrsworTotal(stages, c(1, 3, 5, 2, 6, 7)),
               c(total = 56, variance = 24))
})

test_that("Preston bootstrap has the exact three-stage mean and variance", {
  stages <- makeThreeStageSrswor()
  response <- c(1, 3, 4, 8, 2, 6, 10, 20)
  # There are seven independent choices of one of two sampled units:
  # one at the first stage, two at the second, and four at the third.
  # Enumerate their equiprobable joint distribution, avoiding Monte Carlo
  # tolerances and any dependency on R's particular random-number sequence.
  choices <- as.matrix(expand.grid(rep(list(1:2), 7)))
  bootstrap <- generateSrsworReplicates
  bootstrapEnvironment <- new.env(parent = environment(bootstrap))
  draw <- 0L
  bootstrapEnvironment$sample.int <- function(n, size) {
    stopifnot(n == 2L, size == 1)
    draw <<- draw + 1L
    choices[(draw - 1L) %/% 7L + 1L, (draw - 1L) %% 7L + 1L]
  }
  environment(bootstrap) <- bootstrapEnvironment
  totals <- bootstrap(stages, response, nrow(choices))

  expect_identical(draw, 7L * nrow(choices))
  expect_equal(mean(totals), 432)
  # A population variance divides by 128, since all outcomes are present.
  expect_equal(mean((totals - 432)^2), 25344)
  expect_true(all(is.finite(totals)))
})

test_that("census links preserve descendant uncertainty and random draws", {
  terminal <- data.frame(key = c("x", "y"), parent = "census",
                         stratum = "all", n = 2, N = 4)
  census <- data.frame(key = "census", parent = ".root", stratum = "all",
                       n = 1, N = 1)
  response <- c(1, 3)
  expect_equal(calculateSrsworTotal(list(census, terminal), response),
               c(total = 8, variance = 8))

  set.seed(927)
  withoutLink <- generateSrsworReplicates(list(terminal), response, 12)
  set.seed(927)
  withLink <- generateSrsworReplicates(list(census, terminal), response, 12)
  expect_identical(withLink, withoutLink)
})

test_that("census links after a sampled stage cannot erase its uncertainty", {
  stages <- makeThreeStageSrswor()
  stages[[2]]$N <- 2
  stages[[3]]$N <- 2
  response <- c(rep(1, 4), rep(3, 4))
  # Two of four PSUs are sampled. Their fully observed totals are 4 and 12.
  # T = 2 * (4 + 12) = 32, V = 16 * .5 * var(c(4, 12)) / 2 = 128.
  expect_equal(calculateSrsworTotal(stages, response),
               c(total = 32, variance = 128))

  bootstrap <- generateSrsworReplicates
  bootstrapEnvironment <- new.env(parent = environment(bootstrap))
  draw <- 0L
  bootstrapEnvironment$sample.int <- function(n, size) {
    stopifnot(n == 2L, size == 1L)
    draw <<- draw + 1L
    # Select PSU 1 in the first replicate and PSU 2 in the second. The six
    # lower-stage draws cannot affect totals because those stages are censuses.
    1L + ((draw - 1L) %/% 7L) %% 2L
  }
  environment(bootstrap) <- bootstrapEnvironment
  totals <- bootstrap(stages, response, 2)
  expect_identical(draw, 14L)
  expect_equal(totals, c(32 - 8 * sqrt(2), 32 + 8 * sqrt(2)))
  # These are both equiprobable outcomes, so use their distribution variance.
  expect_equal(mean((totals - 32)^2), 128)
})

test_that("singleton zero contributions retain descendant sampling fractions", {
  stages <- list(
    data.frame(key = "a", parent = ".root", stratum = "all", n = 1, N = 2),
    data.frame(key = c("x", "y"), parent = "a", stratum = "all", n = 2, N = 4)
  )
  # Zero top-stage contribution is an explicit caller policy. The lower-stage
  # contribution remains (2 / 1) * 8 = 16, rather than being discarded.
  expect_equal(calculateSrsworTotal(stages, c(1, 3)),
               c(total = 16, variance = 16))
  bootstrap <- generateSrsworReplicates
  bootstrapEnvironment <- new.env(parent = environment(bootstrap))
  draw <- 0L
  bootstrapEnvironment$sample.int <- function(n, size) {
    draw <<- draw + 1L
    draw
  }
  environment(bootstrap) <- bootstrapEnvironment
  expect_equal(bootstrap(stages, c(1, 3), 2), c(12, 20))
})

test_that("a complete census has no analytical or bootstrap uncertainty", {
  stages <- list(data.frame(key = letters[1:4], parent = ".root",
                            stratum = "all", n = 4, N = 4))
  expect_equal(calculateSrsworTotal(stages, c(1, 2, 4, 8)),
               c(total = 15, variance = 0))
  expect_equal(generateSrsworReplicates(stages, c(1, 2, 4, 8), 4), rep(15, 4))
})

test_that("missing responses omit contributions without changing design counts", {
  stages <- list(data.frame(key = letters[1:3], parent = ".root",
                            stratum = "all", n = 3, N = 12))
  response <- c(1, NA_real_, 4)
  expect_equal(calculateSrsworTotal(stages, response),
               c(total = 20, variance = NA_real_))

  # Enumerate the three equiprobable one-unit subsamples. Expansion weights
  # remain 12 / 3 = 4, including the sampled unit whose response is missing.
  bootstrap <- generateSrsworReplicates
  bootstrapEnvironment <- new.env(parent = environment(bootstrap))
  draw <- 0L
  bootstrapEnvironment$sample.int <- function(n, size) {
    draw <<- draw + 1L
    draw
  }
  environment(bootstrap) <- bootstrapEnvironment
  expect_equal(bootstrap(stages, response, 3),
               20 + c(-8, -20, 28) * sqrt(3 / 8))
  expect_identical(response, c(1, NA_real_, 4))

  # A census does not resolve the unknown full-population response.
  stages[[1L]]$N <- 3
  expect_equal(calculateSrsworTotal(stages, response),
               c(total = 5, variance = NA_real_))
})

test_that("estimation cores preserve caller-owned stages and responses", {
  stages <- makeThreeStageSrswor()
  response <- c(1, 3, 4, 8, 2, 6, 10, 20)
  originalStages <- unserialize(serialize(stages, NULL))
  originalResponse <- response
  calculateSrsworTotal(stages, response)
  generateSrsworReplicates(stages, response, 3)
  expect_identical(stages, originalStages)
  expect_identical(response, originalResponse)
})
