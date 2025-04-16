# Step 1: Generate independent random normal variables
n <- 100  # Number of samples
Lengthmm <- rnorm(n, mean = 100, sd = 1)  # Lengthmm variable
Ageyear <- rnorm(n, mean = 2, sd = 1)     # Ageyear variable
Weightg <- rnorm(n, mean = 10, sd = 1)    # Weightg variable


biolCLQ1 <- data.table(
  BVfishId = rep(1:100,each=3),
  BVvalueMeas = c(Lengthmm, Ageyear, Weightg),
  BVvalUnitScale = rep(c("Lengthmm", "Ageyear", "Weightg"), 100),
  sumCLoffWeight = 396210
)

# Test 1: Check the output structure of BVestimCANUM
test_that("doBVestimCANUM returns correct structure", {
  lenCANUMQ1 <- doBVestimCANUM(biolCLQ1, c("sumCLoffWeight"),
                             classUnits = "Lengthmm",
                             classBreaks = seq(70, 130, 10),
                             verbose = FALSE)

  # Check that the output is a data.table
  expect_true(is.data.table(lenCANUMQ1))

  # Check that the expected columns are present
  expected_columns <- c("Group", "WeightgMean", "WeightgLen", "lenMeas", "targetMeas",
                        "propSample", "WeightIndex", "totWeight", "totNum")
  expect_true(all(expected_columns %in% names(lenCANUMQ1)))
})

# Test 2: Check that Group is correctly classified
test_that("doBVestimCANUM correctly classifies Lengthmm groups", {
  lenCANUMQ1 <- doBVestimCANUM(biolCLQ1, c("sumCLoffWeight"),
                             classUnits = "Lengthmm",
                             classBreaks = seq(70, 130, 10),
                             verbose = FALSE)

  # Check that the Group column contains the correct labels
  expected_groups <- c("70-80", "80-90", "90-100", "100-110", "110-120", "120-130", "130+")
  actual_groups <- unique(lenCANUMQ1$Group)
  expect_true(all(actual_groups %in% expected_groups))
})

# Test 3: Check WeightgMean calculation
test_that("doBVestimCANUM calculates WeightgMean correctly", {
  lenCANUMQ1 <- doBVestimCANUM(biolCLQ1, c("sumCLoffWeight"),
                             classUnits = "Lengthmm",
                             classBreaks = seq(70, 130, 10),
                             verbose = FALSE)

  # Ensure WeightgMean is numeric and positive
  expect_true(is.numeric(lenCANUMQ1$WeightgMean))
  expect_true(all(lenCANUMQ1$WeightgMean > 0))
})


# Test 5: Sanity check for total weights
test_that("doBVestimCANUM performs a sanity check for total weights", {
  lenCANUMQ1 <- doBVestimCANUM(biolCLQ1, c("sumCLoffWeight"),
                             classUnits = "Lengthmm",
                             classBreaks = seq(70, 130, 10),
                             verbose = FALSE)

  # Check that the sum of totWeight equals sumCLoffWeight (within tolerance)
  total_weight <- sum(lenCANUMQ1$totWeight)
  expected_sum <- sum(unique(lenCANUMQ1$sumCLoffWeight))

  expect_true(isTRUE(all.equal(total_weight, expected_sum)))
})

# Test 6: Check TWCoef calculation
test_that("doBVestimCANUM calculates TWCoef correctly", {
  lenCANUMQ1 <- doBVestimCANUM(biolCLQ1, c("sumCLoffWeight"),
                             classUnits = "Lengthmm",
                             classBreaks = seq(70, 130, 10),
                             verbose = FALSE)

  # Recalculate TWCoef
  lenCANUMQ1[, expected_TWCoef := sumCLoffWeight / WeightIndexSum]

  # Ensure TWCoef is correctly calculated
  expect_equal(lenCANUMQ1$TWCoef, lenCANUMQ1$expected_TWCoef)
})

test_that("doBVestimCANUM works for age without lenght data", {
  biolCLQ1_noLength <- copy(biolCLQ1[BVvalUnitScale != "Lengthmm",])
  lenCANUMQ1 <- doBVestimCANUM(biolCLQ1_noLength, c("sumCLoffWeight"),
                               classUnits = "Ageyear",
                               classBreaks = 1:8,
                               verbose = FALSE)
  expect_true(all(c("Group", "WeightgMean", "WeightgLen", "lenMeas", "targetMeas","LengthmmMean",
                    "propSample", "WeightIndex", "totWeight", "totNum") %in% names(lenCANUMQ1)))

  expect_true(all(unique(lenCANUMQ1$Group) %in% c("1", "2", "3", "4", "5", "6", "7", "8+")))


  expect_true(all(is.nan(lenCANUMQ1$LengthmmMean)))


})

