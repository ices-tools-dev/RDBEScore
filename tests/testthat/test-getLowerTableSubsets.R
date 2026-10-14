
# AI-assisted: Yes
# Review: rix133
capture.output({  ## suppresses printing of console output when running test()


# Define mock data for testing
DE <- data.frame(DEid = c(1, 2, 3, 4), SDid = c(1, 2, 3, 4))
SD <- data.frame(SDid = c(1, 2, 3, 4), TEid = c(1, 2, 3, 4))
TE <- data.frame(SDid = c(1, 2, 3, 4), TEid = c(1, 2, 3, 4))
VS <- data.frame(TEid = c(1, 2, 3, 4), VSid = c(1, 2, 3, 4))
LE <- data.frame(VSid = c(1, 2, 3, 4, 5),LEid = 1:5, value = c(10, 20, 3, 4, 6))
tbls <- list(DE = DE, SD = SD, TE = TE, VS = VS, LE = LE)


# Begin test cases

test_that("Function checks for RDBESDataObject class", {
  subsets <- list(TEid = c(4))

  expect_error(getLowerTableSubsets(subsets, "VS", tbls),
               "rdbesTables must be of class RDBESDataObject")
})

test_that("Function throws error for missing table name", {
  subsets <- list(TEid = c(4))

  expect_error(getLowerTableSubsets(subsets, "XYZ", H8ExampleEE1),
               "Table XYZ not found in the RDBESDataObject")
})

test_that("Function returns correct data with valid inputs", {
  res <- getLowerTableSubsets(list(TEstratumName = "January"), "SA", H8ExampleEE1, T)

  expect_equal(nrow(res), 3)
  expect_true("TEstratumName" %in% names(res))

})

test_that("Function works with combineStrata = TRUE and collapsing strata", {
  res <- expect_warning(getLowerTableSubsets(list(TEstratumName = c("January", "February")), "SA", H8ExampleEE1, T))

  expect_equal(nrow(res), 6)
  expect_equal(unique(res$TEstratumName), "January|February")

})

test_that("Function works with empty subsets", {
  res <- getLowerTableSubsets(list(TEstratumName = character(0)), "SA", H8ExampleEE1, T)

  expect_equal(nrow(res), 0)

})

test_that("Function handles intersection correctly", {
  res <- getLowerTableSubsets(list(TEid = 4, TEstratumName = "January"), "SA", H8ExampleEE1, T)

  expect_equal(nrow(res), 2)
  expect_equal(res$TEstratumName, rep("January",2))

})

test_that("Function does not duplicate the requested fields in the result", {
  res <- getLowerTableSubsets(list(TEid = 4, TEstratumName = "January"), "SA", H8ExampleEE1, T)

  expect_equal(anyDuplicated(colnames(res)), 0)
  expect_equal(sum(colnames(res) == "TEstratumName"), 1)
  expect_equal(sum(colnames(res) == "TEid"), 1)
})
})