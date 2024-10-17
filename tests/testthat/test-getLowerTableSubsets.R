
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
               "Table XYZ not found in the RDBESData object")
})

test_that("Function returns correct data with valid inputs", {
  res <- getLowerTableSubsets(list(TEstratumName = "January"), "SA", H8ExampleEE1, F)

  expect_equal(nrow(res), 3)
  #expect last column to be "TEstratumName"
  expect_equal(colnames(res)[ncol(res)], "TEstratumName")

})

test_that("Function works with combineStrata = TRUE and collapsing strata", {
  res <- expect_warning(getLowerTableSubsets(list(TEstratumName = c("January", "February")), "SA", H8ExampleEE1, T))

  expect_equal(nrow(res), 6)
  #expect last column to be "TEstratumName"
  expect_equal(colnames(res)[ncol(res)], "TEstratumName")
  #ecpect all values in ast column to be January|February
  expect_equal(all(grepl("January|February", res$TEstratumName)), TRUE)

})

test_that("Function works with empty subsets", {
  res <- getLowerTableSubsets(list(TEstratumName = character(0)), "SA", H8ExampleEE1, F)

  expect_equal(nrow(res), 0)

})

test_that("Function handles intersection correctly", {
  res <- getLowerTableSubsets(list(TEid = 4, TEstratumName = "January"), "SA", H8ExampleEE1, F)

  expect_equal(nrow(res), 2)
  #expect last column to be "TEstratumName"
  expect_equal(colnames(res)[ncol(res)], "TEstratumName")
  #expect value in last column to be "January"
  expect_equal(res$TEstratumName, rep("January",2))

})
