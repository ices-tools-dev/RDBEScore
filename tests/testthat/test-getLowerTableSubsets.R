
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

})

test_that("Function works with empty subsets", {

})

test_that("Function handles intersection correctly", {

})
