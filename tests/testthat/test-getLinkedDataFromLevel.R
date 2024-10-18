
# Test for Example 1: Going up in the table hierarchy to retrieve data from the DE table
test_that("Retrieve data from DE based on BVid", {
  result <- getLinkedDataFromLevel("BVid", c(1), H8ExampleEE1, "DE")

  # Check that the result is a data.table (or other expected data structure)
  expect_s3_class(result, "data.table")

  # Check that the result has 1 row, as expected
  expect_equal(nrow(result), 1)

})

# Test for Example 2: Going down in the table hierarchy to retrieve data from the SA table
test_that("Retrieve data from SA based on DEid", {
  result <- getLinkedDataFromLevel("DEid", c(1), H8ExampleEE1, "SA")

  # Check that the result is a data.table (or other expected data structure)
  expect_s3_class(result, "data.table")

  # Check that the result has 15 rows, as expected
  expect_equal(nrow(result), 15)

})

# Test for Example 3: Going up in the table hierarchy to retrieve data from the VS table based on BVfishId
test_that("Retrieve data from VS based on BVfishId", {
  result <- getLinkedDataFromLevel("BVfishId", c("410472143", "410472144"), H8ExampleEE1, "VS")

  # Check that the result is a data.table (or other expected data structure)
  expect_s3_class(result, "data.table")

  # Check that the result contains data (you can adjust this depending on known expected results)
  expect_equal(nrow(result), 1)

  # Optionally, check for specific values in the result

})

test_that("Error when rdbesTables is not of class RDBESDataObject", {
  # Pass an invalid class (e.g., a list) instead of RDBESDataObject
  invalid_rdbesTables <- list()

  # Expect the function to throw an error
  expect_error(
    getLinkedDataFromLevel("BVid", c(1), invalid_rdbesTables, "DE"),
    "rdbesTables must be of class RDBESDataObject"
  )
})

test_that("Error when level is not found in rdbesTables", {
  # Assume H8ExampleEE1 is a valid RDBESDataObject, but "XX" is not a valid level
  expect_error(
    getLinkedDataFromLevel("BVid", c(1), H8ExampleEE1, "XX"),
    "Table XX not found in the RDBESDataObject"
  )
})






