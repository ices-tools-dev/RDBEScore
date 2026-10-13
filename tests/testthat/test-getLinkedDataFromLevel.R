# AI-assisted: Yes
# Review: rix133
capture.output({  ## suppresses printing of console output when running test()

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

  expect_equal(nrow(result), 2)
  expect_setequal(result$BVfishId, c("410472143", "410472144"))

})

test_that("optional source tables can be linked to required tables", {
  result <- getLinkedDataFromLevel(
    "FTid",
    H5Example$FT$FTid[1],
    H5Example,
    "DE"
  )

  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 1)
})

test_that("error when a required table is missing", {
  h5 <- data.table::copy(H5Example)
  h5[["SD"]] <- NULL

  expect_error(
    getLinkedDataFromLevel("DEid", c(1), h5, "BV"),
    "Missing required tables in the RDBESDataObject: SD"
  )
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

test_that("Error when mixed hierarchy in rdbesTables", {
  obj <- data.table::copy(H8ExampleEE1)
  # Add a DE row from a different hierarchy
  obj[["DE"]] <- rbind(obj[["DE"]], H1Example[["DE"]][1])


  expect_error(
    suppressWarnings(
      getLinkedDataFromLevel("BVid", c(1), obj, "TE")
    ),
    "Mixed hierarchy RDBESDataObject!"
  )
})

test_that("Retrieve data from VS based on BVfishId if There is an empty intermedate FM table", {
  h8 <- H8ExampleEE1

  #add FM table to the rdbes object
  h8[["FM"]] <- H1Example[["FM"]][0,]



  result <- getLinkedDataFromLevel("BVfishId", c("410472143", "410472144"), h8, "VS")

  # Check that the result is a data.table (or other expected data structure)
  expect_s3_class(result, "data.table")

  expect_equal(nrow(result), 2)
  expect_setequal(result$BVfishId, c("410472143", "410472144"))

  #check also downwards direction
  res2 <- getLinkedDataFromLevel("DEid", c(1), h8, "BV")

  expect_s3_class(res2, "data.table")
  expect_equal(nrow(res2), 3995)
})

test_that("Retrieve data from DE based on BVid retains the BVid field", {
  result <- getLinkedDataFromLevel("BVid", c(1), H8ExampleEE1, "DE")

  # Check that the result is a data.table (or other expected data structure)
  expect_s3_class(result, "data.table")

  # Check that the result has 1 row, as expected
  expect_equal(nrow(result), 1)

  # Check that the result has the correct field
  expect_true("BVid" %in% names(result))

  # Check that the BVid field has the correct value
  expect_equal(result$BVid, 1)
})

test_that("Retrieve data from TE based on DEid retains the DEid field", {
  result <- getLinkedDataFromLevel("DEid", c(1), H8ExampleEE1, "TE")

  # Check that the result is a data.table (or other expected data structure)
  expect_s3_class(result, "data.table")

  # Check that the result has 1 row, as expected
  expect_equal(nrow(result), 11)

  # Check that the result has the correct field
  expect_true("DEid" %in% names(result))

  # Check that the DEid field has the correct value
  expect_equal(result$DEid, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))
})

test_that("Retrieve data from FO based on DEid retains the DEid field", {
  result <- getLinkedDataFromLevel("DEid", c(5351,5393), H1Example, "FO")

  # Check that the result is a data.table (or other expected data structure)
  expect_s3_class(result, "data.table")

  # Check that the result has 1 row, as expected
  expect_equal(nrow(result), 320)

  # Check that the result has the correct field
  expect_true("DEid" %in% names(result))

  # Check that the DEid field has the correct value
  expect_equal(result$DEid, c(rep(5351, 300), rep(5393, 20)))
})

})




