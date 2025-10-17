# Define some mock data for testing
DE <- data.table(DEid = c(1, 2, 3, 4))
SD <- data.table(SDid = c(1, 2, 3, 4), DEid = c(1, 2, 3, 4))
TE <- data.table(TEid = c(1, 2, 3, 4), SDid = c(1, 2, 3, 4))
VS <- data.table(VSid = c(1, 2, 3, 4), TEid = c(1, 2, 3, 4))
LE <- data.table(VSid = 1:5, LEid = 1:5, value = c(10, 20, 3, 4, 6))

tblsSprat <- list(DE = DE, SD = SD, TE = TE, VS = VS, LE = LE)

# Begin test cases
test_that("Function returns correct data for valid inputs", {
  result <- lowerTblData("TEid", c(1), tblsSprat, "LE", FALSE)
  expected <- data.table(TEid = 1, VSid = 1, LEid=1, value = 10)

  expect_equal(result, expected)
})

test_that("Function handles multiple values correctly", {
  result <- lowerTblData("TEid", c(1, 4), tblsSprat, "LE", FALSE)
  expected <- data.table(TEid=c(1,4) ,VSid = c(1, 4),LEid=c(1,4) , value = c(10, 4))

  expect_equal(result, expected)
})

test_that("Function handles non-existent field gracefully", {
  result <- lowerTblData("TEid", c(99), tblsSprat, "LE", FALSE)
  expected <- data.table(TEid = integer(),VSid = integer(), LEid=integer() , value = integer())

  expect_equal(result, expected)
})

test_that("Function works with printLevels = TRUE", {
  expect_output(lowerTblData("TEid", c(4), tblsSprat, "LE", TRUE), "TEid: 4")
})

test_that("Recursive functionality works", {
  result <- lowerTblData("SDid", c(1), tblsSprat, "LE", FALSE)
  expected <- data.table(SDid = 1, TEid = 1,  VSid = 1, LEid=1, value = 10)

  expect_equal(result, expected)
})

# Test when lower table has no match
test_that("Function returns empty dataframe when no match is found", {
  result <- lowerTblData("TEid", c(99), tblsSprat, "LE", FALSE)
  expect_equal(nrow(result), 0)
})

test_that("Function works with RDBESdataObjects", {
  tbl <- lowerTblData("TEid", c(4), H8ExampleEE1, "SA", FALSE)
  expect_equal(nrow(tbl), 2)
})

test_that("Function works with sorted RDBESdataObjects", {
  tbl <- lowerTblData("DEid", c(1), sort(H8ExampleEE1), "SA", FALSE)
  expect_equal(nrow(tbl), 15)
})

# Test for wrong input type
test_that("Function handles invalid inputs gracefully", {
  expect_error(lowerTblData("invalidField", c(1), tblsSprat, "LE", FALSE))
})
