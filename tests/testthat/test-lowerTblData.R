# AI-assisted: Yes
# Review: rix133
capture.output({  ## suppresses printing of console output when running test()

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

  expect_equal(result[, names(expected), with = FALSE], expected)
})

test_that("Function handles multiple values correctly", {
  result <- lowerTblData("TEid", c(1, 4), tblsSprat, "LE", FALSE)
  expected <- data.table(TEid=c(1,4) ,VSid = c(1, 4),LEid=c(1,4) , value = c(10, 4))

  expect_equal(result[, names(expected), with = FALSE], expected)
})

test_that("Function handles non-existent field gracefully", {
  result <- lowerTblData("TEid", c(99), tblsSprat, "LE", FALSE)
  expected <- data.table(TEid = integer(),VSid = integer(), LEid=integer() , value = integer())

  expect_equal(result[, names(expected), with = FALSE], expected)
})

test_that("Function works with printLevels = TRUE", {
  expect_output(lowerTblData("TEid", c(4), tblsSprat, "LE", TRUE), "TEid: 4")
})

test_that("Recursive functionality works", {
  result <- lowerTblData("SDid", c(1), tblsSprat, "LE", FALSE)
  expected <- data.table(SDid = 1, VSid = 1, LEid = 1, value = 10)

  expect_setequal(names(result), names(expected))
  expect_equal(result[, names(expected), with = FALSE], expected)
})

test_that("Intermediate hierarchy ids are not added to the result", {
  result <- lowerTblData("DEid", c(1), tblsSprat, "LE", FALSE)

  expect_setequal(names(result), c("DEid", names(LE)))
})

test_that("A non-id requested field is retained with its linked records", {
  TEnamed <- data.table(TEid = c(1, 2, 3, 4), SDid = c(1, 2, 3, 4),
                        TEstratumName = c("Jan", "Feb", "Jan", "Mar"))
  tbls <- list(DE = DE, SD = SD, TE = TEnamed, VS = VS, LE = LE)

  result <- lowerTblData("TEstratumName", c("Jan", "Feb"), tbls, "LE", FALSE)
  result <- result[order(LEid)]

  expect_setequal(names(result), c("TEstratumName", names(LE)))
  expect_equal(result$LEid, c(1, 2, 3))
  expect_equal(result$TEstratumName, c("Jan", "Feb", "Jan"))
})

test_that("Repeated requested values do not duplicate target records", {
  result <- lowerTblData("TEid", c(1, 1), tblsSprat, "LE", FALSE)

  expect_equal(nrow(result), 1)
})

test_that("The requested value replaces an unused target field", {
  result <- getLinkedDataFromLevel("LEid", 5, H8ExampleEE1, "SA")

  expect_equal(result$SAid, 5)
  expect_equal(result$LEid, 5)
})

test_that("Same-level queries retain the target columns without duplication", {
  result <- lowerTblData("VSid", c(2, 3), tblsSprat, "VS", FALSE)

  expect_setequal(names(result), names(VS))
  expect_equal(anyDuplicated(names(result)), 0)
  expect_equal(result$VSid, c(2, 3))
})

test_that("Empty intermediate tables are skipped", {
  tbls <- list(DE = DE, SD = SD, TE = TE, VS = VS[0], LE = data.table(TEid = 1:4, LEid = 1:4))
  result <- lowerTblData("SDid", c(2), tbls, "LE", FALSE)

  expect_setequal(names(result), c("SDid", "TEid", "LEid"))
  expect_equal(result$LEid, 2)
})

test_that("Linking fails clearly when the tables cannot be joined", {
  tbls <- list(DE = DE, SD = SD, TE = TE, VS = data.table(VSid = 1:4), LE = LE)

  expect_error(lowerTblData("TEid", c(1), tbls, "LE", FALSE),
               "Error in linking tables TE and VS")
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
})