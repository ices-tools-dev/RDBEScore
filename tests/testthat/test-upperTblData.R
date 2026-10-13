# AI-assisted: Yes
# Review: rix133
capture.output({  ## suppresses printing of console output when running test()


DE <- data.table(DEid = c(1, 2))
SD <- data.table(SDid = c(1, 2), DEid = c(1, 2))
VS <- data.table(VSid = c(1, 2), SDid = c(1, 2), VSvalue = c(10, 20))
tbls <- list(DE = DE, SD = SD, VS = VS)

test_that("upperTblData returns correct data for VSid = 1", {

  result <- upperTblData("VSid", c(1), tbls, "DE")
  expected <- data.table(VSid = 1, DEid = 1)

  expect_equal(result, expected)
})

test_that("upperTblData returns correct data for VSid = 2", {

  result <- upperTblData("VSid", c(2), tbls, "DE")
  expected <- data.table(VSid = 2, DEid = 2)

  expect_equal(result, expected)
})

test_that("upperTblData handles non-existent VSid", {

  result <- upperTblData("VSid", c(3), tbls, "DE")
  expected <- data.table(VSid = integer(0), DEid = integer(0))

  expect_equal(result, expected)
})

test_that("upperTblData works with RDBESDataObjects", {

  result <- upperTblData("SAid", c(1, 2), H8ExampleEE1, "TE")

  expect_equal(nrow(result), 2)
  expect_equal(result$SAid, c(1, 2))
  expect_equal(result$TEid, c(1, 1))
})

test_that("upperTblData works with sorted RDBESDataObjects", {

  result <- upperTblData("SAid", c(1, 2), sort(H8ExampleEE1), "DE")

  expect_equal(nrow(result), 2)
  expect_equal(result$SAid, c(1, 2))
  expect_equal(result$DEid, c(1, 1))
})


test_that("upperTblData throws error for incorrect level", {

  expect_error(upperTblData("VSid", c(1), tbls, "XX"), "Missing table in data: XX")
})

test_that("upperTblData has the in about the several rows requested also in the result", {

  result <- upperTblData("VSvalue", c(10,20), tbls, "DE")
  expected <- data.table(VSvalue = c(10,20), DEid = c(1,2))

  expect_equal(result, expected)
})

test_that("upperTblData keeps different requested values that share a target record", {

  SD2 <- data.table(SDid = c(1, 2), DEid = c(1, 1))
  tbls2 <- list(DE = DE, SD = SD2, VS = VS)

  result <- upperTblData("VSid", c(1, 2), tbls2, "DE")

  expect_setequal(names(result), c("VSid", "DEid"))
  expect_equal(result[, .(VSid, DEid)], data.table(VSid = c(1, 2), DEid = c(1, 1)))
})

test_that("upperTblData collapses repeated paths between a requested value and a target record", {

  VS2 <- data.table(VSid = c(1, 2), SDid = c(1, 1), VSvalue = c(10, 10))
  tbls2 <- list(DE = DE, SD = SD, VS = VS2)

  result <- upperTblData("VSvalue", c(10), tbls2, "DE")

  expect_equal(result[, .(VSvalue, DEid)], data.table(VSvalue = 10, DEid = 1))
})

test_that("upperTblData keeps the requested value when the target field is unused", {
  tbls2 <- list(DE = data.table(DEid = 1, VSid = NA_real_), SD = SD, VS = VS)
  result <- upperTblData("VSid", 1, tbls2, "DE")

  expect_equal(result$DEid, 1)
  expect_equal(result$VSid, 1)
})
})
