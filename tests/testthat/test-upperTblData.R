DE <- data.table(DEid = c(1, 2))
SD <- data.table(SDid = c(1, 2), DEid = c(1, 2))
VS <- data.table(VSid = c(1, 2), SDid = c(1, 2), value = c(10, 20))
tbls <- list(DE = DE, SD = SD, VS = VS)

test_that("upperTblData returns correct data for VSid = 1", {

  result <- upperTblData("VSid", c(1), tbls, "DE")
  expected <- data.table(DEid = 1)

  expect_equal(result, expected)
})

test_that("upperTblData returns correct data for VSid = 2", {

  result <- upperTblData("VSid", c(2), tbls, "DE")
  expected <- data.table(DEid = 2)

  expect_equal(result, expected)
})

test_that("upperTblData handles non-existent VSid", {

  result <- upperTblData("VSid", c(3), tbls, "DE")
  expected <- data.table(DEid = integer(0))

  expect_equal(result, expected)
})

test_that("upperTblData works with RDBESDataObjects", {

  result <- upperTblData("SAid", c(1, 2), H8ExampleEE1, "TE")

  expect_equal((nrow(result)), 1)
  expect_equal((result$TEid), 1)
})

test_that("upperTblData works with sorted RDBESDataObjects", {

  result <- upperTblData("SAid", c(1, 2), sort(H8ExampleEE1), "DE")

  expect_equal((nrow(result)), 1)
  expect_equal((result$DEid), 1)
})

test_that("upperTblData throws error for incorrect tbls type", {
  expect_error(upperTblData("VSid", c(1), list(DE = 1, SD = 2, VS = 3), "DE"), "object 'VSid' not found")
})

test_that("upperTblData throws error for incorrect level", {

  expect_error(upperTblData("VSid", c(1), tbls, "XX"), "XX must be a character string in the names of the tables")
})

