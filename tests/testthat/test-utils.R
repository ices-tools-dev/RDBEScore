capture.output({  ## suppresses printing of console output when running test()


# as.integer.or.dbl -------------------------------------------------------

test_that("as.integer.or.dbl successfully converts columns without large values to integer", {

  nums_as_nums <- data.frame(vals = 1:4)
  nums_as_nums_w_na <- data.frame(vals = c(1:4, NA))
  nums_as_nums_all_na <- data.frame(vals = rep(NA, 5))
  nums_as_char <- data.frame(vals = c("1", "2", "3", "4"))
  nums_as_char_w_NA <- data.frame(vals = c("1", "2", "3", NA))

  expect_is(as.integer.or.dbl(nums_as_nums[[1]]),
            "integer")
  expect_is(as.integer.or.dbl(nums_as_nums_w_na[[1]]),
            "integer")
  expect_is(as.integer.or.dbl(nums_as_nums_all_na[[1]]),
            "integer")

  expect_is(as.integer.or.dbl(nums_as_char[[1]]),
            "integer")
  expect_is(as.integer.or.dbl(nums_as_char_w_NA[[1]]),
            "integer")

})

test_that("as.integer.or.dbl successfully converts columns with large values to double/numeric", {

  nums_as_nums <- data.frame(vals = c(1, 2, 3, 4.1, 99999999999))
  nums_as_nums_w_na <- data.frame(vals = c(1, 2, 3, 4.1, 99999999999, NA))
  nums_as_char <- data.frame(vals = c("1", "2", "3", "4.1", "99999999999"))
  nums_as_char_w_na <- data.frame(vals = c("1", "2", "3", "4.1", "99999999999", NA))

  expect_is(as.integer.or.dbl(nums_as_nums[[1]]),
            "numeric")
  expect_identical(as.integer.or.dbl(nums_as_nums[[1]]),
                   c(1, 2, 3, 4, 99999999999)) # NOTE - the 4.1 should be rounded DOWN to an integer 4

  expect_is(as.integer.or.dbl(nums_as_nums_w_na[[1]]),
            "numeric")
  expect_identical(as.integer.or.dbl(nums_as_nums_w_na[[1]]),
                   c(1, 2, 3, 4, 99999999999, NA)) # NOTE - the 4.1 should be rounded DOWN to an integer 4

  expect_is(as.integer.or.dbl(nums_as_char[[1]]),
            "numeric")
  expect_is(as.integer.or.dbl(nums_as_char_w_na[[1]]),
            "numeric")
})


# extractHigherFields -----------------------------------------------------

# Small H1 path DE-SD-VS-FT-FO-SS; SS 53 links to FO 42 whose FTid 99 does not exist
makeH1Path <- function() {
  list(DE = data.table::data.table(DEid = c(1L, 2L), DEhierarchy = 1L, DEyear = c(2020L, 2021L)),
       SD = data.table::data.table(SDid = c(10L, 11L), DEid = c(1L, 2L), SDctry = c("EE", "LV")),
       VS = data.table::data.table(VSid = c(20L, 21L), SDid = c(10L, 11L)),
       FT = data.table::data.table(FTid = c(30L, 31L), VSid = c(20L, 21L)),
       FO = data.table::data.table(FOid = c(40L, 41L, 42L), FTid = c(31L, 30L, 99L)),
       SS = data.table::data.table(SSid = c(50L, 51L, 52L, 53L), FOid = c(41L, 40L, 40L, 42L)))
}

# AI-assisted: Yes
# Human review: rix133
# Notes/scope: Expected values traced by hand through the ids in makeH1Path().
test_that("extractHigherFields returns one value per SS row and NA for broken links", {
  obj <- makeH1Path()

  expect_equal(extractHigherFields(obj, "SS", "DEyear"), c(2020L, 2021L, 2021L, NA))
  expect_equal(extractHigherFields(obj, "SS", "SDctry"), c("EE", "LV", "LV", NA))
})

# AI-assisted: Yes
# Human review: rix133
test_that("extractHigherFields gives an error for duplicated ids or a missing field", {
  obj <- makeH1Path()
  expect_error(extractHigherFields(obj, "SS", "XXyear"), "'field' not found")

  obj$FT <- data.table::data.table(FTid = c(30L, 30L), VSid = c(20L, 21L))
  expect_error(extractHigherFields(obj, "SS", "DEyear"), "Duplicated FTid")
})


}) ## end capture.output
