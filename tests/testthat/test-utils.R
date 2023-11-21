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


}) ## end capture.output
