capture.output({  ## suppresses printing of console output when running test()


# Make a data.frame and a data.table holding the same columns use as
# x <- makeDFandDT(id = c(3L, 1L, 2L), a = c("x3", "x1", "x2"))
makeDFandDT <- function(...) {
  df <- data.frame(...)
  list(df = df, dt = data.table::as.data.table(df))
}


# AI-assisted: No
# Human review: rix133
# Notes/scope: Expected results written by hand from the dplyr join conventions.
test_that("joinDT left join keeps all x rows in x order with y matches in y order", {
  x <- makeDFandDT(id = c(3L, 1L, 2L), a = c("x3", "x1", "x2"))
  y <- makeDFandDT(id = c(1L, 3L, 1L), b = c("y1a", "y3", "y1b"))

  res <- joinDT(x$dt, y$dt, by = "id")

  expected <- data.table::data.table(id = c(3L, 1L, 1L, 2L),
                                     a = c("x3", "x1", "x1", "x2"),
                                     b = c("y3", "y1a", "y1b", NA))
  expect_equal(res, expected)

  # Manual comparison code:
  # joinDT(x$df, y$df, by = "id")
  # dplyr::left_join(x$df, y$df, by = "id")
})

# AI-assisted: No
# Human review: rix133
# Notes/scope: Expected results written by hand from the dplyr join conventions.
test_that("joinDT inner join drops the rows without a match", {
  x <- makeDFandDT(id = c(3L, 1L, 2L), a = c("x3", "x1", "x2"))
  y <- makeDFandDT(id = c(1L, 3L, 4L), b = c("y1", "y3", "y4"))

  res <- joinDT(x$dt, y$dt, by = "id", type = "inner")

  expected <- data.table::data.table(id = c(3L, 1L),
                                     a = c("x3", "x1"),
                                     b = c("y3", "y1"))
  expect_equal(res, expected)

  # Manual comparison code:
  # joinDT(x$df, y$df, by = "id", type = "inner")
  # dplyr::inner_join(x$df, y$df, by = "id")
})

# AI-assisted: No
# Human review: rix133
# Notes/scope: Expected results written by hand from the dplyr join conventions.
test_that("joinDT right join appends the unmatched y rows at the end", {
  x <- makeDFandDT(id = c(1L, 2L, 5L), a = c("x1", "x2", "x5"))
  y <- makeDFandDT(id = c(4L, 2L, 1L), b = c("y4", "y2", "y1"))

  res <- joinDT(x$dt, y$dt, by = "id", type = "right")

  expected <- data.table::data.table(id = c(1L, 2L, 4L),
                                     a = c("x1", "x2", NA),
                                     b = c("y1", "y2", "y4"))
  expect_equal(res, expected)

  # Manual comparison code:
  # joinDT(x$df, y$df, by = "id", type = "right")
  # dplyr::right_join(x$df, y$df, by = "id")
})

# AI-assisted: Yes
# Human review: rix133
# Notes/scope: Expected results written by hand from the dplyr join conventions.
test_that("joinDT joins on differently named keys and adds suffixes to shared columns", {
  x <- makeDFandDT(k = c(1L, 2L), v = c("x1", "x2"))
  y <- makeDFandDT(key = c(2L, 1L), v = c("y2", "y1"))

  res <- joinDT(x$df, y$df, by = c("k" = "key"))
  expect_false(data.table::is.data.table(res))
  expect_equal(res, data.frame(k = c(1L, 2L),
                               v.x = c("x1", "x2"),
                               v.y = c("y1", "y2")))

  res2 <- joinDT(x$df, y$df, by = c("k" = "key"), suffix = c("", ".y"))
  expect_equal(names(res2), c("k", "v", "v.y"))

  # Manual comparison code:
  # joinDT(x$df, y$df, by = c("k" = "key"))
  # dplyr::left_join(x$df, y$df, by = c("k" = "key"))
  # joinDT(x$df, y$df, by = c("k" = "key"), suffix = c("", ".y"))
  # dplyr::left_join(x$df, y$df, by = c("k" = "key"), suffix = c("", ".y"))
})

# AI-assisted: No
# Human review: rix133
# Notes/scope: Expected results written by hand from the dplyr join conventions.
test_that("joinDT matches NA keys and returns all many-to-many combinations", {
  x <- makeDFandDT(id = c(NA, 1L, 1L), a = c(1L, 2L, 3L))
  y <- makeDFandDT(id = c(1L, NA, 1L), b = c(10L, 20L, 30L))

  res <- joinDT(x$dt, y$dt, by = "id", type = "inner")

  expected <- data.table::data.table(id = c(NA, 1L, 1L, 1L, 1L),
                                     a = c(1L, 2L, 2L, 3L, 3L),
                                     b = c(20L, 10L, 30L, 10L, 30L))
  expect_equal(res, expected)

  # Manual comparison code:
  # joinDT(x$df, y$df, by = "id", type = "inner")
  # dplyr::inner_join(x$df, y$df, by = "id", relationship = "many-to-many")
})

# AI-assisted: Yes
# Human review: rix133
# Notes/scope: Expected results written by hand from the dplyr join conventions.
test_that("joinDT uses a common key type and does not modify its inputs", {
  x <- makeDFandDT(id = c(1L, 2L), a = c("p", "q"))
  y <- makeDFandDT(id = c(2, 3), b = c("r", "s"))
  xBefore <- data.table::copy(x$dt)
  yBefore <- data.table::copy(y$dt)

  res <- joinDT(x$dt, y$dt, by = "id")

  expect_type(res$id, "double")
  expect_equal(res$b, c(NA, "r"))
  expect_equal(x$dt, xBefore)
  expect_equal(y$dt, yBefore)

  # Manual comparison code:
  # joinDT(x$df, y$df, by = "id")
  # dplyr::left_join(x$df, y$df, by = "id")
})

# AI-assisted: Yes
# Human review: rix133
# Notes/scope: ids above the integer limit (2147483647) are stored as double by
#   as.integer.or.dbl(); expected results written by hand.
test_that("joinDT matches large double ids exactly", {
  x <- makeDFandDT(id = c(3000000001, 2147483648, 5), a = c("x1", "x2", "x3"))
  y <- makeDFandDT(id = c(2147483648, 3000000002, 3000000001),
                   b = c("y2", "yNear", "y1"))

  res <- joinDT(x$dt, y$dt, by = "id")

  expect_identical(res$id, c(3000000001, 2147483648, 5))
  expect_equal(res$b, c("y1", "y2", NA))

  # Manual comparison code:
  # joinDT(x$df, y$df, by = "id")
  # dplyr::left_join(x$df, y$df, by = "id")
})

# AI-assisted: Yes
# Human review: rix133
# Notes/scope: e.g. SA$SAid is double while FM$SAid is integer; expected
#   results written by hand.
test_that("joinDT keeps a double key when joining double and integer ids", {
  # large double x ids must not be turned into integer (they would become NA)
  x <- makeDFandDT(id = c(3000000001, 5), a = c("x1", "x2"))
  y <- makeDFandDT(id = c(5L, 6L), b = c("y5", "y6"))
  res <- joinDT(x$dt, y$dt, by = "id")
  expect_identical(res$id, c(3000000001, 5))
  expect_equal(res$b, c(NA, "y5"))

  # small double x ids stay double too
  x2 <- makeDFandDT(id = c(1, 5), a = c("x1", "x2"))
  res2 <- joinDT(x2$dt, y$dt, by = "id")
  expect_identical(res2$id, c(1, 5))

  # unmatched large y ids are kept in a right join
  res3 <- joinDT(y$dt, x$dt, by = "id", type = "right")
  expect_identical(res3$id, c(5, 3000000001))
  expect_equal(res3$b, c("y5", NA))

  # Manual comparison code:
  # joinDT(x$df, y$df, by = "id")
  # dplyr::left_join(x$df, y$df, by = "id")
  # joinDT(x2$df, y$df, by = "id")
  # dplyr::left_join(x2$df, y$df, by = "id")
  # joinDT(y$df, x$df, by = "id", type = "right")
  # dplyr::right_join(y$df, x$df, by = "id")
})

# AI-assisted: Yes
# Human review: rix133
test_that("joinDT gives an error for missing or incompatible key columns", {
  expect_error(joinDT(data.table::data.table(id = 1L),
                      data.table::data.table(id = "1"),
                      by = "id"),
               "Incompatible join types")
  expect_error(joinDT(data.table::data.table(id = 1L),
                      data.table::data.table(other = 1L),
                      by = "id"),
               "missing from y")
})

})
