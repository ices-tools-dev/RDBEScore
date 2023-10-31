capture.output({  ## suppresses printing of console output when running test()
test_that("summary has correct structure", {
  res <- summary(H1Example)
  expect_named(res, c("hierarchy", "rows", "CS"))
  expect_type(res, "list")
  expect_type(res$hierarchy, "integer")
  expect_type(res$rows, "list")
  expect_type(res$CS, "logical")
})



})
