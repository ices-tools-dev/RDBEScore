capture.output({  ## suppresses printing of console output when running test()
test_that("summary has correct structure", {
  res <- summary(H1Example)
  expect_named(res, c("hierarchy", "data"))
  expect_type(res, "list")
  expect_type(res$hierarchy, "integer")
  expect_type(res$data, "list")
  expect_type(res$data$DE$design, "list")
  expect_type(res$data$DE$rows, "integer")
})



})
