capture.output({  ## suppresses printing of console output when running test()

test_that("printing gives a warning when multiple hierarchies", {
  a <- H1Example
  a$DE$DEhierarchy[1] <- 2
  expect_warning(print(a),"Mixed hierarchy RDBESDataObject!")
})

test_that("printing produces output for hierarchy", {
  expect_output(print(H1Example),"^Hierarchy 1")
})

test_that("printing produces expected upper tables", {
  expect_output(print(H1Example),"DE:")
  expect_output(print(H1Example),"SD:")
  expect_output(print(H1Example),"VS:")
  expect_output(print(H1Example),"FT:")
  expect_output(print(H1Example),"FO:")
  expect_output(print(H1Example),"SS:")
  expect_output(print(H1Example),"SA:")
})

}) #end capture.output
