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

test_that("printing removes NA rows on character", {
  a <- H8ExampleEE1
  a$TE$TEsampMeth[1] <- NA
  expect_output(suppressWarnings(print(a)),'TE: 11 \\(SRSWOR: 2-3/4\\)')
})

test_that("printing removes NA rows on numeric", {
  a <- H8ExampleEE1
  a$TE$TEnumTotal[1] <- NA
  expect_output(suppressWarnings(print(a)),'TE: 11 (SRSWOR: 2-3/4)', fixed=T)
})

test_that("having NA on num total creates a warning", {
  a <- H8ExampleEE1
  a$TE$TEnumTotal[1] <- NA
  expect_warning(print(a),"TE: numTotal has NAs!")
})

test_that("having NA on sel Meth creates a warning", {
  a <- H8ExampleEE1
  a$TE$TEselectMeth[1] <- NA
  expect_warning(print(a),"TE: selectMeth has NAs!")
})

test_that("having NA on sel Meth and num total creates a warning", {
  a <- H8ExampleEE1
  a$TE$TEselectMeth[1] <- NA
  a$TE$TEnumTotal[1] <- NA
  expect_warning(print(a),"TE: selectMeth, numTotal have NAs!")
})

test_that("printing gives right nr of rows", {

  expect_output(print(H8ExampleEE1),'TE: 11')

  a <- H8ExampleEE1
  a$TE <- data.table()
  expect_output(print(a),'TE: 0\\n')
})

}) #end capture.output
