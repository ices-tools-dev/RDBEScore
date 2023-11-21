capture.output({  ## suppresses printing of console output when running test()

test_that("getTablesInRDBESHierarchy fails for non-int hierarchy parameter H1",  {
  expect_error(getTablesInRDBESHierarchy("H1"),"hierarchy parameter must be an integer")
})

test_that("getTablesInRDBESHierarchy fails for non-int hierarchy parameter 1.2",  {

  expect_error(getTablesInRDBESHierarchy(1.2),"hierarchy parameter must be an integer")
})
test_that("getTablesInRDBESHierarchy fails for hierarchy parameter ouisde allowed range",  {

  expect_error(getTablesInRDBESHierarchy(14),"hierarchy parameter must be between 1 and 13")

})
test_that("getTablesInRDBESHierarchy works for hierachy 5 with default parameters",  {

  expectedResult <- c("DE","SD","OS","LE","FT","SS","SA","FM","BV")
  expect_equal(getTablesInRDBESHierarchy(5),expectedResult)

})
test_that("getTablesInRDBESHierarchy works for hierachy 5 excluding lower tables",  {

  expectedResult <- c("DE","SD","OS","LE","FT","SS","SA")
  actualResult <- getTablesInRDBESHierarchy(5,includeOptTables = TRUE,
                                            includeLowHierTables = FALSE,
                                            includeTablesNotInSampHier = TRUE)
  expect_equal(actualResult,expectedResult)

})
test_that("getTablesInRDBESHierarchy works for hierachy 5 excluding optional tables",  {

  expectedResult <- c("DE","SD","OS","LE","SS","SA","FM","BV")
  actualResult <- getTablesInRDBESHierarchy(5,includeOptTables = FALSE,
                                            includeLowHierTables = TRUE,
                                            includeTablesNotInSampHier = TRUE)
  expect_equal(actualResult,expectedResult)

})
test_that("getTablesInRDBESHierarchy works for hierachy 7 excluding tables not in sampling hierarchy",  {

  expectedResult <- c("DE","SD","OS","SS","SA","FM","BV")
  actualResult <- getTablesInRDBESHierarchy(7,includeOptTables = TRUE,
                                            includeLowHierTables = TRUE,
                                            includeTablesNotInSampHier = FALSE)
  expect_equal(actualResult,expectedResult)

})

}) ## end capture.output
