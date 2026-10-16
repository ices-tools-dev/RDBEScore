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

test_that("getTablesInRDBESHierarchy follows the XSD nesting for hierarchies 6, 8, 11, 12 and 13",  {

  # Expected values read from data-raw/xsd (and SS.LEid for H11)
  expect_equal(getTablesInRDBESHierarchy(6, includeLowHierTables = FALSE),
               c("DE","SD","OS","FT","LE","FO","SS","SA"))
  expect_equal(getTablesInRDBESHierarchy(8, includeLowHierTables = FALSE),
               c("DE","SD","TE","VS","LE","FT","SS","SA"))
  expect_equal(getTablesInRDBESHierarchy(11, includeLowHierTables = FALSE),
               c("DE","SD","LO","TE","FT","LE","SS","SA"))
  expect_equal(getTablesInRDBESHierarchy(12, includeLowHierTables = FALSE),
               c("DE","SD","LO","TE","LE","FT","SS","SA"))
  expect_equal(getTablesInRDBESHierarchy(13, includeLowHierTables = FALSE),
               c("DE","SD","FO","FT","SS","SA"))

  suOnly <- function(h) {
    getTablesInRDBESHierarchy(h, includeLowHierTables = FALSE,
                              includeTablesNotInSampHier = FALSE)
  }
  expect_equal(suOnly(6), c("DE","SD","OS","FT","FO","SS","SA"))
  expect_equal(suOnly(8), c("DE","SD","TE","VS","LE","SS","SA"))
  expect_equal(suOnly(11), c("DE","SD","LO","TE","FT","SS","SA"))
  expect_equal(suOnly(12), c("DE","SD","LO","TE","LE","SS","SA"))
  expect_equal(suOnly(13), c("DE","SD","FO","SS","SA"))

})

}) ## end capture.output
