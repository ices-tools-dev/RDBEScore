capture.output({  ## suppresses printing of console output when running test()

test_that("validateRDBESEstObject does not produce errors or warnings",  {

  myEmptyObject <- createRDBESDataObject()
  myEStObj <- createRDBESEstObject(myEmptyObject,1)

  expect_warning(validateRDBESEstObject(myEStObj),NA)
  expect_error(validateRDBESEstObject(myEStObj),NA)
})
test_that("validateRDBESEstObject does not error for valid empty object",  {

  myEmptyObject <- createRDBESDataObject()
  myEStObj <- createRDBESEstObject(myEmptyObject,1)
  expect_error(validateRDBESEstObject(myEStObj),NA)

})
test_that("validateRDBESEstObject does not error for valid object from H1 data",  {

  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
  myEStObj <- createRDBESEstObject(myObject,1)
  expect_error(validateRDBESEstObject(myEStObj),NA)

})
test_that("validateRDBESEstObject does not error for valid object from H5 data",  {

  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h5_v_1_19_18")
  myEStObj <- createRDBESEstObject(myObject,5)
  expect_error(validateRDBESEstObject(myEStObj),NA)

})
test_that("validateRDBESEstObject errors for NA",  {

  expect_error(validateRDBESEstObject(NA, verbose = TRUE),
               regexp = "objectToCheck is NA")


})
test_that("validateRDBESEstObject errors for a data frame",  {

  myTemp <- data.frame(a=c(0,1))
  expect_error(validateRDBESEstObject(myTemp, verbose = TRUE),
               regexp = "objectToCheck is not of the class RDBESEstObject")


})
test_that("validateRDBESEstObject errors for an incorrectly defined class",  {

  myTemp <- data.frame(a=c(0,1))
  class(myTemp) <- c("RDBESEstObject", class(myTemp))
  expect_error(validateRDBESEstObject(myTemp, verbose = TRUE),
               regexp = "objectToCheck does not inherit from data.table")

})
test_that("validateRDBESEstObject errors for an RDBESDataObject",  {

  myEmptyObject <- createRDBESDataObject()
  expect_error(validateRDBESEstObject(myEmptyObject, verbose = TRUE),
               regexp = "objectToCheck is not of the class RDBESEstObject")

})

}) ## end capture.output
