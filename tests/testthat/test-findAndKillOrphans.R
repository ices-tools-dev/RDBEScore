capture.output({  ## suppresses printing of console output when running test()

test_that("findAndKillOrphans runs without errors on an empty RDBESDataObject",  {

  myEmptyObject <- createRDBESDataObject()

  expect_error(findAndKillOrphans(objectToCheck = myEmptyObject,
                                  verbose = FALSE),NA)


})
test_that("findAndKillOrphans runs without errors on an RDBESDataObject with no orphans",  {

  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_26")

  expect_error(findAndKillOrphans(objectToCheck = myH1RawObject,
                                  verbose = FALSE),NA)

})
test_that("findAndKillOrphans removes orphans on an filtered RDBESDataObject",  {

  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_26")

  # Only use a subset of the test data
  myH1RawObject <- filterRDBESDataObject(myH1RawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

  # remove all the VS rows (but not any other rows)
  myFields <- c("VSunitName")
  myValues <- c("blah" )
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )

  # Check the expected number of rows are returned
  expect_equal(nrow(myFilteredObject[["VS"]]),0)
  expect_equal(nrow(myFilteredObject[["FT"]]),243)

  # Remove the orphans
  myObjectNoOrphans <- findAndKillOrphans(objectToCheck = myFilteredObject,
                                          verbose = FALSE)

  # Check the expected number of rows are returned
  expect_equal(nrow(myObjectNoOrphans[["DE"]]),3)
  expect_equal(nrow(myObjectNoOrphans[["SD"]]),3)
  expect_equal(nrow(myObjectNoOrphans[["VS"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["FT"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["FO"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["SS"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["SA"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["FM"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["BV"]]),0)

})

}) ## end capture.output
