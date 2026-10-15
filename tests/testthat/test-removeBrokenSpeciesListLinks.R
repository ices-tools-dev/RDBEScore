capture.output({  ## suppresses printing of console output when running test()

test_that("removeBrokenSpeciesListLinks runs without errors
          or warnings when there are no invalid species list links",  {

    myPath <- "./h1_v_20250211"
    myH1RawObject <-
    importRDBESDataCSV(rdbesExtractPath = myPath)

})

test_that("removeBrokenSpeciesListLinks prduces an error
          if the SL table is NULL",  {

    myPath <- "./h1_v_20250211"
    myH1RawObject <-
    importRDBESDataCSV(rdbesExtractPath = myPath)
    myH1RawObject["SL"] <- list(NULL)

    myObjectValidSpeciesListLinks <- expect_error(
    removeBrokenSpeciesListLinks(objectToCheck = myH1RawObject,
                                           verbose = FALSE)
              ,"The SL entry in in objectToCheck is null - cannot check for broken species list links")

})
test_that("removeBrokenSpeciesListLinks runs without errors
          or warnings when there are invalid species list links",  {

    myPath <- "./h1_v_20250211"
    myH1RawObject <-
      importRDBESDataCSV(rdbesExtractPath = myPath)
    myFields <- c("SLspeclistName")
    myValues <- c("WGRDBES-EST TEST 5 - sprat data" )
    myFilteredObject <- filterRDBESDataObject(myH1RawObject,
                                               fieldsToFilter = myFields,
                                               valuesToFilter = myValues )
    myObjectValidSpeciesListLinks <- expect_warning(
        removeBrokenSpeciesListLinks(objectToCheck = myFilteredObject
                                     ,verbose = FALSE)
                                    ,NA)
})

test_that("removeBrokenSpeciesListLinks keeps all SS rows when SL links are valid", {
  myObj <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  res <- removeBrokenSpeciesListLinks(myObj, verbose = FALSE)
  expect_equal(nrow(res[["SS"]]), nrow(myObj[["SS"]]))
})

test_that("removeBrokenSpeciesListLinks removes all SS rows when no SL rows remain", {
  myObj <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  filtered <- filterRDBESDataObject(myObj, c("SLspeclistName"), c("xxx"))
  res <- removeBrokenSpeciesListLinks(filtered, verbose = FALSE)
  expect_equal(nrow(res[["SS"]]), 0)
})

test_that("removeBrokenSpeciesListLinks removes SS rows when SL catch fraction does not match", {
  myObj <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  expect_gt(nrow(myObj[["SS"]]), 0)
  myObj[["SL"]]$SLcatchFrac <- "xxx"
  res <- removeBrokenSpeciesListLinks(myObj, verbose = FALSE)
  expect_equal(nrow(res[["SS"]]), 0)
})

test_that("removeBrokenSpeciesListLinks removes SS rows when SL year or country does not match", {
  myObj <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  objYear <- myObj
  objYear[["SL"]]$SLyear[] <- NA
  expect_equal(nrow(removeBrokenSpeciesListLinks(objYear, verbose = FALSE)[["SS"]]), 0)
  objCou <- myObj
  objCou[["SL"]]$SLcou[] <- NA
  expect_equal(nrow(removeBrokenSpeciesListLinks(objCou, verbose = FALSE)[["SS"]]), 0)
})

}) ## end capture.output
