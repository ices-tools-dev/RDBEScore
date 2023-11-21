capture.output({  ## suppresses printing of console output when running test()

test_that("removeBrokenSpeciesListLinks runs without errors
          or warnings when there are no invalid species list links",  {

    myPath <- "./h1_v_1_19_18"
    myH1RawObject <-
    importRDBESDataCSV(rdbesExtractPath = myPath)

})

test_that("removeBrokenSpeciesListLinks prduces an error
          if the SL table is NULL",  {

    myPath <- "./h1_v_1_19_18"
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

    myPath <- "./h1_v_1_19_18"
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

}) ## end capture.output
