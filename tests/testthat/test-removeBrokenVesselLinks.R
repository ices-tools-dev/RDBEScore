test_that("removeBrokenVesselLinks runs without errors
          or warnings when there are no invalid vessel links",  {

  myPath <- "./h1_v_1_19_26"
  myH1RawObject <-
    importRDBESDataCSV(rdbesExtractPath = myPath)

  myObjectValidVesselLinks <- expect_warning(
    removeBrokenVesselLinks(objectToCheck = myH1RawObject, verbose = FALSE)
    ,NA)

})
test_that("removeBrokenVesselLinks produces an error if the VD table is null ",  {

  myPath <- "./h1_v_1_19_26"
  myH1RawObject <-
    importRDBESDataCSV(rdbesExtractPath = myPath)
  myH1RawObject["VD"] <- list(NULL)

  myObjectValidVesselLinks <- expect_error(
  removeBrokenVesselLinks(objectToCheck = myH1RawObject, verbose = FALSE)
              ,"The VD entry in in objectToCheck is null - cannot check for broken vessel details links")

})
test_that("removeBrokenVesselLinks runs without errors
          or warnings when there are invalid vessel links",  {

  myPath <- "./h1_v_1_19_26"
  myH1RawObject <-
      importRDBESDataCSV(rdbesExtractPath = myPath)
  myFields <- c("VDlenCat")
  myValues <- c("18-<24" )
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )
  myObjectValidVesselLinks <- expect_warning(
    removeBrokenVesselLinks(objectToCheck = myFilteredObject,verbose = FALSE)
    ,NA)

})
