#capture.output({  ## suppresses printing of console output when running test()


# common parameters
# H1 directory
dirH1 <- "./h1_v_1_19_26/"
# H5 directory
dirH5 <- "./h5_v_1_19_26/"
# H7 directory
dirH7 <- "./h7_v_1_19_26/"

createH1TestData<- function(){

  myH1 <- createRDBESDataObject(input = dirH1)
  myH1 <- filterAndTidyRDBESDataObject(myH1,
                                       fieldsToFilter =c("DEsampScheme"),
                                       valuesToFilter = c("National Routine"))

  numOfRows <- 5
  myH1[["SA"]] <- head(myH1[["SA"]],numOfRows)
  myH1 <- findAndKillOrphans(myH1)

  myH1

}


test_that("renameSpeciesSA changes sprat to clupeidae when it is in the Species List", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Sprat(126425) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126425", numOfRows)

  # Change SL species codes to Clupeidae(125464) to test the function
  myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList" &
                         myH1[["SL"]]$SLcatchFrac=="Lan","SLsppCode"] <- 125464
  myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList" &
                         myH1[["SL"]]$SLcatchFrac=="Lan","SLcommTaxon"] <- 125464

  # Run the function
  myH1renamed <- renameSpeciesSA(myH1, verbose = TRUE)

  # Check that the species codes have been changed to Clupeidae(125464)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("125464", numOfRows))

})

test_that("renameSpeciesSA does not change sprat to clupeidae when it is not in the Species List", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Sprat(126425) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126425", numOfRows)

  # Change SL species codes to Clupeidae(125464) to test the function
  #myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList" &
  #               myH1[["SL"]]$SLcatchFrac=="Lan","SLsppCode"] <- 125464
  #myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList" &
  #               myH1[["SL"]]$SLcatchFrac=="Lan","SLcommTaxon"] <- 125464

  # Run the function
  myH1renamed <- renameSpeciesSA(myH1, verbose = TRUE)

  # Check that the species codes have been left as sprat(126425)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("126425", numOfRows))

})




#}) ## end capture.output
