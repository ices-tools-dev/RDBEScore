capture.output({  ## suppresses printing of console output when running test()


# common parameters
# H1 directory
dirH1 <- "./h1_v_20250211/"
# H5 directory
dirH5 <- "./h5_v_20250211/"


createH1TestData<- function(){

  #setwd("./tests/testthat/")
  myH1 <- createRDBESDataObject(input = dirH1)
  myH1 <- filterAndTidyRDBESDataObject(myH1,
                                       fieldsToFilter =c("DEsampScheme"),
                                       valuesToFilter = c("National Routine"))

  # Keep 5 SA rows
  numOfRows <- 5
  myH1[["SA"]] <- head(myH1[["SA"]],numOfRows)
  myH1 <- findAndKillOrphans(myH1)

  # Just keep 1 row of the SL data
  myH1[["SL"]] <- head( myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList1" &
                 myH1[["SL"]]$SLcatchFrac=="Lan",],1)

  myH1

}

createH5TestData <- function(){

  myH5 <- createRDBESDataObject(input = dirH5)
  myH5 <- filterAndTidyRDBESDataObject(myH5,
                                       fieldsToFilter =c("DEsampScheme"),
                                       valuesToFilter = c("National Routine"))

  numOfRows <- 5
  myH5[["SA"]] <- head(myH5[["SA"]],numOfRows)
  myH5 <- findAndKillOrphans(myH5)

  # Just keep the first row of the SL data
  myH5[["SL"]] <- head( myH5[["SL"]][myH5[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList" &
                                       myH5[["SL"]]$SLcatchFrac=="Lan",],1)

  myH5

}


test_that("updateSAwithTaxonFromSL changes sprat to clupeidae when it is in the Species List - Family", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Sprat(126425) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126425", numOfRows)

  # Change IS species codes to Clupeidae(125464) to test the function
  SLidToUse <- myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList1" &
                              myH1[["SL"]]$SLcatchFrac=="Lan","SLid"][[1]]
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"ISsppCode"] <- 125464
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"IScommTaxon"] <- 125464

  # Run the function
  myH1renamed <- updateSAwithTaxonFromSL(myH1)

  # Check that the species codes have been changed to Clupeidae(125464)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("125464", numOfRows))

})

# using testthat test that the function updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Gadus (125732) - Genus
test_that("updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Gadus (125732) - Genus", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Gadus Morpha(126436) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126436", numOfRows)

  # Change IS species codes to Gadus(125732) to test the function
  SLidToUse <- myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList1" &
                              myH1[["SL"]]$SLcatchFrac=="Lan","SLid"][[1]]
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"ISsppCode"] <- 125732
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"IScommTaxon"] <- 125732

  # Run the function
  myH1renamed <- updateSAwithTaxonFromSL(myH1)

  # Check that the species codes have been changed to Gadus(125732)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("125732", numOfRows))

})

# using testthat test that the function updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Gadidae (125469) - Family
test_that("updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Gadidae (125469)", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Gadus Morpha(126436) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126436", numOfRows)

  # Change IS species codes to Gadidae(125469) to test the function
  SLidToUse <- myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList1" &
                              myH1[["SL"]]$SLcatchFrac=="Lan","SLid"][[1]]
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"ISsppCode"] <- 125469
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"IScommTaxon"] <- 125469

  # Run the function
  myH1renamed <- updateSAwithTaxonFromSL(myH1)

  # Check that the species codes have been changed to Gadidae(125469)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("125469", numOfRows))

})

# using testthat test that the function updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Gadiformes (10313) - Order
test_that("updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Gadiformes (10313) - Order", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Gadus Morpha(126436) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126436", numOfRows)

  # Change IS species codes to Gadiformes(10313) to test the function
  SLidToUse <- myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList1" &
                              myH1[["SL"]]$SLcatchFrac=="Lan","SLid"][[1]]
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"ISsppCode"] <- 10313
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"IScommTaxon"] <- 10313

  # Run the function
  myH1renamed <- updateSAwithTaxonFromSL(myH1)

  # Check that the species codes have been changed to Gadiformes(10313)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("10313", numOfRows))

})

# using testthat test that the function updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Teleostei (293496) - Class
test_that("updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Teleostei (293496) - Class", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Gadus Morpha(126436) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126436", numOfRows)

  # Change IS species codes to Teleostei(293496) to test the function
  SLidToUse <- myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList1" &
                              myH1[["SL"]]$SLcatchFrac=="Lan","SLid"][[1]]
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"ISsppCode"] <- 293496
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"IScommTaxon"] <- 293496

  # Run the function
  myH1renamed <- updateSAwithTaxonFromSL(myH1)

  # Check that the species codes have been changed to Teleostei(293496)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("293496", numOfRows))

})



# test that the function updateSAwithTaxonFromSL changes Melanogrammus aeglefinus (126437) to Animalia (2) - Kingdom
test_that("updateSAwithTaxonFromSL changes Melanogrammus aeglefinus (126437) to Animalia (2) - Kingdom", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Melanogrammus aeglefinus(126437) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126437", numOfRows)

  # Change IS species codes to Animalia(2) to test the function
  SLidToUse <- myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList1" &
                              myH1[["SL"]]$SLcatchFrac=="Lan","SLid"][[1]]
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"ISsppCode"] <- 2
  myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"IScommTaxon"] <- 2

  # Run the function
  myH1renamed <- updateSAwithTaxonFromSL(myH1)

  # Check that the species codes have been changed to Animalia(2)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("2", numOfRows))

})


# using testthat test that the function updateSAwithTaxonFromSL changes
# Gadus Morpha (126436) to Teleostei 293496 when both Genus (Gadus 125732) and
# Class (Teleostei 293496) are present
test_that("updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Teleostei (293496) - Genus and Class present (1)", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Gadus Morpha(126436) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126436", numOfRows)

  SLidToUse <- myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList1" &
                              myH1[["SL"]]$SLcatchFrac=="Lan","SLid"][[1]]
  numISRows <- nrow(myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,])

  # Change first half of IS species codes to Gadus(125732)
  firstISRowIDs <- head(myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"ISid"],numISRows/2)
  myH1[["IS"]][myH1[["IS"]]$ISid %in% firstISRowIDs$ISid,"ISsppCode"] <- 125732
  myH1[["IS"]][myH1[["IS"]]$ISid %in% firstISRowIDs$ISid,"IScommTaxon"] <- 125732

  # Change second half of IS species codes to Teleostei (293496)
  lastISRowIDs <- tail(myH1[["IS"]][myH1[["IS"]]$SLid == SLidToUse,"ISid"],numISRows/2)
  myH1[["IS"]][myH1[["IS"]]$ISid %in% lastISRowIDs$ISid,"ISsppCode"] <- 293496
  myH1[["IS"]][myH1[["IS"]]$ISid %in% lastISRowIDs$ISid,"IScommTaxon"] <- 293496

  # Run the function
  myH1renamed <- updateSAwithTaxonFromSL(myH1)

  # Check that the species codes have been changed to Teleostei (293496)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("293496", numOfRows))

})

# using testthat test that the function updateSAwithTaxonFromSL changes
# Gadus Morpha (126436) to Teleostei 293496 when both Genus (Gadus 125732) and
# Class (Teleostei 293496) are present
# (Reverse the order of Genus and Class in SL compared to previous test)
test_that("updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Teleostei (293496) - Genus and Class present (2)", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Gadus Morpha(126436) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126436", numOfRows)


  SLidToUse <- myH1[["SL"]][myH1[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList1" &
                              myH1[["SL"]]$SLcatchFrac=="Lan","SLid"][[1]]
  numISRows <- nrow(myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,])

  # Change first half of IS species codes to Teleostei (293496)
  firstISRowIDs <- head(myH1[["IS"]][myH1[["IS"]]$SLid==SLidToUse,"ISid"],numISRows/2)
  myH1[["IS"]][myH1[["IS"]]$ISid %in% firstISRowIDs$ISid,"ISsppCode"] <- 293496
  myH1[["IS"]][myH1[["IS"]]$ISid %in% firstISRowIDs$ISid,"IScommTaxon"] <- 293496


  # Change second half of IS species codes to Gadus(125732)
  lastISRowIDs <- tail(myH1[["IS"]][myH1[["IS"]]$SLid == SLidToUse,"ISid"],numISRows/2)
  myH1[["IS"]][myH1[["IS"]]$ISid %in% lastISRowIDs$ISid,"ISsppCode"] <- 125732
  myH1[["IS"]][myH1[["IS"]]$ISid %in% lastISRowIDs$ISid,"IScommTaxon"] <- 125732


  # Run the function
  myH1renamed <- updateSAwithTaxonFromSL(myH1)

  # Check that the species codes have been changed to Teleostei (293496)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("293496", numOfRows))

})

test_that("updateSAwithTaxonFromSL does not change sprat to clupeidae when it is not in the Species List", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Sprat(126425) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126425", numOfRows)


  # Run the function
  myH1renamed <- updateSAwithTaxonFromSL(myH1)

  # Check that the species codes have been left as sprat(126425)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("126425", numOfRows))

})

# test that the function updateSAwithTaxonFromSL does not changes Gadus Morpha (126436) to Gadidae (125469) when it is not in the species list
test_that("updateSAwithTaxonFromSL does not change Gadus Morpha (126436) to Gadidae (125469) when it is not in the species list", {

  # Create test data
  myH1 <- createH1TestData()

  # Change SA species codes to Gadus Morpha(126436) to test the function
  numOfRows <- nrow(myH1[["SA"]])
  myH1[["SA"]]$SAspeCode[1:numOfRows] <- rep("126436", numOfRows)

  # Run the function
  myH1renamed <- updateSAwithTaxonFromSL(myH1)

  # Check that the species codes have been left as Gadus Morpha(126436)
  expect_equal(myH1renamed[["SA"]]$SAspeCode, rep("126436", numOfRows))

})

# using testthat test that the function updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Gadus (125732)
# using H5 test data
test_that("Using H5 data updateSAwithTaxonFromSL changes Gadus Morpha (126436) to Gadus (125732) - Genus", {

  # Create test data
  myH5 <- createH5TestData()

  # Change SA species codes to Gadus Morpha(126436) to test the function
  numOfRows <- nrow(myH5[["SA"]])
  myH5[["SA"]]$SAspeCode[1:numOfRows] <- rep("126436", numOfRows)

  # Change IS species codes to Gadus(125732) to test the function
  SLidToUse <- myH5[["SL"]][myH5[["SL"]]$SLspeclistName=="ZW_1965_SpeciesList" &
                              myH5[["SL"]]$SLcatchFrac=="Lan","SLid"][[1]]
  myH5[["IS"]][myH5[["IS"]]$SLid==SLidToUse,"ISsppCode"] <- 125732
  myH5[["IS"]][myH5[["IS"]]$SLid==SLidToUse,"IScommTaxon"] <- 125732

  # Run the function
  myH5renamed <- updateSAwithTaxonFromSL(myH5)

  # Check that the species codes have been changed to Gadus(125732)
  expect_equal(myH5renamed[["SA"]]$SAspeCode, rep("125732", numOfRows))

})


# Using testthat test the function updateSAwithTaxonFromSL with an invalid RDBESDataObject
test_that("updateSAwithTaxonFromSL with an invalid RDBESDataObject", {

  # Create test data
  myH1 <- createH1TestData()

  # Remove the SA data
  myH1$SA <- NULL

  # Check that the function returns an error
  expect_error(updateSAwithTaxonFromSL(myH1))

})


}) ## end capture.output
