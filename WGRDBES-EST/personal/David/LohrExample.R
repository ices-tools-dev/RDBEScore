library(RDBEScore)


getwd()

myLohr <- RDBEScore::createRDBESDataObject('data-raw/WGRDBES-EST_TEST_LOHR_eg_3_2_3_6.zip')
validateRDBESDataObject(myLohr, checkDataTypes = TRUE)
runChecksOnSelectionAndProbs(myLohr)

# Filter to just 1 stratum
#myTest <- filterRDBESDataObject(myLohr, c("VSstratumName", "SLspeclistName"), c("NE", "WGRDBES-EST_TEST_LOHR_eg_3.2_3.6"), killOrphans = TRUE)
myTest <- filterRDBESDataObject(myLohr, c("SLspeclistName"), c("WGRDBES-EST_TEST_LOHR_eg_3.2_3.6"), killOrphans = TRUE)
myTest <- removeBrokenSpeciesListLinks(myTest)

validateRDBESDataObject(myTest)

# Set stratification to N
#myTest[["VS"]]$VSstratification = "N"

runChecksOnSelectionAndProbs(myTest)

myTest2 <- applyGenerateProbs(myTest, "inclusion", TRUE)

validateRDBESDataObject(myTest2)

myTest3 <- generateZerosUsingSL(myTest2)

#myTest3 <- myLohr

myTest3[["SA"]]$SAsampWtLive <- myTest3[["SA"]]$SAsampWtMes
myTest3[["SA"]]$SAnumSamp <- 1
myTest3[["SA"]]$SAnumTotal <- 1
myTest3[["FT"]]$FTnumSamp <- 1
myTest3[["FT"]]$FTnumTotal <- 1
myTest3[["FO"]]$FOnumSamp <- 1
myTest3[["FO"]]$FOnumTotal <- 1
myTest3[["SS"]]$SSnumSamp <- 1
myTest3[["SS"]]$SSnumTotal <- 1

myTest4 <- doEstimationForAllStrata(myTest3, 1)
View(myTest4[myTest4$recType == "VS",])

myEst <- createRDBESEstObject(myTest3, 1)



