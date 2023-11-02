
library(remotes)
install_github("ices-tools-dev/icesRDBES@dev")
library(icesRDBES)

getwd()

mySurveyH1 <- RDBEScore::createRDBESDataObject('data-raw/WGRDBES-EST_TEST_Pckg_Survey_data_apistrat_H1.zip')
validateRDBESDataObject(mySurveyH1, checkDataTypes = TRUE)
runChecksOnSelectionAndProbs(mySurveyH1)

# Inclusion of more species in SL to test the function 'generateZerosUsingSL'
names(mySurveyH1[["SL"]])
df1 <- data.frame('31830','SL','ZW','4484','WGRDBES-EST_TEST_Pckg_Survey_data_apistrat_H1','1968','Lan','119605','119605')
colnames(df1) <- names(mySurveyH1[["SL"]])

df2 <- data.frame('31831','SL','ZW','4484','WGRDBES-EST_TEST_Pckg_Survey_data_apistrat_H1','1968','Dis','119605','119605')
colnames(df2) <- names(mySurveyH1[["SL"]])

mySurveyH1[["SL"]] <- rbind(mySurveyH1[["SL"]],df1,df2)
mySurveyH1[["SL"]]$SLid <- as.integer(mySurveyH1[["SL"]]$SLid)
mySurveyH1[["SL"]]$SLyear <- as.integer(mySurveyH1[["SL"]]$SLyear)
mySurveyH1[["SL"]]$SLcommTaxon <- as.integer(mySurveyH1[["SL"]]$SLcommTaxon)
mySurveyH1[["SL"]]$SLsppCode <- as.integer(mySurveyH1[["SL"]]$SLsppCode)

# validation after changes
validateRDBESDataObject(mySurveyH1, checkDataTypes = TRUE)
runChecksOnSelectionAndProbs(mySurveyH1)

myTest <- filterRDBESDataObject(mySurveyH1, c("SLspeclistName"), c("WGRDBES-EST_TEST_Pckg_Survey_data_apistrat_H1"), killOrphans = TRUE)
myTest <- removeBrokenSpeciesListLinks(myTest)

validateRDBESDataObject(myTest)

# Set stratification to N
#myTest[["VS"]]$VSstratification = "N"

runChecksOnSelectionAndProbs(myTest)

myTest2 <- applyGenerateProbs(myTest, "inclusion", TRUE)

validateRDBESDataObject(myTest2)

myTest3 <- generateZerosUsingSL(myTest2)

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



