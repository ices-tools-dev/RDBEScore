capture.output({  ## suppresses printing of console output when running test()

createTestData <- function(){

  # create test data from download [to be used in different tests]

  myH1DataObject <- RDBEScore::createRDBESDataObject("./h1_v_20250211/ZW_1965_WGRDBES-EST_TEST_1")

  # Only use a subset of the test data
  myH1DataObject0 <- filterRDBESDataObject(myH1DataObject,c("DEstratumName"),c("Pckg_survey_apistrat_H1"))
  myH1DataObject0 <- filterRDBESDataObject(myH1DataObject0,c("SLspeclistName"),c("WGRDBES-EST_TEST_1_Pckg_survey_apistrat_H1"))
  myH1DataObject0 <- findAndKillOrphans(myH1DataObject0)

  # Fix the SL institute
  myH1DataObject0[["SL"]]$SLinst = "4484"

  validateRDBESDataObject(myH1DataObject0, checkDataTypes = TRUE)

  df1SL <- data.frame('31831','SL','ZW','4484',myH1DataObject0[["SL"]]$SLspeclistName,'1965','Dis')
  df1IS <- data.frame('31832','31831','IS','107254','107254')

  data.table::setnames(df1SL, names(myH1DataObject0[["SL"]]))
  data.table::setnames(df1IS, names(myH1DataObject0[["IS"]]))
  myH1DataObject0[["SL"]] <- rbind(myH1DataObject0[["SL"]],df1SL)
  myH1DataObject0[["SL"]]$SLid <- as.integer(myH1DataObject0[["SL"]]$SLid)
  myH1DataObject0[["SL"]]$SLyear <- as.integer(myH1DataObject0[["SL"]]$SLyear)
  myH1DataObject0[["IS"]] <- rbind(myH1DataObject0[["IS"]],df1IS)
  myH1DataObject0[["IS"]]$ISid <- as.integer(myH1DataObject0[["IS"]]$ISid)
  myH1DataObject0[["IS"]]$SLid <- as.integer(myH1DataObject0[["IS"]]$SLid)
  myH1DataObject0[["IS"]]$IScommTaxon <- as.integer(myH1DataObject0[["IS"]]$IScommTaxon)
  myH1DataObject0[["IS"]]$ISsppCode <- as.integer(myH1DataObject0[["IS"]]$ISsppCode)
  # add an additional species list - could be many in the SL download
  extraSL <- myH1DataObject[["SL"]][1,]
  extraSL$SLid <- as.integer(max(myH1DataObject[["SL"]]$SLid) +1)
  myH1DataObject0[["SL"]]<-rbind(myH1DataObject0[["SL"]], extraSL)

  # ensure key is set on SL and IS
  setkey(myH1DataObject0[["SL"]], SLid)
  setkey(myH1DataObject0[["IS"]], ISid)

  myH1DataObject0[["SS"]]<-rbind(myH1DataObject0[["SS"]][1,],myH1DataObject0[["SS"]][1,])
  myH1DataObject0[["SS"]]$SScatchFra[2]<-"Dis"
  myH1DataObject0[["SS"]]$SSid[2]<-myH1DataObject0[["SS"]]$SSid[1]+1
  myH1DataObject0[["SS"]]$SLid[2]<-as.integer(31831)
  myH1DataObject0[["SS"]]$SSid<-as.integer(myH1DataObject0[["SS"]]$SSid)
  myH1DataObject0[["SS"]]$SSuseCalcZero<-'Y'
  # ensure key is set on SS
  setkey(myH1DataObject0[["SS"]], SSid)


  myH1DataObject1 <- filterRDBESDataObject(myH1DataObject0, c("SAid"), c(572813), killOrphans = TRUE)
  validateRDBESDataObject(myH1DataObject0, checkDataTypes = TRUE)
  validateRDBESDataObject(myH1DataObject1, checkDataTypes = TRUE)

  myH1DataObject1

}

test_that("generateZerosUsingSL creates rows for SLcou*SLinst*SLspeclistName*SLyear*SLcatchFrac*SLcommTaxon", {

    myH1DataObject1 <- createTestData()

	  # check generateZerosUsingSL is creating missing rows in SA
	  # generateZerosUsingSL should:
	  #   a) create 1 extra row in SA for the SS row that does not have a child SA row
	  #   b) create 1 extra rows in SA for the SL/IS rows that do not match an SA row
	  expectSARowsFromSS <- 1
	  expectSARowsFromSL <- 1

		myTest <- generateZerosUsingSL(myH1DataObject1, verbose = TRUE)

	  # create aux id_table [Nuno's function] and tmpKey to use in test
		#aux<-createTableOfRDBESIds(x = myTest3, addSAseqNums=FALSE)

  	# Check we have the correct number of SA rows
	  expect_equal(nrow(myTest$SA),  nrow(myH1DataObject1$SA)+expectSARowsFromSS+expectSARowsFromSL)
	  # Check all species in species list are now present in SA (that won't always be the case but should be here)
	  tempSL <- merge(myTest[["SL"]], myTest[["IS"]],  by = "SLid")
	  expect_equal(all(tempSL$IScommTaxon %in% myTest[["SA"]]$SAspeCode),TRUE)

})

test_that("Adds 1 more species to SL to test (SSuseCalcZero = Y)", {

  myH1DataObject1 <- createTestData()

  # adds 1 more species to IS to test
  df1IS <- data.frame('31833','31831','IS','126437','126437')
  data.table::setnames(df1IS, names(myH1DataObject1[["IS"]]))
  myH1DataObject1[["IS"]] <- rbind(myH1DataObject1[["IS"]],df1IS)
  myH1DataObject1[["IS"]]$ISid <- as.integer(myH1DataObject1[["IS"]]$ISid)
  myH1DataObject1[["IS"]]$SLid <- as.integer(myH1DataObject1[["IS"]]$SLid)
  myH1DataObject1[["IS"]]$IScommTaxon <- as.integer(myH1DataObject1[["IS"]]$IScommTaxon)
  myH1DataObject1[["IS"]]$ISsppCode <- as.integer(myH1DataObject1[["IS"]]$ISsppCode)
  setkey(myH1DataObject1[["IS"]], ISid)

  # check generateZerosUsingSL is creating missing rows in SA
  # generateZerosUsingSL should:
  #   a) create 1 extra row in SA for the SS row that does not have a child SA row
  #   b) create 2 extra rows in SA for the SL/IS rows that do not match an SA row
  expectSARowsFromSS <- 2
  expectSARowsFromSL <- 1

  myTest <- generateZerosUsingSL(myH1DataObject1)

  # Check we have the correct number of SA rows
  expect_equal(nrow(myTest$SA),  nrow(myH1DataObject1$SA)+expectSARowsFromSS+expectSARowsFromSL)
  # Check all species in species list are now present in SA (that won't always be the case but should be here)
  tempSL <- merge(myTest[["SL"]], myTest[["IS"]],  by = "SLid")
  expect_equal(all(tempSL$IScommTaxon %in% myTest[["SA"]]$SAspeCode),TRUE)


})

test_that("Check that when SSuseCalcZero = N no rows are added)", {

  myH1DataObject1 <- createTestData()
  myH1DataObject1[["SS"]]$SSuseCalcZero <- 'N'

  # check generateZerosUsingSL is not creating rows in SA
  # generateZerosUsingSL should not create any new rows because SSuseCalcZero = N
  expectSARowsFromSS <- 0
  expectSARowsFromSL <- 0

  myTest <- generateZerosUsingSL(myH1DataObject1)

  # Check we have the correct number of SA rows
  expect_equal(nrow(myTest$SA),  nrow(myH1DataObject1$SA)+expectSARowsFromSS+expectSARowsFromSL)


})

test_that("Species already there - no rows are added", {

  #species*catchFrac in SL and in SA: expected behavior -> do not generate a 0 row in SA
  myH1DataObject1 <- createTestData()

  # Get rid of the SS rows that don't have SA rows linked to them
  ssIDToKeep <- myH1DataObject1[["SA"]]$SSid
  myH1DataObject1[["SS"]] <- myH1DataObject1[["SS"]][myH1DataObject1[["SS"]]$SSid == ssIDToKeep,]
  myH1DataObject1[["SS"]]$SScatchFra <- 'Dis'

  # Get rid of the IS rows that contain species not in SA
  spToKeep <- myH1DataObject1[["SA"]]$SAspeCode
  myH1DataObject1[["IS"]] <- myH1DataObject1[["IS"]][myH1DataObject1[["IS"]]$IScommTaxon == spToKeep,]

  # Ensure the catch fractions are the same
  myH1DataObject1[["SA"]]$SAcatchCat <- 'Dis'
  myH1DataObject1[["SL"]]$SLcatchFrac <- 'Dis'


  # check generateZerosUsingSL is not creating rows in SA
  # generateZerosUsingSL should:
  #   a) create 0 extra row in SAs bcause there are no SS rows that do not have a child SA row
  #   b) create 0 extra rows in SA because all SL/IS rows match an SA row
  expectSARowsFromSS <- 0
  expectSARowsFromSL <- 0

  myTest <- generateZerosUsingSL(myH1DataObject1)

  # Check we have the correct number of SA rows
  expect_equal(nrow(myTest$SA),  nrow(myH1DataObject1$SA)+expectSARowsFromSS+expectSARowsFromSL)

})

test_that("SScatchFra=='Catch' and spp present", {

  # if SScatchFra=="Catch" it should not generate 0s for any SA strata if spp present in list

  #species*catchFrac in SL and in SA: expected behavior -> do not generate a 0 row in SA
  myH1DataObject1 <- createTestData()

  # Get rid of the SS rows that don't have SA rows linked to them, and make the Catch rows
  ssIDToKeep <- myH1DataObject1[["SA"]]$SSid
  myH1DataObject1[["SS"]] <- myH1DataObject1[["SS"]][myH1DataObject1[["SS"]]$SSid == ssIDToKeep,]
  myH1DataObject1[["SS"]]$SScatchFra <- 'Catch'

  # Make SL relate to Catch
  myH1DataObject1[["SL"]]$SLcatchFrac<-"Catch"

  # Get rid of the IS rows that contain species not in SA
  spToKeep <- myH1DataObject1[["SA"]]$SAspeCode
  myH1DataObject1[["IS"]] <- myH1DataObject1[["IS"]][myH1DataObject1[["IS"]]$IScommTaxon == spToKeep,]

  # Duplicate the SA row - we want 1 row of Lan and 1 row of Dis
  tmpSA <- myH1DataObject1[["SA"]][1,]
  tmpSA$SAid <- tmpSA$SAid + 1
  myH1DataObject1[["SA"]] <- rbind(myH1DataObject1[["SA"]], tmpSA)
  myH1DataObject1[["SA"]]$SAstratification<-"Y"
  myH1DataObject1[["SA"]]$SAstratumName<-c("Landings","Discards")
  myH1DataObject1[["SA"]]$SAcatchCat<-c("Lan","Dis")	# this is just cosmetics
  setkey(myH1DataObject1[["SA"]], "SAid")

  # check generateZerosUsingSL is not creating rows in SA
  # generateZerosUsingSL should:
  #   a) create 0 extra row in SAs bcause there are no SS rows that do not have a child SA row
  #   b) create 0 extra rows in SA because all SL/IS rows match an SA row
  expectSARowsFromSS <- 0
  expectSARowsFromSL <- 0

  myTest <- generateZerosUsingSL(myH1DataObject1)

  # Check we have the correct number of SA rows
  expect_equal(nrow(myTest$SA),  nrow(myH1DataObject1$SA)+expectSARowsFromSS+expectSARowsFromSL)

})

test_that("SScatchFra=='Catch' and spp absent", {

  # if SScatchFra=="Catch" it should generate 0s for all SA strata present if spp absent(but in SL)

  myH1DataObject1 <- createTestData()

  # Get rid of the SS rows that don't have SA rows linked to them, and make the Catch rows
  ssIDToKeep <- myH1DataObject1[["SA"]]$SSid
  myH1DataObject1[["SS"]] <- myH1DataObject1[["SS"]][myH1DataObject1[["SS"]]$SSid == ssIDToKeep,]
  myH1DataObject1[["SS"]]$SScatchFra <- 'Catch'

  # Make SL relate to Catch
  myH1DataObject1[["SL"]]$SLcatchFrac<-"Catch"

  # Get rid of the IS rows that contain species not in SA
  spToKeep <- myH1DataObject1[["SA"]]$SAspeCode
  myH1DataObject1[["IS"]] <- myH1DataObject1[["IS"]][myH1DataObject1[["IS"]]$IScommTaxon == spToKeep,]
  # Make sure the SLid in IS is the one referred to by the SS data
  myH1DataObject1[["IS"]]$SLid <- min(myH1DataObject1[["SS"]]$SLid)
  # Add a species that is not in SA
  tmpIS <- myH1DataObject1[["IS"]]
  tmpIS$ISid <- tmpIS$ISid +1
  tmpIS$IScommTaxon <- 127007
  tmpIS$ISsppCode <- 127007
  myH1DataObject1[["IS"]] <- rbind(myH1DataObject1[["IS"]],tmpIS)
  setkey(myH1DataObject1[["IS"]], "ISid")

  # Duplicate the SA row - we want 1 row of Lan and 1 row of Dis
  tmpSA <- myH1DataObject1[["SA"]][1,]
  tmpSA$SAid <- tmpSA$SAid + 1
  myH1DataObject1[["SA"]] <- rbind(myH1DataObject1[["SA"]], tmpSA)
  myH1DataObject1[["SA"]]$SAstratification<-"Y"
  myH1DataObject1[["SA"]]$SAstratumName<-c("Landings","Discards")
  myH1DataObject1[["SA"]]$SAcatchCat<-c("Lan","Dis")	# this is just cosmetics
  setkey(myH1DataObject1[["SA"]], "SAid")

  # check generateZerosUsingSL is not creating rows in SA
  # generateZerosUsingSL should:
  #   a) create 0 extra row in SAs bcause there are no SS rows that do not have a child SA row
  #   b) create 2 extra rows in SA because we have 1 "catch" species in SL/IS row that does not match an SA row
  expectSARowsFromSS <- 0
  expectSARowsFromSL <- 2

  myTest <- generateZerosUsingSL(myH1DataObject1)

  # Check we have the correct number of SA rows
  expect_equal(nrow(myTest$SA),  nrow(myH1DataObject1$SA)+expectSARowsFromSS+expectSARowsFromSL)

})

test_that("Produces error when there are multiple values of SAsex per SSid*SAstratumName", {

  # issue error if >1 SAcatchCat OR SAsex OR SAlandCat in same SSid*SAstratumName
  # explanation:
  # in a normal situation these variables are filled in the new from the 1st row of SSid*SAstratumName
  # if it happens that there are >1 values in that SSid*SAstratumName this is not valid - an error is issued

  myH1DataObject1 <- createTestData()

  # Make the SA rows stratified
  myH1DataObject1[["SA"]]$SAstratification <- "Y"
  myH1DataObject1[["SA"]]$SAstratumName <- "S1"
  myH1DataObject1[["SA"]]$SAsex <- "M"
  # Duplicate the SA row and change the sex
  tmpSA <- myH1DataObject1[["SA"]][1,]
  tmpSA$SAid <- tmpSA$SAid + 1
  tmpSA$SAsex <- "F"
  myH1DataObject1[["SA"]] <- rbind(myH1DataObject1[["SA"]], tmpSA)
  setkey(myH1DataObject1[["SA"]], "SAid")

  expect_error(generateZerosUsingSL(myH1DataObject1),
               "Cannot generateZerosUsingSL*")

})

test_that("Produces error when there are multiple values of SAcatchCat per SSid*SAstratumName", {

  # issue error if >1 SAcatchCat OR SAsex OR SAlandCat in same SSid*SAstratumName
  # explanation:
  # in a normal situation these variables are filled in the new from the 1st row of SSid*SAstratumName
  # if it happens that there are >1 values in that SSid*SAstratumName this is not valid - an error is issued

  myH1DataObject1 <- createTestData()

  # Make the SA rows stratified
  myH1DataObject1[["SA"]]$SAstratification <- "Y"
  myH1DataObject1[["SA"]]$SAstratumName <- "S1"
  myH1DataObject1[["SA"]]$SAcatchCat <- "Lan"
  # Duplicate the SA row and change the sex
  tmpSA <- myH1DataObject1[["SA"]][1,]
  tmpSA$SAid <- tmpSA$SAid + 1
  tmpSA$SAcatchCat <- "Dis"
  myH1DataObject1[["SA"]] <- rbind(myH1DataObject1[["SA"]], tmpSA)
  setkey(myH1DataObject1[["SA"]], "SAid")

  expect_error(generateZerosUsingSL(myH1DataObject1),
               "Cannot generateZerosUsingSL*")

})

test_that("Produces error when there are multiple values of SAlandCat per SSid*SAstratumName", {

  # issue error if >1 SAcatchCat OR SAsex OR SAlandCat in same SSid*SAstratumName
  # explanation:
  # in a normal situation these variables are filled in the new from the 1st row of SSid*SAstratumName
  # if it happens that there are >1 values in that SSid*SAstratumName this is not valid - an error is issued

  myH1DataObject1 <- createTestData()

  # Make the SA rows stratified
  myH1DataObject1[["SA"]]$SAstratification <- "Y"
  myH1DataObject1[["SA"]]$SAstratumName <- "S1"
  myH1DataObject1[["SA"]]$SAlandCat <- "Ind"
  # Duplicate the SA row and change the sex
  tmpSA <- myH1DataObject1[["SA"]][1,]
  tmpSA$SAid <- tmpSA$SAid + 1
  tmpSA$SAlandCat <- "HuC"
  myH1DataObject1[["SA"]] <- rbind(myH1DataObject1[["SA"]], tmpSA)
  setkey(myH1DataObject1[["SA"]], "SAid")

  expect_error(generateZerosUsingSL(myH1DataObject1),
               "Cannot generateZerosUsingSL*")

})


}) ## end capture.output


