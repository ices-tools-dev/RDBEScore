capture.output({  ## suppresses printing of console output when running test()

# download and subset original data

	myH1DataObject <- RDBEScore::createRDBESDataObject("./h1_v_20250211/ZW_1965_WGRDBES-EST_TEST_1")
	#myH1DataObject <- RDBEScore::createRDBESDataObject("./tests/testthat/h1_v_20250211/ZW_1965_WGRDBES-EST_TEST_1")

	# Subset data
		myH1DataObject <- filterRDBESDataObject(myH1DataObject,c("DEstratumName","SLspeclistName"),
				c("Pckg_survey_apistrat_H1","WGRDBES-EST_TEST_1_Pckg_survey_apistrat_H1"),
					killOrphans=TRUE, strict=TRUE)

		#myH1DataObject[["SL"]]
		#myH1DataObject[["SS"]]

	# adds a species to IS
	#rowToAdd <- data.frame('31831','SL','ZW','4484',myH1DataObject[["SL"]]$SLspeclistName,'1965','Dis','107254','107254')
	rowToAdd <- data.frame('1099',myH1DataObject[["SL"]]$SLid,'IS','107254','107254')
	#colnames(rowToAdd) <- names(myH1DataObject[["SL"]])
	colnames(rowToAdd) <- names(myH1DataObject[["IS"]])

	# myH1DataObject[["SL"]] <- rbind(myH1DataObject[["SL"]],rowToAdd)
	# myH1DataObject[["SL"]]$SLid <- as.integer(myH1DataObject[["SL"]]$SLid)
	# myH1DataObject[["SL"]]$SLyear <- as.integer(myH1DataObject[["SL"]]$SLyear)
	# myH1DataObject[["SL"]]$SLcommTaxon <- as.integer(myH1DataObject[["SL"]]$SLcommTaxon)
	# myH1DataObject[["SL"]]$SLsppCode <- as.integer(myH1DataObject[["SL"]]$SLsppCode)
	# ensure key is set on IS
	#setkey(myH1DataObject[["SL"]], SLid)
	setkey(myH1DataObject[["IS"]], ISid)

	# adds a row to SS
	myH1DataObject[["SS"]]<-rbind(myH1DataObject[["SS"]][1,],myH1DataObject[["SS"]][1,])
	myH1DataObject[["SS"]]$SScatchFra[2]<-"Dis"
	myH1DataObject[["SS"]]$SSid[2]<-myH1DataObject[["SS"]]$SSid[1]+1
	myH1DataObject[["SS"]]$SSid<-as.integer(myH1DataObject[["SS"]]$SSid)
	# ensure key is set on SS
	setkey(myH1DataObject[["SS"]], SSid)
	setkey(myH1DataObject[["SA"]], SAid)

	validateRDBESDataObject(myH1DataObject, checkDataTypes = TRUE)

# prepare myH1DataObject1: test data 1 species

	myH1DataObject1 <- filterRDBESDataObject(myH1DataObject, c("SSid"), c(227694),
		killOrphans = TRUE, strict=TRUE)

	setkey(myH1DataObject1[["SS"]], SSid)
	setkey(myH1DataObject1[["SA"]], SAid)

	validateRDBESDataObject(myH1DataObject1, checkDataTypes = TRUE)

# prepare myH1DataObject2: test data >1 species
	myH1DataObject2 <- myH1DataObject1
	#myH1DataObject2$SL<-rbind(myH1DataObject2$SL,myH1DataObject2$SL)
	myH1DataObject2$IS<-rbind(myH1DataObject2$IS,myH1DataObject2$IS)
	#myH1DataObject2$SL[,c("SLcommTaxon","SLsppCode")]<-as.integer(c(107254, 107253))
	myH1DataObject2$IS[,c("IScommTaxon","ISsppCode")]<-as.integer(c(107254, 107253))
	#myH1DataObject2$SL$SLid[2]<-as.integer(47892)
	myH1DataObject2$IS$ISid[2]<-as.integer(47892)

	myH1DataObject2$SA<-rbind(myH1DataObject2$SA,myH1DataObject2$SA)
	myH1DataObject2$SA$SAspeCode[2] <- "107253"
	myH1DataObject2$SA$SAid[2] <- as.integer(572814)

	#setkey(myH1DataObject2[["SL"]], SLid)
	setkey(myH1DataObject2[["IS"]], ISid)
	setkey(myH1DataObject2[["SS"]], SSid)
	setkey(myH1DataObject2[["SA"]], SAid)

	validateRDBESDataObject(myH1DataObject2, checkDataTypes = TRUE)

# object demo
	myH1DataObject[c("SL","SS","SA")]
	myH1DataObject1[c("SL","SS","SA")]
	myH1DataObject2[c("SL","SS","SA")]


# ------------------
# tests: 1 spp call
# ------------------

  test_that("simpleSA: generateNAsUsingSL does not add any NA rows if none are missing (1 targetAphiaId, SS present)", {

		expect_equal(myH1DataObject1,generateNAsUsingSL(myH1DataObject1, targetAphiaId = c("107254")))

  })

  test_that("simpleSA: generateNAsUsingSL adds one NA row if spp not in list (case: 1 targetAphiaId, SS present)", {

		# expect 1 row to be added
		expect_equal(nrow(generateNAsUsingSL(myH1DataObject1, targetAphiaId = c("107253"))$SA),2)
		# expect 1 spp ("107253") in the 2nd row
		expect_equal(generateNAsUsingSL(myH1DataObject1, targetAphiaId = c("107253"))$SA$SAspeCode,c("107254","107253"))
		# expect SAtotalWtMes of 2nd row to be NA
		expect_equal(generateNAsUsingSL(myH1DataObject1, targetAphiaId = c("107253"))$SA$SAtotalWtMes[2],as.integer(NA))
		# expect SAsampWtMes of 2nd row to be NA
		expect_equal(generateNAsUsingSL(myH1DataObject1, targetAphiaId = c("107253"))$SA$SAsampWtMes[2],as.integer(NA))

  })

  test_that("simpleSA: generateNAsUsingSL makes spp weights NA if spp not in list and overwriteSampled = TRUE [default] (case: 1 targetAphiaId, SS present)", {

		myH1DataObject11<-myH1DataObject1
		myH1DataObject11$SL$SLid<-1; setkey(myH1DataObject11$SL, "SLid")
			# check: should yield TRUE
				!myH1DataObject11$SS$SLid %in% myH1DataObject11$SL$SLid
		myH1DataObject11[c("SL","SS","SA")]

		# expect 0 row to be added
		expect_equal(nrow(generateNAsUsingSL(myH1DataObject11, targetAphiaId = c("107254"))$SA),1)
		# expect SAtotalWtMes & SAsampWtMes of spp to be NA
		expect_equal(generateNAsUsingSL(myH1DataObject11, targetAphiaId = c("107254"))$SA$SAtotalWtMes,as.integer(NA))
		expect_equal(generateNAsUsingSL(myH1DataObject11, targetAphiaId = c("107254"))$SA$SAsampWtMes,as.integer(NA))
		# expect all other vars to have remained unchanged
		expect_equal(apply(generateNAsUsingSL(myH1DataObject11, targetAphiaId = c("107254"))$SA[,!c("SAtotalWtMes","SAsampWtMes")],1, paste0, collapse=""),apply(myH1DataObject11$SA[,!c("SAtotalWtMes","SAsampWtMes")],1, paste0, collapse=""))

  })

  test_that("simpleSA: generateNAsUsingSL does makes spp weights NA if spp not in list and overwriteSampled = FALSE (case: 1 targetAphiaId, SS present)", {

		myH1DataObject11<-myH1DataObject1
		myH1DataObject11$SL$SLid<-1; setkey(myH1DataObject11$SL, "SLid")
			# check: should yield TRUE
				!myH1DataObject11$SS$SLid %in% myH1DataObject11$SL$SLid
		myH1DataObject11[c("SL","SS","SA")]

		# expect 0 row to be added
		expect_equal(nrow(generateNAsUsingSL(myH1DataObject11, targetAphiaId = c("107254"), overwriteSampled=FALSE)$SA),1)
		# expect all vars to have remained unchanged
		expect_equal(apply(generateNAsUsingSL(myH1DataObject11, targetAphiaId = c("107254"), overwriteSampled=FALSE)$SA,1, paste0, collapse=""),apply(myH1DataObject11$SA,1, paste0, collapse=""))

  })

# ------------------
# tests: >1 spp call
# ------------------

  test_that("simpleSA: generateNAsUsingSL does not add any NA rows or change data if none are missing (2 targetAphiaId, SS present)", {



		# expect 0 row to be added
		expect_equal(generateNAsUsingSL(myH1DataObject2, targetAphiaId =  c("107254", "107253")), myH1DataObject2)

})


  test_that("simpleSA: generateNAsUsingSL adds an NA row if one of target_spp (spp2) not in SL (2 targetAphiaId, SS present)", {

		# prepare test data
			myH1DataObject21 <- myH1DataObject2
			myH1DataObject21$SL <- myH1DataObject21$SL[1,]
			myH1DataObject21$SA <- myH1DataObject21$SA[1,]
			validateRDBESDataObject(myH1DataObject21, checkDataTypes = TRUE)

		myH1DataObject21[c("SL","SS","SA")]

		# expect 0 row to be added
		expect_equal(nrow(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107254", "107253"))$SA),2)
		# expect both spp to be present in result
		expect_equal(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107254", "107253"))$SA$SAspeCode,c("107254", "107253"))
		# expect all columns in spp1 to remain the same
		expect_equal(apply(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107254", "107253"))$SA[1,],1,paste0, collapse=""),apply(myH1DataObject2$SA[1,],1, paste0, collapse=""))
		# expect SAtotalWtMes of spp2 (not in list) to be NA
		expect_equal(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107254", "107253"))$SA$SAtotalWtMes[2],as.integer(NA))
		# expect SAsampWtMes of spp2 (not in list) to be NA
		expect_equal(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107254", "107253"))$SA$SAsampWtMes[2],as.integer(NA))
})


  test_that("simpleSA: generateNAsUsingSL makes spp weights NA in spp not in list if overwriteSampled = TRUE [default] (case: 2 targetAphiaId, SS present)", {

		# prepare test data
			myH1DataObject21 <- myH1DataObject2
			myH1DataObject21$SL <- myH1DataObject21$SL[1,]
			validateRDBESDataObject(myH1DataObject21, checkDataTypes = TRUE)

		myH1DataObject21[c("SL","SS","SA")]

		# expect 0 row to be added
		expect_equal(nrow(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"))$SA),2)
		# expect SAtotalWtMes & SAsampWtMes of spp1 (in list) to remain the same
		expect_equal(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"))$SA$SAtotalWtMes[1],myH1DataObject21$SA$SAtotalWtMes[1])
		expect_equal(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"))$SA$SAsampWtMes[1],myH1DataObject21$SA$SAsampWtMes[1])
		# expect SAtotalWtMes & SAsampWtMes of spp2 (not in list) to be set to NA (because overwriteSampled== TRUE by default)
		expect_equal(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"))$SA$SAtotalWtMes[2],as.integer(NA))
		expect_equal(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"))$SA$SAsampWtMes[2],as.integer(NA))
		# expect all other vars to have remained unchanged
		expect_equal(apply(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"))$SA[,!c("SAtotalWtMes","SAsampWtMes")],1, paste0, collapse=""),apply(myH1DataObject21$SA[,!c("SAtotalWtMes","SAsampWtMes")],1, paste0, collapse=""))

  })

  test_that("simpleSA: generateNAsUsingSL does not makes spp weights NA in spp not in list if overwriteSampled = FALSE (case: 2 targetAphiaId, SS present)", {

		# prepare test data
			myH1DataObject21 <- myH1DataObject2
			myH1DataObject21$SL <- myH1DataObject21$SL[1,]
			validateRDBESDataObject(myH1DataObject21, checkDataTypes = TRUE)

		myH1DataObject21[c("SL","SS","SA")]

		# expect 0 row to be added
		expect_equal(nrow(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"), overwriteSampled=FALSE)$SA),2)
		# expect SAtotalWtMes & SAsampWtMes of spp1 (in list) to remain the same
		expect_equal(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"), overwriteSampled=FALSE)$SA$SAtotalWtMes[1],myH1DataObject21$SA$SAtotalWtMes[1])
		expect_equal(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"), overwriteSampled=FALSE)$SA$SAsampWtMes[1],myH1DataObject21$SA$SAsampWtMes[1])
		# expect SAtotalWtMes & SAsampWtMes of spp2 (not in list) to remain the same (because overwriteSampled== FALSE)
		expect_equal(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"), overwriteSampled=FALSE)$SA$SAtotalWtMes[2],myH1DataObject21$SA$SAtotalWtMes[2])
		expect_equal(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"), overwriteSampled=FALSE)$SA$SAsampWtMes[2],myH1DataObject21$SA$SAsampWtMes[2])
		# expect all other vars to have remained unchanged
		expect_equal(apply(generateNAsUsingSL(myH1DataObject21, targetAphiaId = c("107253"), overwriteSampled=FALSE)$SA[,!c("SAtotalWtMes","SAsampWtMes")],1, paste0, collapse=""),apply(myH1DataObject21$SA[,!c("SAtotalWtMes","SAsampWtMes")],1, paste0, collapse=""))

  })

}) ## end capture.output


