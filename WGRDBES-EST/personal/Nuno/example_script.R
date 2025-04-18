# check R version
installr::check.for.updates.R(notify_user = TRUE, GUI = TRUE, 
	page_with_download_url = "https://cran.rstudio.com/bin/windows/base/", pat = "R-[0-9.]+.+-win\\.exe")

# load data	
	# info: object myH1RawObject in file "testData.Rdata" is an 
		# anonymized extract of RDBES data after filterRDBESDataObject & findAndKillOrphans
	load ("data/testData.Rdata")

# load new or modified functions [they are exemplified below]
	source("R/removePrefixFromVarNames.R") # clean the table prefixes of variable names [makes it easier to handle in other functions]
	source("R/runChecksOnSelectionAndProbs.R") # checks data for some issues related to selection and probabilities, including some features not yet developed [stops need to corrected before probabilities are calculated]
	source("R/generateProbs.r") # modified/improved version of generateProbs developed during WKRDB-EST 1 and WKRDB-EST 2 [several new functionalities, warnings, etc]
	source("R/applyGenerateProbs.r") # wrapper to "runChecksOnSelectionAndProbs" and "generateProbs"
	
# clean the prefixes [makes it easier to handle in other functions]
	myH7RawObject <- removePrefixFromVarNames(myH7RawObject)

# checks data for some issues related to selection and probabilities
	# note: this function is only exemplified here - in general only applyGenerateProbs will be used as the wrapper also includes this function
	runChecksOnSelectionAndProbs(myH7RawObject, printStopIssue = FALSE)

# CHANGES TO DATA
	# changes to selection methods
	myH7RawObject$OS$selectMeth<-"SRSWOR"
	runChecksOnSelectionAndProbs(myH7RawObject)

	myH7RawObject$SA$stratification<-"N"
	runChecksOnSelectionAndProbs(myH7RawObject)

	myH7RawObject$SA$selectMeth<-"SRSWOR"
	runChecksOnSelectionAndProbs(myH7RawObject)

	myH7RawObject$BV$selectMeth<-"SRSWOR"
	runChecksOnSelectionAndProbs(myH7RawObject)

		# note: by now main issues relation to selection methods have been corrected (and documented)

	# still probabilities cannot be calculated because numTotal are missing
	myH7PrepObject<-applyGenerateProbs (x = myH7RawObject, probType = "both", overwrite=T, runInitialProbChecks = FALSE)

	# changes to numTotal
	myH7RawObject$OS$numTotal<-c(3,3,3)
	myH7RawObject$SS$numTotal<-c(1,1,1)
	myH7RawObject$SA$numTotal<-c(20, 10, 5)
	myH7RawObject$BV$numTotal<-c(rep(50, 29*5), rep(200, 95*5), rep(200, 39*5))

# Probability generation
	myH7PrepObject<-applyGenerateProbs (x = myH7RawObject, probType = "both", overwrite=T, runInitialProbChecks = TRUE)
	myH7PrepObject
	
# generation of zeros using SL
	source("R/generateZerosUsingSL.r") 
	
	myH7PrepObject2<-generateZerosUsingSL(myH7PrepObject)
	
	# note the solution [dec SAid added to avoid integer that could duplciate other already existing]
	myH7PrepObject2$SA


# other
	# check why [is it table var prefixes? do we need them back? create a addPrefixToVarNames if so]
		RDBEScore::validateRDBESDataObject(myH7PrepObject2, verbose = TRUE)
		RDBEScore::validateRDBESDataObject(myH7PrepObject, verbose = FALSE)

	

	
	
