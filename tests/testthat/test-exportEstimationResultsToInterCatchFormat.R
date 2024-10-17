#capture.output({  ## suppresses printing of console output when running test()

  # H1 directory
  dirH1 <- "./h1_v_1_19_26/"
  myCountry <- "ZW"
  myYear <- 1965
  myCatchFraction <- "Lan"
  mySpecies <- 1019159

  generateTestData <- function(){

    ## Step 1) load and prepare some test data
    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = dirH1)

    # Edit our data so that we have SRSWOR on each level and calculate the probs
    myH1RawObject[["VS"]]$VSselectMeth <- "SRSWOR"
    myH1RawObject[["VS"]]$VSincProb <- myH1RawObject[["VS"]]$VSnumSamp / myH1RawObject[["VS"]]$VSnumTotal
    myH1RawObject[["VS"]]$VSselProb <- 1/myH1RawObject[["VS"]]$VSnumTotal
    myH1RawObject[["FT"]]$FTselectMeth <- "SRSWOR"
    myH1RawObject[["FT"]]$FTincProb <- myH1RawObject[["FT"]]$FTnumSamp / myH1RawObject[["FT"]]$FTnumTotal
    myH1RawObject[["FT"]]$FTselProb <- 1/myH1RawObject[["FT"]]$FTnumTotal
    myH1RawObject[["FO"]]$FOselectMeth <- "SRSWOR"
    myH1RawObject[["FO"]]$FOincProb <- myH1RawObject[["FO"]]$FOnumSamp / myH1RawObject[["FO"]]$FOnumTotal
    myH1RawObject[["FO"]]$FOselProb <- 1/myH1RawObject[["FO"]]$FOnumTotal
    myH1RawObject[["SS"]]$SSselectMeth <- "SRSWOR"
    myH1RawObject[["SS"]]$SSincProb <- myH1RawObject[["SS"]]$SSnumSamp / myH1RawObject[["SS"]]$SSnumTotal
    myH1RawObject[["SS"]]$SSselProb <- 1/myH1RawObject[["SS"]]$SSnumTotal
    myH1RawObject[["SA"]]$SAselectMeth <- "SRSWOR"
    myH1RawObject[["SA"]]$SAincProb <- myH1RawObject[["SA"]]$SAnumSamp / myH1RawObject[["SA"]]$SAnumTotal
    myH1RawObject[["SA"]]$SAselProb <- 1/myH1RawObject[["SA"]]$SAnumTotal

    # Update our test data with some random sample measurements (it didn't include these)
    # set the random seed
    set.seed(1234)
    myH1RawObject[['SA']]$SAsampWtLive <- round(runif(n=nrow(myH1RawObject[['SA']]),min = 1, max = 100))
    myH1RawObject[['SA']]$SAsampWtMes <- round(runif(n=nrow(myH1RawObject[['SA']]),min = 1, max = 100))

    #Filter our data for WGRDBES-EST TEST 1, 1965, H1
    myFields <- c("DEyear","SDctry","DEhierarchy","DEsampScheme","DEstratumName","SAspeCode","SScatchFra")
    myValues <- c(myYear,myCountry,1,"National Routine","DE_stratum1_H1",mySpecies,myCatchFraction)
    myH1RawObject <- filterRDBESDataObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )
    myH1RawObject <- findAndKillOrphans(myH1RawObject)
    myH1RawObject

  }

test_that("exportEstimationResultsToInterCatchFormat runs without errors or warnings",  {

      # get some test data
      myH1RawObject <- generateTestData()

      ## Create an estimation object, but stop at SA

      myTestData <- createRDBESEstObject(myH1RawObject, 1, stopTable = "SA")
      # Get rid of rows that don't have an SA row
      myTestData <- myTestData[!is.na(myTestData$SAid),]
      # Estimate using the data
      myStrataResults <- doEstimationForAllStrata(myTestData, "SAsampWtLive")
      myStrataResults

      # Get our estimated values for the PSU
      psuEstimates <- myStrataResults[myStrataResults$recType == "VS",]

      # This is the data we will export to IC format
      dataToOutput <- data.frame(Country = myCountry,
                                 Year = myYear,
                                 SeasonType = NA,
                                 Season = NA,
                                 Fleet= NA,
                                 AreaType = "Stratum",
                                 FishingArea = psuEstimates$stratumName,
                                 Species = mySpecies ,
                                 Stock = mySpecies,
                                 CatchCategory = substr(myCatchFraction, 1, 1),
                                 ReportingCategory = "A",
                                 Usage = "H",
                                 SamplesOrigin = "O",
                                 UnitCATON = "kg",
                                 CATON = psuEstimates$est.total/1000.0,
                                 varCATON = psuEstimates$var.total/1000000.0)


      expect_error(exportEstimationResultsToInterCatchFormat(dataToOutput),NA )
      expect_warning(exportEstimationResultsToInterCatchFormat(dataToOutput),NA )

      icOutput <- exportEstimationResultsToInterCatchFormat(dataToOutput)
      expect_equal(length(icOutput), 6)
      expect_equal(icOutput[1], "HI,ZW,1965,NA,NA,NA,Stratum,VS_stratum1,NA,NA,-9,NA")
      expect_equal(icOutput[2], "SI,ZW,1965,NA,NA,NA,Stratum,VS_stratum1,NA,1019159,1019159,L,A,NA,H,O,NA,kg, 3342.222,-9, 1053266,NA,NA,NA")

  })


#}) ## end capture.output
