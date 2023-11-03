capture.output({  ## suppresses printing of console output when running test()

  generateTestData <- function(tableToStop){

    ## Step 1) load and prepare some test data

    myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")

    #Filter our data for WGRDBES-EST TEST 1, 1965, H1
    myValues <- c(1965,1,"National Routine","DE_stratum1_H1",1019159)
    myFields <- c("DEyear","DEhierarchy","DEsampScheme","DEstratumName","SAspeCode")

    myH1RawObject <- filterRDBESDataObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )
    myH1RawObject <- findAndKillOrphans(myH1RawObject)

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

    ## Step 2) Create an estimation object, but stop at tableToStop
    myH1EstObj <- createRDBESEstObject(myH1RawObject, 1, stopTable = tableToStop)
    # Get rid of rows that don't have an SA row
    myH1EstObj <- myH1EstObj[!is.na(myH1EstObj$SAid),]

    myH1EstObj

  }

test_that("doEstimationForAllStrata creates correct est.total for SAsampWtLive using H1",  {

      # get some SA test data
      myTestData <- generateTestData("SA")

      # Edit our data so that we have SRSWOR on each level
      myTestData$su1selectMeth <- "SRSWOR"
      myTestData$su2selectMeth <- "SRSWOR"
      myTestData$su3selectMeth <- "SRSWOR"
      myTestData$su4selectMeth <- "SRSWOR"
      myTestData$su5selectMeth <- "SRSWOR"

      # Choose what we will estimate for
      targetValue <- "SAsampWtLive"

      # Get a point estimate for comparison
      x <- myTestData
      x$studyVariable <- x[,..targetValue]
      targetProbColumns <- names(x)[grep("^.*incProb$", names(x))]
      x$totalIncProb <- apply(x[, ..targetProbColumns], 1, prod)
      x$pointEstimate <- x$studyVariable / x$totalIncProb
      # Total estimate - add up all point estimates
      expectedTotal <- sum(x$pointEstimate , na.rm = TRUE)

      # Check the new function works - and the estimated total is the same
      myStrataResults <- doEstimationForAllStrata(myTestData, targetValue)
      actualTotal <- myStrataResults[myStrataResults$recType == "DE","est.total"]

      expect_equal(actualTotal,expectedTotal )

  })
test_that("doEstimationForAllStrata creates correct est.total for SAsampWtMes using H1",  {

  myTestData <- generateTestData("SA")

  # Edit our data so that we have SRSWOR on each level
  myTestData$su1selectMeth <- "SRSWOR"
  myTestData$su2selectMeth <- "SRSWOR"
  myTestData$su3selectMeth <- "SRSWOR"
  myTestData$su4selectMeth <- "SRSWOR"
  myTestData$su5selectMeth <- "SRSWOR"

  # Choose what we will estimate for
  targetValue <- "SAsampWtMes"

  # Get a point estimate for comparison
  x <- myTestData
  x$studyVariable <- x[,..targetValue]
  targetProbColumns <- names(x)[grep("^.*incProb$", names(x))]
  x$totalIncProb <- apply(x[, ..targetProbColumns], 1, prod)
  x$pointEstimate <- x$studyVariable / x$totalIncProb
  # Total estimate - add up all point estimates
  expectedTotal <- sum(x$pointEstimate , na.rm = TRUE)

  # Check the new function works - and the estimated total is the same
  myStrataResults <- doEstimationForAllStrata(myTestData, targetValue)
  actualTotal <- myStrataResults[myStrataResults$recType == "DE","est.total"]

  expect_equal(actualTotal,expectedTotal )

})

test_that("doEstimationForAllStrata gets some results for H8ExampleEE1",  {

  myTestData <- H8ExampleEE1
  # Only use a subset of the test data

  # create an est object
  myEstData <- createRDBESEstObject(myTestData, 8, "SA" )

  #convvert to Kgs from grammes
  myEstData$SAsampWtMes <- myEstData$SAsampWtMes /1000
  # Run estimation
  myResults <- doEstimationForAllStrata(myEstData, "SAsampWtMes")

  # Get the results for the TE strata
  January_actual <-  myResults[myResults$recType == "TE" & myResults$stratumName == "January" ,c("stratumName","est.total","est.mean", "se.total","se.mean")]
  February_actual <-  myResults[myResults$recType == "TE" & myResults$stratumName == "February" ,c("stratumName","est.total","est.mean", "se.total","se.mean")]
  March_actual <-  myResults[myResults$recType == "TE" & myResults$stratumName == "March" ,c("stratumName","est.total","est.mean", "se.total","se.mean")]


  # Check if the results are correct

  # January stratum
  expect_equal(round(January_actual$est.total,0), 138995)
  expect_equal(round(January_actual$se.total,0), 41709)
  expect_equal(round(January_actual$est.mean,3), 34748.659)
  expect_equal(round(January_actual$se.mean,3), 10427.312)

  # February stratum
  expect_equal(round(February_actual$est.total,0), 209271)
  expect_equal(round(February_actual$se.total,0), 97071)
  expect_equal(round(February_actual$est.mean,3), 52317.864)
  expect_equal(round(February_actual$se.mean,3), 24267.667)

  # March stratum
  expect_equal(round(March_actual$est.total,0), 32996)
  expect_equal(round(March_actual$se.total,0), 7213)
  expect_equal(round(March_actual$est.mean,3), 8248.884)
  expect_equal(round(March_actual$se.mean,3), 1803.148)



})

test_that("doEstimationForAllStrata creates get correct results for Lohr worked examples",  {

  myTestData <- RDBEScore::createRDBESDataObject("./h1_v_1_19_18/ZW_1965_WGRDBES-EST_TEST_1.zip")

  # Only use a subset of the test data
  myTestData <- filterRDBESDataObject(myTestData,c("DEstratumName"),c("Pckg_SDAResources_agstrat_H1"))
  myTestData <- findAndKillOrphans(myTestData, verbose = FALSE)

  validateRDBESDataObject(myTestData, checkDataTypes = TRUE)

  # ensure the numer of sampled and total items is set on the relevent levels
  myTestData[["SA"]]$SAnumSamp <- 1
  myTestData[["SA"]]$SAnumTotal <- 1
  myTestData[["FT"]]$FTnumSamp <- 1
  myTestData[["FT"]]$FTnumTotal <- 1
  myTestData[["FO"]]$FOnumSamp <- 1
  myTestData[["FO"]]$FOnumTotal <- 1
  myTestData[["SS"]]$SSnumSamp <- 1
  myTestData[["SS"]]$SSnumTotal <- 1

  # create an est object
  myEstData <- createRDBESEstObject(myTestData, 1, "SA" )

  # Run estimation
  myResults <- doEstimationForAllStrata(myEstData, "SAsampWtMes")

  # Get the results for the VS strata
  NC_actual <-  myResults[myResults$recType == "VS" & myResults$stratumName == "NC" ,c("stratumName","est.total","est.mean", "se.total","se.mean")]
  NE_actual <-  myResults[myResults$recType == "VS" & myResults$stratumName == "NE" ,c("stratumName","est.total","est.mean", "se.total","se.mean")]
  S_actual <-  myResults[myResults$recType == "VS" & myResults$stratumName == "S" ,c("stratumName","est.total","est.mean", "se.total","se.mean")]
  W_actual <-  myResults[myResults$recType == "VS" & myResults$stratumName == "W" ,c("stratumName","est.total","est.mean", "se.total","se.mean")]

  # Check if the results are correct

  # NC stratum
  expect_equal(round(NC_actual$est.total,0), 316731380)
  expect_equal(round(NC_actual$se.total,0), 16977399)
  expect_equal(round(NC_actual$est.mean,2), 300504.16)
  expect_equal(round(NC_actual$se.mean,2), 16107.59)

  # NE stratum
  expect_equal(round(NE_actual$est.total,0), 21478558)
  expect_equal(round(NE_actual$se.total,0), 3992889)
  expect_equal(round(NE_actual$est.mean,2), 97629.81)
  expect_equal(round(NE_actual$se.mean,2), 18149.49)

  # S stratum
  expect_equal(round(S_actual$est.total,0), 292037391)
  expect_equal(round(S_actual$se.total,0), 26154840)
  expect_equal(round(S_actual$est.mean,2), 211315.04)
  expect_equal(round(S_actual$se.mean,2), 18925.35)

  # W stratum
  expect_equal(round(W_actual$est.total,0), 279488706)
  expect_equal(round(W_actual$se.total,0), 39416342)
  expect_equal(round(W_actual$est.mean,2), 662295.51)
  expect_equal(round(W_actual$se.mean,2), 93403.65)


})

}) ## end capture.output
