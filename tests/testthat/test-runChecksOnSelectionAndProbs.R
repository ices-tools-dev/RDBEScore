capture.output({  ## suppresses printing of console output when running test()

test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present",  {

  myPath <- "./h1_v_1_19_26"
  myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
  # Only use the non-clustered test data
  myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myObject <- findAndKillOrphans(myObject, verbose = FALSE)

  myObject[["SA"]]$SAlowHierarchy <- "D"

  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})
  test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present - different numTotal for different strata",  {

    myPath <- "./h1_v_1_19_26"
    myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
    # Only use the non-clustered test data
    myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
    myObject <- findAndKillOrphans(myObject, verbose = FALSE)

    myObject[["SA"]]$SAlowHierarchy <- "D"
    myObject[["VS"]][myObject[["VS"]]$VSstratumName == "VS_stratum2","VSnumTotal"] <- 40
    expect_error(runChecksOnSelectionAndProbs(myObject),NA)

  })
test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present - different numSampled for different strata",  {

  myPath <- "./h1_v_1_19_26"
  myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
  # Only use the non-clustered test data
  myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myObject <- findAndKillOrphans(myObject, verbose = FALSE)

  myObject[["SA"]]$SAlowHierarchy <- "D"
  myObject[["VS"]][myObject[["VS"]]$VSstratumName == "VS_stratum2","VSnumSamp"] <- 2
  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})

})
