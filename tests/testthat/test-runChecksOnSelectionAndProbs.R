capture.output({  ## suppresses printing of console output when running test()


# Test that runChecksOnSelectionAndProbs runs without errors when stratification is present (H1 and lower hierarchy A)
test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present (H1 and lower hierarchy A)",  {

  myPath <- "./h1_v_20250211"
  myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
  # Only use the non-clustered test data
  myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myObject <- findAndKillOrphans(myObject, verbose = FALSE)

  myObject[["SA"]]$SAlowHierarchy <- "A"

  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})

# Test that runChecksOnSelectionAndProbs runs without errors when stratification is present (H1 and lower hierarchy B)
test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present (H1 and lower hierarchy B)",  {

  myPath <- "./h1_v_20250211"
  myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
  # Only use the non-clustered test data
  myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myObject <- findAndKillOrphans(myObject, verbose = FALSE)

  myObject[["SA"]]$SAlowHierarchy <- "B"

  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})

# Test that runChecksOnSelectionAndProbs runs without errors when stratification is present (H1 and lower hierarchy C)
test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present (H1 and lower hierarchy C)",  {


  myPath <- "./h1_v_20250211"
  myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
  # Only use the non-clustered test data
  myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myObject <- findAndKillOrphans(myObject, verbose = FALSE)

  myObject[["SA"]]$SAlowHierarchy <- "C"

  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})

test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present (H1 and lower hierarchy D)",  {

  myPath <- "./h1_v_20250211"
  myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
  # Only use the non-clustered test data
  myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myObject <- findAndKillOrphans(myObject, verbose = FALSE)

  myObject[["SA"]]$SAlowHierarchy <- "D"

  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})


test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present (H1 and lower hierarchy D) - different numTotal for different strata",  {

    myPath <- "./h1_v_20250211"
    myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
    # Only use the non-clustered test data
    myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
    myObject <- findAndKillOrphans(myObject, verbose = FALSE)

    myObject[["SA"]]$SAlowHierarchy <- "D"
    myObject[["VS"]][myObject[["VS"]]$VSstratumName == "VS_stratum2","VSnumTotal"] <- 40
    expect_error(runChecksOnSelectionAndProbs(myObject),NA)

  })
test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present (H1 and lower hierarchy D) - different numSampled for different strata",  {

  myPath <- "./h1_v_20250211"
  myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
  # Only use the non-clustered test data
  myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myObject <- findAndKillOrphans(myObject, verbose = FALSE)

  myObject[["SA"]]$SAlowHierarchy <- "D"
  myObject[["VS"]][myObject[["VS"]]$VSstratumName == "VS_stratum2","VSnumSamp"] <- 2
  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})

# Test that runChecksOnSelectionAndProbs runs without errors when stratification is present (H5 and lower hierarchy A)
test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present (H5 and lower hierarchy A)",  {

  myPath <- "./h5_v_20250211"
  myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
  # Only use the non-clustered test data
  myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H5","DE_stratum2_H5","DE_stratum3_H5"))
  myObject <- findAndKillOrphans(myObject, verbose = FALSE)

  myObject[["SA"]]$SAlowHierarchy <- "A"

  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})

# Test that runChecksOnSelectionAndProbs runs without errors when stratification is present (H5 and lower hierarchy B)
test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present (H5 and lower hierarchy B)",  {

  myPath <- "./h5_v_20250211"
  myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
  # Only use the non-clustered test data
  myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H5","DE_stratum2_H5","DE_stratum3_H5"))
  myObject <- findAndKillOrphans(myObject, verbose = FALSE)

  myObject[["SA"]]$SAlowHierarchy <- "B"

  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})

# Test that runChecksOnSelectionAndProbs runs without errors when stratification is present (H5 and lower hierarchy C)
test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present (H5 and lower hierarchy C)",  {

  myPath <- "./h5_v_20250211"
  myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
  # Only use the non-clustered test data
  myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H5","DE_stratum2_H5","DE_stratum3_H5"))
  myObject <- findAndKillOrphans(myObject, verbose = FALSE)

  myObject[["SA"]]$SAlowHierarchy <- "C"

  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})

# Test that runChecksOnSelectionAndProbs runs without errors when stratification is present (H5 and lower hierarchy D)
test_that("runChecksOnSelectionAndProbs runs without errors when stratification is present (H5 and lower hierarchy D)",  {

  myPath <- "./h5_v_20250211"
  myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
  # Only use the non-clustered test data
  myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H5","DE_stratum2_H5","DE_stratum3_H5"))
  myObject <- findAndKillOrphans(myObject, verbose = FALSE)

  myObject[["SA"]]$SAlowHierarchy <- "D"

  expect_error(runChecksOnSelectionAndProbs(myObject),NA)

})

})
