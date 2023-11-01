capture.output({  ## suppresses printing of console output when running test()

test_that("createRDBESEstObject can create an object from an H1 data extract
          with no warnings or errors",  {

  myPath <- "./h1_v_1_19_18"
  myRawObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

  # Only use a subset of the test data
  myRawObject <- filterRDBESDataObject(myRawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myRawObject <- findAndKillOrphans(myRawObject, verbose = FALSE)

  myEstObject <- expect_warning(createRDBESEstObject(myRawObject,1),NA)
  myEstObject <- expect_error(createRDBESEstObject(myRawObject,1),NA)
  # Check we have the right number of sampling unit fields
  expect_equal(length(grep("^su1.*",names(myEstObject))),24)
  expect_equal(length(grep("^su2.*",names(myEstObject))),24)
  expect_equal(length(grep("^su3.*",names(myEstObject))),24)
  expect_equal(length(grep("^su4.*",names(myEstObject))),24)
  expect_equal(length(grep("^su5.*",names(myEstObject))),17)
  expect_equal(length(grep("^su6.*",names(myEstObject))),0)
  expect_equal(length(grep("^su7.*",names(myEstObject))),0)
  expect_equal(length(grep("^su8.*",names(myEstObject))),0)


})
test_that("createRDBESEstObject can create an object from an H5 data extract
          with no warnings or errors",  {

  myPath <- "./h5_v_1_19_18"
  myRawObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

  myEstObject <- expect_warning(createRDBESEstObject(myRawObject,5),NA)
  myEstObject <- expect_error(createRDBESEstObject(myRawObject,5),NA)
  # Check we have the right number of sampling unit fields
  expect_equal(length(grep("^su1.*",names(myEstObject))),24)
  expect_equal(length(grep("^su2.*",names(myEstObject))),24)
  expect_equal(length(grep("^su3.*",names(myEstObject))),24)
  expect_equal(length(grep("^su4.*",names(myEstObject))),17)
  expect_equal(length(grep("^su5.*",names(myEstObject))),0)
  #optional tables should be excluded from hierarchy when making estObjects
  #they have 1:1 relationship with the parent table and are not in hierarchy
  #expect_equal(length(grep("^su4.*",names(myEstObject))),24)
  #expect_equal(length(grep("^su5.*",names(myEstObject))),17)
  expect_equal(length(grep("^su6.*",names(myEstObject))),0)
  expect_equal(length(grep("^su7.*",names(myEstObject))),0)
  expect_equal(length(grep("^su8.*",names(myEstObject))),0)


})
test_that("createRDBESEstObject can create an object from an empty H1 data extract
          with no warnings or errors",  {

  myRawObject <- createRDBESDataObject()

  myEstObject <- expect_warning(createRDBESEstObject(myRawObject,1),NA)
  myEstObject <- expect_error(createRDBESEstObject(myRawObject,1),NA)
  # Check we have the right number of sampling unit fields
  expect_equal(length(grep("^su1.*",names(myEstObject))),0)
  expect_equal(length(grep("^su2.*",names(myEstObject))),0)
  expect_equal(length(grep("^su3.*",names(myEstObject))),0)
  expect_equal(length(grep("^su4.*",names(myEstObject))),0)
  expect_equal(length(grep("^su5.*",names(myEstObject))),0)
  expect_equal(length(grep("^su6.*",names(myEstObject))),0)
  expect_equal(length(grep("^su7.*",names(myEstObject))),0)
  expect_equal(length(grep("^su8.*",names(myEstObject))),0)


})
test_that("createRDBESEstObject can create an object from an H1 data extract with sub-sampling
          with no warnings or errors",  {

  myPath <- "./h1_v_1_19_18"
  myRawObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

  # Only use a subset of the test data
  myRawObject <- filterRDBESDataObject(myRawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myRawObject <- findAndKillOrphans(myRawObject, verbose = FALSE)

  # Put some sub-sampling in the test data
  # Remove column because it seems to be logical data type
  #myRawObject[["SA"]][, SAparentID:= NULL]
  # Generate some new parent ids
  myRawObject[["SA"]][51:100,"SAparSequNum"] <- myRawObject[["SA"]][1:50,"SAseqNum"]
  myRawObject[["SA"]][101:125,"SAparSequNum"] <- myRawObject[["SA"]][51:75,"SAseqNum"]
  # Fix the column order
  #setcolorder(myRawObject[["SA"]],
  #            c("SAid","SAparentID", setdiff(names(myRawObject[["SA"]]), c("SAid","SAparentID"))   )
  #)

  myEstObject <- expect_warning(createRDBESEstObject(myRawObject,1),NA)
  myEstObject <- expect_error(createRDBESEstObject(myRawObject,1),NA)
  # Check we have the right number of sampling unit fields
  expect_equal(length(grep("^su1.*",names(myEstObject))),24)
  expect_equal(length(grep("^su2.*",names(myEstObject))),24)
  expect_equal(length(grep("^su3.*",names(myEstObject))),24)
  expect_equal(length(grep("^su4.*",names(myEstObject))),24)
  expect_equal(length(grep("^su5.*",names(myEstObject))),17)
  expect_equal(length(grep("^su6.*",names(myEstObject))),17)
  expect_equal(length(grep("^su7.*",names(myEstObject))),17)
  expect_equal(length(grep("^su8.*",names(myEstObject))),0)


})
test_that("createRDBESEstObject fails when an invalid hierarchy is requested",  {

  myPath <- "./h1_v_1_19_18"
  myRawObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

  myEstObject <- expect_error(createRDBESEstObject(myRawObject,99),"An invalid value was used for the 'hierarchyToUse' parameter - createRDBESEstObject will not proceed")

})
test_that("createRDBESEstObject can create an object from an H1 data extract
          with no warnings or errors, stopping at VS",  {

    myPath <- "./h1_v_1_19_18"
    myRawObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

    # Only use a subset of the test data
    myRawObject <- filterRDBESDataObject(myRawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
    myRawObject <- findAndKillOrphans(myRawObject, verbose = FALSE)

    myEstObject <- expect_warning(createRDBESEstObject(myRawObject,1,stopTable = "VS"),NA)
    myEstObject <- expect_error(createRDBESEstObject(myRawObject,1,stopTable = "VS"),NA)
    expect_equal(unique(myEstObject$su1table),"VS")
    # Check we have the right number of sampling unit fields
    expect_equal(length(grep("^su1.*",names(myEstObject))),24)
    expect_equal(length(grep("^su2.*",names(myEstObject))),0)
    expect_equal(length(grep("^su3.*",names(myEstObject))),0)
    expect_equal(length(grep("^su4.*",names(myEstObject))),0)
    expect_equal(length(grep("^su5.*",names(myEstObject))),0)
    expect_equal(length(grep("^su6.*",names(myEstObject))),0)
    expect_equal(length(grep("^su7.*",names(myEstObject))),0)
    expect_equal(length(grep("^su8.*",names(myEstObject))),0)
})
test_that("createRDBESEstObject can correctly create an object when there is no BV data",  {

    myPath <- "./h1_v_1_19_18"
    myRawObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

    # Only use a subset of the test data
    myRawObject <- filterRDBESDataObject(myRawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
    myRawObject <- findAndKillOrphans(myRawObject, verbose = FALSE)

    # Filter the object
    myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "DEid", valuesToFilter = 5406)
    myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "VSid", valuesToFilter = 78006)
    myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "FTunitName", valuesToFilter = "FT_unit_1")
    myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "FOunitName", valuesToFilter = "FO_unit_1")
    myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "SSid", valuesToFilter = 225062)
    myRawObject <- findAndKillOrphans(myRawObject)

    # get rid of BV data
    myRawObject[["SA"]]$SAlowHierarchy <- "B"
    myRawObject["BV"] <- list(NULL)

    myEstObject <- createRDBESEstObject(myRawObject, hierarchyToUse = 1)
    expect_equal(nrow(myEstObject),10)

})
test_that("createRDBESEstObject can correctly create an object when there is no FM data",  {

  myPath <- "./h1_v_1_19_18"
  myRawObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

  # Only use a subset of the test data
  myRawObject <- filterRDBESDataObject(myRawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myRawObject <- findAndKillOrphans(myRawObject, verbose = FALSE)

  # Filter the object
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "DEid", valuesToFilter = 5406)
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "VSid", valuesToFilter = 78006)
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "FTunitName", valuesToFilter = "FT_unit_1")
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "FOunitName", valuesToFilter = "FO_unit_1")
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "SSid", valuesToFilter = 225062)
  myRawObject <- findAndKillOrphans(myRawObject)

  # get rid of FM data
  myRawObject[["SA"]]$SAlowHierarchy <- "C"
  myRawObject["FM"] <- list(NULL)

  myRawObject[["BV"]][,"SAid"]<-548860
  myRawObject[["BV"]]$FMid <- NA

  myEstObject <- createRDBESEstObject(myRawObject, hierarchyToUse = 1)
  expect_equal(nrow(myEstObject),20)

})
test_that("createRDBESEstObject can correctly create an object when there is no FM or BV data",  {

  myPath <- "./h1_v_1_19_18"
  myRawObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

  # Only use a subset of the test data
  myRawObject <- filterRDBESDataObject(myRawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myRawObject <- findAndKillOrphans(myRawObject, verbose = FALSE)

  # Filter the object
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "DEid", valuesToFilter = 5406)
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "VSid", valuesToFilter = 78006)
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "FTunitName", valuesToFilter = "FT_unit_1")
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "FOunitName", valuesToFilter = "FO_unit_1")
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "SSid", valuesToFilter = 225062)
  myRawObject <- findAndKillOrphans(myRawObject)

  # get rid of FM and BV data
  myRawObject[["SA"]]$SAlowHierarchy <- "D"
  myRawObject["FM"] <- list(NULL)
  myRawObject["BV"] <- list(NULL)

  myEstObject <- createRDBESEstObject(myRawObject, hierarchyToUse = 1)
  expect_equal(nrow(myEstObject),1)

})
test_that("createRDBESEstObject creates the correct number of rows when there is sub-sampling present (1)",  {

  myPath <- "./h1_v_1_19_18"
  myRawObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

  # Only use a subset of the test data
  myRawObject <- filterRDBESDataObject(myRawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myRawObject <- findAndKillOrphans(myRawObject, verbose = FALSE)

  # Filter the object
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "DEid", valuesToFilter = 5406)
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "VSid", valuesToFilter = 78006)
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "FTunitName", valuesToFilter = "FT_unit_1")
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "FOunitName", valuesToFilter = "FO_unit_1")
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "SSid", valuesToFilter = 225062)
  myRawObject <- findAndKillOrphans(myRawObject)

  # Introduce sub-sampling - only the lowest level has data
  myRawObject[["SA"]]$SAsamp <- "N"
  myRawObject[["SA"]]$SAlowHierarchy <- "D"
  newSARows <- data.table::copy(myRawObject[["SA"]])
  #newSARows$SAparentID <- newSARows$SAid
  newSARows$SAparSequNum <- newSARows$SAseqNum
  newSARows$SAid <- newSARows$SAid +1
  newSARows$SAseqNum <- newSARows$SAseqNum +1
  newSARows2 <- data.table::copy(newSARows)
  #newSARows2$SAparentID <- newSARows2$SAid
  newSARows2$SAparSequNum <- newSARows2$SAseqNum
  newSARows2$SAid <- newSARows2$SAid +1
  newSARows2$SAseqNum <- newSARows2$SAseqNum +1
  newSARows2$SAsamp <- "Y"
  newSARows2$SAlowHierarchy <- "B"

  myRawObject["BV"] <- list(NULL)
  myRawObject[["SA"]]<-rbind(myRawObject[["SA"]],newSARows,newSARows2)
  # Need to re-set the key afer the rbind
  data.table::setkey(myRawObject[["SA"]],SAid)

  myRawObject[["FM"]][myRawObject[["FM"]]$SAid == 548860,"SAid"] <- 548862

  myEstObject <- createRDBESEstObject(myRawObject, hierarchyToUse = 1)
  expect_equal(nrow(myEstObject),10)

})
test_that("createRDBESEstObject creates the correct number of rows when there is sub-sampling present (2)",  {

  myPath <- "./h1_v_1_19_18"
  myRawObject <- importRDBESDataCSV(rdbesExtractPath = myPath)

  # Only use a subset of the test data
  myRawObject <- filterRDBESDataObject(myRawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myRawObject <- findAndKillOrphans(myRawObject, verbose = FALSE)

  # Filter the object
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "DEid", valuesToFilter = 5406)
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "VSid", valuesToFilter = 78006)
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "FTunitName", valuesToFilter = "FT_unit_1")
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "FOunitName", valuesToFilter = "FO_unit_1")
  myRawObject <- filterRDBESDataObject(myRawObject, fieldsToFilter = "SSid", valuesToFilter = 225062)
  myRawObject <- findAndKillOrphans(myRawObject)

  # Introduce sub-sampling - the upper and lowest level of sampling have data
  myRawObject[["SA"]]$SAsamp <- "N"
  myRawObject[["SA"]]$SAlowHierarchy <- "D"
  newSARows <- data.table::copy(myRawObject[["SA"]])
  myRawObject[["SA"]]$SAsamp <- "Y"
  myRawObject[["SA"]]$SAlowHierarchy <- "B"
  #newSARows$SAparentID <- newSARows$SAid
  newSARows$SAparSequNum <- newSARows$SAseqNum
  newSARows$SAid <- newSARows$SAid +1
  newSARows$SAseqNum <- newSARows$SAseqNum +1
  newSARows2 <- data.table::copy(newSARows)
  #newSARows2$SAparentID <- newSARows2$SAid
  newSARows2$SAparSequNum <- newSARows2$SAseqNum
  newSARows2$SAid <- newSARows2$SAid +1
  newSARows2$SAseqNum <- newSARows2$SAseqNum +1
  newSARows2[1,"SAsamp"] <- "Y"
  newSARows2[1,"SAlowHierarchy"] <- "B"
  myRawObject[["SA"]][2,"SAsamp"] <- "Y"
  myRawObject[["SA"]][2,"SAlowHierarchy"] <- "B"

  myRawObject["BV"] <- list(NULL)
  myRawObject[["SA"]]<-rbind(myRawObject[["SA"]],newSARows,newSARows2)
  # Need to re-set the key afer the rbind
  data.table::setkey(myRawObject[["SA"]],SAid)

  newFM <- data.table::copy(myRawObject[["FM"]])
  newFM[newFM$SAid == 548860,"SAid"] <- 548862
  newFM$FMid <- newFM$FMid + 50

  myRawObject[["FM"]]<- rbind(myRawObject[["FM"]],newFM)
  # Need to re-set the key afer the rbind
  data.table::setkey(myRawObject[["FM"]],FMid)

  myEstObject <- createRDBESEstObject(myRawObject, hierarchyToUse = 1)
  expect_equal(nrow(myEstObject),20)

})

test_that("createRDBESEstObject does not create parentID columns",  {

            myPath <- "./h1_v_1_19_18"
            myRawObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
            myEstObject <- createRDBESEstObject(myRawObject,1)

            if (sum(grepl(".*parentID", names(myEstObject)), na.rm = TRUE) >0){
              SAParentIDExists <- TRUE
            } else {
              SAParentIDExists <- FALSE
            }
            expect_false(SAParentIDExists)
})

}) ## end capture.output
