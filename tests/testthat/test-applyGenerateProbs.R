capture.output({  ## suppresses printing of console output when running test()

  test_that("applyGenerateProbs runs without errors when stratification is present",  {
    myPath <- "./h1_v_1_19_26"
    myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
    # Only use the non-clustered test data
    myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("DE_stratum1_H1"), killOrphans = T)
    myObject[["FM"]]<-myObject[["FM"]][0,]
    myObject[["BV"]]<-myObject[["BV"]][0,]
    myObject[["SA"]]$SAlowHierarchy <- "D"
    myObject[["VS"]][myObject[["VS"]]$VSstratumName == "VS_stratum2","VSnumTotal"] <- 40

    expect_error(applyGenerateProbs(myObject, "selection"),NA)
  })

  test_that("applyGenerateProbs stops errors when clustering is present",  {
    myPath <- "./h1_v_1_19_26"
    myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
    # Only use the non-clustered test data
    myObject <- filterRDBESDataObject(myObject,c("DEstratumName"),c("Pckg_SDAResources_coots_H1"), killOrphans = T)
    myObject[["FM"]]<-myObject[["FM"]][0,]
    myObject[["BV"]]<-myObject[["BV"]][0,]
    myObject[["SA"]]$SAlowHierarchy <- "D"

    myObject$VS$VSclustering
    expect_error(applyGenerateProbs(myObject, "selection"),"clusters present: not yet specified")
})

  test_that("produces the right inclusion probabilities from numTotal and numSampled under SRSWOR",  {
    myPath <- "./h1_v_1_19_26"
    myObject <- importRDBESDataCSV(rdbesExtractPath = myPath)
    # Only use the non-clustered test data
    myObject <- filterRDBESDataObject(myObject,c("DEstratumName","VSstratumName"),c("Pckg_SDAResources_agstrat_H1","NC"), killOrphans = T)
    myObject[["FM"]]<-myObject[["FM"]][0,]
    myObject[["BV"]]<-myObject[["BV"]][0,]
    myObject[["SA"]]$SAlowHierarchy <- "D"
    expect_identical(all(applyGenerateProbs(myObject, "inclusion")$VS$VSincProb==103/1054), TRUE)
  })

}) ## end capture.output
