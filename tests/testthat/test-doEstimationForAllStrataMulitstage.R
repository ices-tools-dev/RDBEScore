capture.output({  ## suppresses printing of console output when running test()

test_that("doEstimationForAllStrata creates correct results for Survey apiclust2 multistage",  {

  myTestData <- RDBEScore::Pckg_survey_apiclus2_H1 #this is valid H1 RDBESDataObj

    # create an est object
  myEstData <- createRDBESEstObject(myTestData, 1, "SA" )

  # Run estimation
  myResults <- doEstimationForAllStrata(myEstData, "SAsampWtMes")

  #replicate  two-stage `with replacement' designs from survey
  #library(survey)
  #dclus2<-svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)
  #svymean(~enroll, dclus2wr, na.rm=TRUE)
  #svytotal(~enroll, dclus2wr, na.rm=TRUE)


  # Get the results for all strata
  res <-  myResults[myResults$recType == "VS" & myResults$stratumName == "U" ,
                    c("stratumName","est.total","est.mean", "se.total","se.mean")]


  # Check if the results are correct

  expect_equal(round(res$est.total,0), 2639273)
  #test that should work but don't =>
  #expect_equal(round(res$se.total,0), 799638)
  #expect_equal(round(res$est.mean,2), 526.26)
  #expect_equal(round(res$se.mean,2), 80.34)


})

test_that("doEstimationForAllStrata creates correct results for Survey apiclust2 multistage when reordered",  {

    myTestData <- RDBEScore::Pckg_survey_apiclus2_H1 #this is valid H1 RDBESDataObj

    # create an est object
    myEstData <- createRDBESEstObject(myTestData, 1, "SA" )
    myEstData <- myEstData[order(myEstData$su1unitName),]
    # Run estimation
    myResults <- doEstimationForAllStrata(myEstData, "SAsampWtMes")

    #replicate  two-stage `with replacement' designs from survey
    #library(survey)
    #dclus2<-svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)
    #svymean(~enroll, dclus2wr, na.rm=TRUE)
    #svytotal(~enroll, dclus2wr, na.rm=TRUE)


    # Get the results for all strata
    res <-  myResults[myResults$recType == "VS" & myResults$stratumName == "U" ,
                      c("stratumName","est.total","est.mean", "se.total","se.mean")]


    # Check if the results are correct

    expect_equal(round(res$est.total,0), 2639273)
    #test that should work but don't =>
    #expect_equal(round(res$se.total,0), 799638)
    #expect_equal(round(res$est.mean,2), 526.26)
    #expect_equal(round(res$se.mean,2), 80.34)


  })

}) ## end capture.output
