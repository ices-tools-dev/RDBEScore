capture.output({  ## suppresses printing of console output when running test()

  # common parameters
  # H1 directory
  dirH1 <- "./h1_v_1_19_26/"
  # H5 directory
  dirH5 <- "./h5_v_1_19_26/"
  # H7 directory
  dirH7 <- "./h7_v_1_19_26/"

createTestObjectFromH1Data <- function(){

  myRawObject <- createRDBESDataObject(input = dirH1)

  # Only use a subset of the test data
  myRawObject <- filterRDBESDataObject(myRawObject,
                                       c("DEstratumName"),
                                       c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"),
                                       killOrphans = TRUE)

  myEstObject <- createRDBESEstObject(myRawObject,1)
  myEstObject

}

test_that("filterRDBESEstObject returns the correct result for RDBESEstObject created from H1 data (1)", {

  myEst <- createTestObjectFromH1Data()

  myFilteredEst <- filterRDBESEstObject(myEst,
                                        c("BVid"),
                                        c(7349207))
  expect_equal(nrow(myFilteredEst), 1)

})

test_that("filterRDBESEstObject returns the correct result for RDBESEstObject created from H1 data (2)", {

  myEst <- createTestObjectFromH1Data()

  myFilteredEst <- filterRDBESEstObject(myEst,
                                        c("BVid"),
                                        c(7349207, 7349208))
  expect_equal(nrow(myFilteredEst), 2)
})

test_that("filterRDBESEstObject returns the correct result for RDBESEstObject created from H1 data (3)", {

  myEst <- createTestObjectFromH1Data()

  myFilteredEst <- filterRDBESEstObject(myEst,
                                        c("DEstratumName"),
                                        c("DE_stratum1_H1"))
  expect_equal(nrow(myFilteredEst), 4860)
})

test_that("filterRDBESEstObject returns the correct result for RDBESEstObject created from H1 data (4)", {

  myEst <- createTestObjectFromH1Data()

  myFilteredEst <- filterRDBESEstObject(myEst,
                                        c("BVtypeMeas", "BVvalueMeas","SAspeCode"),
                                        c("Age",1,2,3,101804))

  expect_equal(nrow(myFilteredEst), 132)
})

test_that("filterRDBESEstObject returns the correct result for RDBESEstObject created from H1 data (5)", {

  myEst <- createTestObjectFromH1Data()

  myFilteredEst <- filterRDBESEstObject(myEst,
                                        c("BVtypeMeas", "BVvalueMeas","SAspeCode"),
                                        c("Age",101804))

  expect_equal(nrow(myFilteredEst), 0)
})

test_that("filterRDBESEstObject returns a warning when an invalid field is given", {

  myEst <- createTestObjectFromH1Data()

  expect_warning(filterRDBESEstObject(myEst,
                                      c("abc", "BVvalueMeas","SAspeCode"),
                                      c("Age",101804)),
                 "The following fields were not found in the RDBESEstObject: abc")

})


}) ## end capture.output
