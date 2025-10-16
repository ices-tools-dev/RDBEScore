capture.output({  ## suppresses printing of console output when running test()




  # common parameters
  # H1 directory
  dirH1 <- "./h1_v_20250211/"
  # H5 directory
  dirH5 <- "./h5_v_20250211/"

  # H1 object for comparison
  expObjH1 <- H1Example


  # Test CSV inputs ---------------------------------------------------------


  test_that("importRDBESDataCSV can create an object from a H1 data extract
          without errors or warnings",  {


            expect_warning(importRDBESDataCSV(rdbesExtractPath = dirH1), NA)
            expect_error(importRDBESDataCSV(rdbesExtractPath = dirH1), NA)


          })





  test_that("importRDBESDataCSV can create an object from an H1 data extract by specifying file names without errors or warnings",  {

    myFileNames <- list("DE"="DE.csv","SD"="SD.csv")

    myObject <- expect_warning(importRDBESDataCSV(rdbesExtractPath = dirH1, listOfFileNames = myFileNames), NA)
    myObject <- expect_error(importRDBESDataCSV(rdbesExtractPath = dirH1, listOfFileNames = myFileNames), NA)

  })

  test_that("importRDBESDataCSV Errors if SL is present but IS is missing",  {


    expect_error(importRDBESDataCSV(rdbesExtractPath = dirH1, listOfFileNames = list("DE"="DE.csv","SL"="SpeciesList.csv")),
                 "objectToCheck contains a non-empty SL table but the IS table is either NULL or empty.")

  })


  test_that("importRDBESDataCSV keeps extra long BVfishId if character",  {
    #make a temporary BV table from the package data
    BV <- H1Example$BV[1,]
    #make a 40 char length  dummy ID this is the max length in RDBES
    id <- paste0(rep("A", 40), collapse = "")
    BV$BVfishId <- id
    tempBVfNname <- tempfile(fileext = ".csv")
    write.csv(BV, file=tempBVfNname, quote=F, row.names = F)
    fname <- basename(tempBVfNname)
    dirname <- dirname(tempBVfNname)
    BV <- importRDBESDataCSV(rdbesExtractPath = dirname, listOfFileNames = list("BV"=fname))$BV
    file.remove(tempBVfNname)
    expect_equal(BV$BVfishId[1],id)
  })

  test_that("importRDBESDataCSV keeps extra long BVfishId if int",  {
    #make a temporary BV table from the package data
    BV <- H1Example$BV[1,]
    #make a 40 char length  dummy ID this is the max length in RDBES
    id <- paste0(rep("1", 40), collapse = "")
    #its not possible to get the exact number if we save as numeric
    id <- format(as.numeric(id), scientific = FALSE, trim = TRUE)
    #the thing we want to test is if we have a long numeric string in there
    #do we get the same string back in when importing
    BV$BVfishId <- id
    tempBVfNname <- tempfile(fileext = ".csv")
    write.csv(BV, file=tempBVfNname, quote=F, row.names = F)
    fname <- basename(tempBVfNname)
    dirname <- dirname(tempBVfNname)
    BV <- importRDBESDataCSV(rdbesExtractPath = dirname, listOfFileNames = list("BV"=fname))$BV
    file.remove(tempBVfNname)
    expect_equal(BV$BVfishId[1],id)
  })




}) ## end capture.output



