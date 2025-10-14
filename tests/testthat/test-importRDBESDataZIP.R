capture.output({  ## suppresses printing of console output when running test()




  # common parameters
  # H1 directory
  dirH1 <- "./h1_v_20250211/"


  # H1 object for comparison
  expObjH1 <- H1Example

test_that("createRDBESDataObject will throw an error when given multiple inputs", {
  zipFiles <- c(
    "H1_Example.zip"
  )
  H1 <- paste0(dirH1, zipFiles)
  df <- as.data.frame(1)

  expect_error(createRDBESDataObject(input = c(H1, df, dirH1)), "Input type not recognised. Should be a RDBES zip file, folder of csv files, or list of data frames.")

})
test_that("importing foldered zipped H1 example data works", {
  zipFiles <- c(
    "H1_Example_fd.zip"
  )

  genObj <- createRDBESDataObject(paste0(dirH1, zipFiles),
                                  castToCorrectDataTypes = TRUE)

  expect_equal(genObj, expObjH1)


})

test_that("importing foldered zipped H1 example data tables have non 0 rows on DE table", {
  zipFiles <- c(
    "H1_Example_fd.zip"
  )

  genObj <- createRDBESDataObject(paste0(dirH1, zipFiles),
                                  castToCorrectDataTypes = TRUE)

  expect_equal(nrow(genObj$DE),8)


})


})
