  capture.output({  ## suppresses printing of console output when running test()

test_that("validateRDBESobject does not produce errors or warnings",  {

  myEmptyObject <- createRDBESDataObject()

  expect_warning(validateRDBESDataObject(objectToCheck = myEmptyObject,
                                        verbose = FALSE),NA)
  expect_error(validateRDBESDataObject(objectToCheck = myEmptyObject,
                                      verbose = FALSE),NA)
})
test_that(
  "validateRDBESDataObject returns empty object for valid empty object",  {

  myEmptyObject <- createRDBESDataObject()
  myReturn <- validateRDBESDataObject(objectToCheck = myEmptyObject,
                                     verbose = FALSE)
  expect_equal(myEmptyObject, myReturn)

})
test_that(
  "validateRDBESDataObject returns input object for valid object from H1 data",
  {

  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_equal(myObject, myReturn)

})
test_that("validateRDBESDataObject returns same object for valid object from H1
          data (also checking data types)",  {

  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_equal(myObject, myReturn)

})
test_that("validateRDBESDataObject returns same object for valid object from H5
          data",  {

  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h5_v_20250211")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE)
  expect_equal(myObject, myReturn)

})
test_that("validateRDBESDataObject returns same object for valid object from H5
          data (also checking data types)",  {

  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h5_v_20250211")
  myReturn <- validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE)
  expect_equal(myObject, myReturn)

})
test_that("validateRDBESDataObject produces error for NA",  {

  expect_error(validateRDBESDataObject(objectToCheck = NA,
                                     verbose = FALSE))

})
test_that("validateRDBESDataObject produces error for NULL",  {

  expect_error(validateRDBESDataObject(objectToCheck = NULL,
                                       verbose = FALSE))

})
test_that("validateRDBESDataObject produces error for object that is not a
          list",  {

  myNonList <- data.frame (tableNames  = c("DE", "SE"))
  expect_error(validateRDBESDataObject(objectToCheck = myNonList,
                                     verbose = FALSE))
})

test_that("validateRDBESDataObject produces error for object with extra name",
          {
  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myObject[['XX']] <- F
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object without all names",
          {
  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myObject[['DE']] <- NULL
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object with a required
          field removed",  {
  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myObject[['DE']]$DEsampScheme <- NULL
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object with a
           'non-essential' field removed (when strict)",  {
            myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
            myObject[['FT']]$FTdepLoc <- NULL
            expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                                 verbose = FALSE,
                                                 strict = TRUE))
})
test_that("validateRDBESDataObject does not produce an error for object with a
           'non-essential' field removed (when not strict)",  {
             myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
             myObject[['FT']]$FTdepLoc <- NULL
             expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                                  verbose = FALSE,
                                                  strict = FALSE),
                                                  NA)
           })
test_that("validateRDBESDataObject produces error for object with duplicate
          rows",  {
  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myObject[['DE']] <- data.table::rbindlist(list(myObject[['DE']],myObject[['DE']]))
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object with duplicate
          DEid values",  {
  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myObject[['DE']][,"DEid"]  <- replicate(nrow(myObject[['DE']]),1)
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object with invalid data
          types (when checking data types)",  {
  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myObject[["DE"]]$DEid <- as.character(myObject[["DE"]]$DEid)
  myObject[["SD"]]$SDid <- as.character(myObject[["SD"]]$SDid)
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE))
})
test_that("validateRDBESDataObject produces error for object with with
          duplicate DEid values and invalid data types (when checking data
          types)",  {
  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myObject[['DE']][,"DEid"]  <- replicate(nrow(myObject[['DE']]),1)
  myObject[["DE"]]$DEid <- as.character(myObject[["DE"]]$DEid)
  myObject[["SD"]]$SDid <- as.character(myObject[["SD"]]$SDid)
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                     checkDataTypes = TRUE,
                                     verbose = FALSE))
})

test_that("validateRDBESDataObject prints empty table note",  {

  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myObject[['FT']] <-   myObject[['FT']][FALSE,]
  expect_output(validateRDBESDataObject(objectToCheck = myObject,
                                      verbose = TRUE), regexp = "Note that FT has 0 rows")

})

test_that("validateRDBESDataObject produces error if keys aren't set on a data table",  {

  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  # Remove the key from DE
  data.table::setkey(myObject[['DE']],NULL)
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                        verbose = TRUE), regexp = "DE does not have a key set")
})

test_that("validateRDBESDataObject prints unnecessary column note",  {

  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myObject[['FT']]$myExtraColumn <- 'Test'
  expect_output(validateRDBESDataObject(objectToCheck = myObject,
                                       verbose = TRUE, strict = FALSE),
                regexp = ".*The following unnecessary columns were found in table FT : myExtraColumn.*")

})

test_that("validateRDBESDataObject produces error for unnecessary column when strict = TRUE",  {

  myObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")
  myObject[['FT']]$myExtraColumn <- 'Test'
  expect_error(validateRDBESDataObject(objectToCheck = myObject,
                                        verbose = TRUE, strict = TRUE),
                regexp = ".*objectToCheck contains the following tables which either don't contain all required fields or they include unnecessary columns FT*")

})

test_that("validateRDBESDataObject produces correct text output", {

  myObject <- Pckg_survey_apistrat_H1

  expectedText <- c("Note that TE is NULL but this is allowed in an RDBESDataObject",
    "Note that LO is NULL but this is allowed in an RDBESDataObject",
    "Note that OS is NULL but this is allowed in an RDBESDataObject",
    "Note that LE is NULL but this is allowed in an RDBESDataObject",
    "Note that CL is NULL but this is allowed in an RDBESDataObject",
    "Note that CE is NULL but this is allowed in an RDBESDataObject",
    "Note that FM has 0 rows but this is allowed in an RDBESDataObject",
    "Note that BV has 0 rows but this is allowed in an RDBESDataObject"
    )
  expectedText <- paste("^.*",expectedText,".*$", sep = "")

  myOutput <- capture_output_lines(validateRDBESDataObject(objectToCheck = myObject,
                                         verbose = TRUE))
  # Make sure we have the right number of text lines output
  expect_equal(length(myOutput),length(expectedText))
  # Make sure we have the text about NULL entries
  for (i in 1:length(expectedText)){
    expect_match(myOutput[i],expectedText[i])
  }


})

# Tests for CHECK 5b: SL non-empty requires non-empty IS
test_that("validateRDBESDataObject errors when SL has rows and IS is NULL", {
  # Build minimal object: non-empty SL, NULL IS
  myObject <- data.table::copy(H1Example)

  myObject$IS <- NULL

  expect_error(
    validateRDBESDataObject(objectToCheck = myObject, verbose = FALSE),
    regexp = "objectToCheck is a list but does not contain all the required names. The missing table names are: IS"
  )
})

test_that("validateRDBESDataObject errors when SL has rows and IS is empty", {
  # Build minimal object: non-empty SL, empty IS
  myObject <- data.table::copy(H1Example)

  myObject$IS <- data.table::copy(myObject$IS[FALSE,])

  expect_error(
    validateRDBESDataObject(objectToCheck = myObject, verbose = FALSE),
    regexp = "objectToCheck contains a non-empty SL table but the IS table is either NULL or empty."
  )
})


}) ## end capture.output
