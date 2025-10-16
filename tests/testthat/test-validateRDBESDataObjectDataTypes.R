capture.output({  ## suppresses printing of console output when running test()

test_that("checkRDBESDataObjectDataTypes returns an empty list for an object with valid data types",  {

  myDE <- data.frame("DEid" = c(1L,2L),
                     "DErecType" = c("DE","DE"),
                     "DEsampScheme" = c("WGRDBES-EST TEST","WGRDBES-EST TEST"),
                     "DEsampSchemeType" = c("NatPilCF","NatPilCF"),
                     "DEyear" = c(1965L,1965L),
                     "DEstratumName" = c("1","2"),
                     "DEhierarchyCor" = c("Y","N"),
                     "DEhierarchy" = c(1L,1L),
                     "DEsamp" = c("Y","N"),
                     "DEnoSampReason" = c ("",""),
                     stringsAsFactors = FALSE
  )

    myObject <- list()
    myObject[["DE"]] <- myDE

    myDiffs <- validateRDBESDataObjectDataTypes(myObject)
    numberOfDifferences <- nrow(myDiffs)
    expect_equal(numberOfDifferences,0)
})
test_that("checkRDBESDataObjectDataTypes returns 2 differences for an object with 2 invalid data types",  {

  myDE <- data.frame("DEid" = c("1","2"), # WRONG
                     "DErecType" = c("DE","DE"),
                     "DEsampScheme" = c("WGRDBES-EST TEST","WGRDBES-EST TEST"),
                     "DEsampSchemeType" = c(1,2), #WRONG
                     "DEyear" = c(1965L,1965L),
                     "DEstratumName" = c("1","2"),
                     "DEhierarchyCor" = c("Y","N"),
                     "DEhierarchy" = c(1L,1L),
                     "DEsamp" = c("Y","N"),
                     "DEnoSampReason" = c ("",""),
                     stringsAsFactors = FALSE
  )


  myObject <- list()
  myObject[["DE"]] <- myDE

  myDiffs <- validateRDBESDataObjectDataTypes(myObject)
  numberOfDifferences <- nrow(myDiffs)
  expect_equal(numberOfDifferences,2)
})

test_that("checkRDBESDataObjectDataTypes treats expected integer vs actual numeric as compatible (issue #251)",  {

  # Here DEid, DEyear and DEhierarchy are provided as numeric (double)
  # while the mapping expects integer. We want this to be considered OK.
  myDE <- data.frame("DEid" = c(1,2),              # numeric (not 1L, 2L)
                     "DErecType" = c("DE","DE"),
                     "DEsampScheme" = c("WGRDBES-EST TEST","WGRDBES-EST TEST"),
                     "DEsampSchemeType" = c("NatPilCF","NatPilCF"),
                     "DEyear" = c(1965,1965),      # numeric
                     "DEstratumName" = c("1","2"),
                     "DEhierarchyCor" = c("Y","N"),
                     "DEhierarchy" = c(1,1),       # numeric
                     "DEsamp" = c("Y","N"),
                     "DEnoSampReason" = c ("",""),
                     stringsAsFactors = FALSE
  )

  myObject <- list()
  myObject[["DE"]] <- myDE

  myDiffs <- validateRDBESDataObjectDataTypes(myObject)
  numberOfDifferences <- nrow(myDiffs)
  expect_equal(numberOfDifferences,0)
})

}) ## end capture.output
