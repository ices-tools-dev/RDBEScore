capture.output({  ## suppresses printing of console output when running test()

test_that("filterRDBESDataObject returns the correct result for
          H1 (1)", {

  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")

  # Only use a subset of the test data
  myH1RawObject <- filterRDBESDataObject(myH1RawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

  myFields <- c("FTarvLoc")
  myValues <- c("ZWBFO")
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues
  )
  myFilteredObject <- findAndKillOrphans(myFilteredObject, verbose = FALSE)

  # Check the filtered object is ok (should return the object)
  expect_equal(myFilteredObject, validateRDBESDataObject(myFilteredObject,
                                                         verbose = FALSE))
  # Check the expected number of FT rows are returned
  expect_equal(nrow(myFilteredObject[["FT"]]), 20)
})

test_that("filterRDBESDataObject returns the correct result for H1 (2)", {
  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")

  # Only use a subset of the test data
  myH1RawObject <- filterRDBESDataObject(myH1RawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

  myFields <- c("FTarvLoc")
  myValues <- c("ZWBFO", "ZWBZH")
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues
  )

  # Check the filtered object is ok (should return TRUE)
  expect_equal(myFilteredObject, validateRDBESDataObject(myFilteredObject,
                                                         verbose = FALSE))
  # Check the expected number of FT rows are returned
  expect_equal(nrow(myFilteredObject[["FT"]]), 35)
})
test_that("filterRDBESDataObject returns the correct result for two fields/
          two values", {
  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
  myFields <- c("DEyear", "DEhierarchy")
  myValues <- c(1965, 2)
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues
  )

  # Check the filtered object is ok (should return TRUE)
  expect_equal(myFilteredObject, validateRDBESDataObject(myFilteredObject,
                                                         verbose = FALSE))
  # Check the expected number of DE rows are returned
  expect_equal(nrow(myFilteredObject[["DE"]]), 0)
})
test_that("filterRDBESDataObject returns the correct result for H1 (3)", {
  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
  # Only use a subset of the test data
  myH1RawObject <- filterRDBESDataObject(myH1RawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)
  myFields <- c("DEyear", "DEhierarchy")
  myValues <- c(1965, 1)
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues
  )

  # Check the filtered object is ok (should return TRUE)
  expect_equal(myFilteredObject, validateRDBESDataObject(myFilteredObject,
                                                         verbose = FALSE))
  # Check the expected number of DE rows are returned
  expect_equal(nrow(myFilteredObject[["DE"]]), 3)
})
test_that("filterRDBESDataObject returns the correct result for three fields/
          three values", {
  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
  myFields <- c("DEsampSchemeType", "DEsampScheme", "DEstratumName")
  myValues <- c("NatPilCF", "National Routine", "DE_stratum3_H1")
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues
  )

  # Check the filtered object is ok (should return TRUE)
  expect_equal(myFilteredObject, validateRDBESDataObject(myFilteredObject,
                                                         verbose = FALSE))
  # Check the expected number of DE rows are returned
  expect_equal(nrow(myFilteredObject[["DE"]]), 1)
})
test_that("filterRDBESDataObject returns a warning if incorrect field name is used", {
  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")
  myFields <- c("DEabc")
  myValues <- c("ZWBFO")

  expect_warning(
    filterRDBESDataObject(myH1RawObject,
      fieldsToFilter = myFields,
      valuesToFilter = myValues
    ),
    "The following fields were not found in the RDBESDataObject: DEabc"
  )
})

test_that("filterRDBESDataObject successfully removes orphans", {
  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")

  # remove all the VS rows (but not any other rows)
  myFields <- c("VSunitName")
  myValues <- c("VDcode_8")
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues,
    killOrphans = TRUE
  )

  expect_equal(
    nrow(myFilteredObject$BV),
    2160
  )
  expect_equal(
    nrow(myFilteredObject$FM),
    1080
  )
})

test_that("filterRDBESDataObject does not removes orphans when killOrphans = FALSE", {
  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_1_19_18")

  # remove all the VS rows (but not any other rows)
  myFields <- c("VSunitName")
  myValues <- c("VDcode_8")
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
    fieldsToFilter = myFields,
    valuesToFilter = myValues,
    killOrphans = FALSE
  )

  expect_equal(
    nrow(myFilteredObject$BV),
    nrow(myH1RawObject$BV)
  )
})

test_that("filterRDBESDataObject retains FT if filtering by FO", {
  #issue #183  problem with filtering by FOid but not SSid?
  myH1RawObject <- Pckg_survey_apistrat_H1

  resFO <- filterRDBESDataObject(myH1RawObject, "FOid", 70849,
                                 killOrphans = F)
  expect_equal(nrow(resFO$FO), 1)
  expect_equal(nrow(resFO$FT), 200)

})


test_that("filterRDBESDataObject filter correctly for FT and FO", {
  #issue #183  problem with filtering by FOid but not SSid?
  #the issue was that the "**FOid** field exists in several tables so it is
  #also on **FT** table (although NA) as not used.
  #So filtering it also filtered the FT and hence the strange behaviour.
  #the solution is to remove all the other id fields from the FT table except FTid
  #when filtering. So the current implementation enables to filter on an id field
  #only if it is the id of that table. I think this makes things less confusing.
  #although it is not very flexible as you might want to filter on an id field
  # but this requires explicitly stating on the call the table where the id is expected.
  myH1RawObject <- Pckg_survey_apistrat_H1

  resFO <- filterRDBESDataObject(myH1RawObject, "FOid", 70849,
                                 killOrphans = TRUE)
  expect_equal(nrow(resFO$FO), 1)
  FTid <- resFO$FO$FTid
  resFT <- filterRDBESDataObject(myH1RawObject, "FTid", FTid,
                                 killOrphans = TRUE)
  expect_equal(nrow(resFO$SS), 1)
  expect_equal(resFO$SS, resFT$SS)
})

test_that("filterRDBESDataObject filter correctly for FO and SS", {
  #issue #183  problem with filtering by FOid but not SSid?
  myH1RawObject <- Pckg_survey_apistrat_H1
  values2filter <- c(227694)
  fields2filter <- c("SSid")
  resSS <- filterRDBESDataObject(myH1RawObject, fields2filter, values2filter,
                                 killOrphans = TRUE)
  FOid <- resSS$SS$FOid
  resFO <- filterRDBESDataObject(myH1RawObject, "FOid", FOid,
                                 killOrphans = TRUE)
  expect_equal(nrow(resFO$FO), 1)
  expect_equal(resSS$SS, resFO$SS)
})

test_that("filterRDBESDataObject filter correctly for SS and SA", {
  #issue #183  problem with filtering by FOid but not SSid?
  myH1RawObject <- Pckg_survey_apistrat_H1
  values2filter <- c(227694)
  fields2filter <- c("SSid")
  resSS <- filterRDBESDataObject(myH1RawObject, fields2filter, values2filter,
                                 killOrphans = TRUE)
  SAid <- resSS$SA$SAid
  resSA <- filterRDBESDataObject(myH1RawObject, "SAid", SAid,
                                 killOrphans = TRUE)

  expect_equal(resSS$SA, resSA$SA)
})



}) ## end capture.output
