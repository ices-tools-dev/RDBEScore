capture.output({  ## suppresses printing of console output when running test()

test_that("findAndKillOrphans runs without errors on an empty RDBESDataObject",  {

  myEmptyObject <- createRDBESDataObject()

  expect_error(findAndKillOrphans(objectToCheck = myEmptyObject,
                                  verbose = FALSE),NA)


})
test_that("findAndKillOrphans runs without errors on an RDBESDataObject with no orphans",  {

  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")

  expect_error(findAndKillOrphans(objectToCheck = myH1RawObject,
                                  verbose = FALSE),NA)

})
test_that("findAndKillOrphans removes orphans on an filtered RDBESDataObject",  {

  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")

  # Only use a subset of the test data
  myH1RawObject <- filterRDBESDataObject(myH1RawObject,c("DEstratumName"),c("DE_stratum1_H1","DE_stratum2_H1","DE_stratum3_H1"))
  myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

  # remove all the VS rows (but not any other rows)
  myFields <- c("VSunitName")
  myValues <- c("blah" )
  myFilteredObject <- filterRDBESDataObject(myH1RawObject,
                                           fieldsToFilter = myFields,
                                           valuesToFilter = myValues )

  # Check the expected number of rows are returned
  expect_equal(nrow(myFilteredObject[["VS"]]),0)
  expect_equal(nrow(myFilteredObject[["FT"]]),243)

  # Remove the orphans
  myObjectNoOrphans <- findAndKillOrphans(objectToCheck = myFilteredObject,
                                          verbose = FALSE)

  # Check the expected number of rows are returned
  expect_equal(nrow(myObjectNoOrphans[["DE"]]),3)
  expect_equal(nrow(myObjectNoOrphans[["SD"]]),3)
  expect_equal(nrow(myObjectNoOrphans[["VS"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["FT"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["FO"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["SS"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["SA"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["FM"]]),0)
  expect_equal(nrow(myObjectNoOrphans[["BV"]]),0)

})

test_that("findAndKillOrphans does not remove upstream rows when lower-level rows are removed", {

  myH1RawObject <- importRDBESDataCSV(rdbesExtractPath = "./h1_v_20250211")

  # Keep a small stable subset first
  myH1RawObject <- filterRDBESDataObject(
    myH1RawObject,
    fieldsToFilter = c("DEstratumName"),
    valuesToFilter = c("DE_stratum1_H1", "DE_stratum2_H1", "DE_stratum3_H1")
  )

  # Start from an object without pre-existing orphans
  myH1RawObject <- findAndKillOrphans(myH1RawObject, verbose = FALSE)

  # Record the upstream state that should remain unchanged
  expectedDEids <- sort(myH1RawObject$DE$DEid)
  expectedSDids <- sort(myH1RawObject$SD$SDid)
  expectedSDtoDE <- myH1RawObject$SD[order(SDid), .(SDid, DEid)]

  # Remove all VS rows only
  myFilteredObject <- filterRDBESDataObject(
    myH1RawObject,
    fieldsToFilter = "VSunitName",
    valuesToFilter = "blah"
  )

  expect_equal(nrow(myFilteredObject[["VS"]]), 0)

  # Kill downstream orphans created by removing VS
  myObjectNoOrphans <- findAndKillOrphans(
    objectToCheck = myFilteredObject,
    verbose = FALSE
  )

  # Upstream tables must be unchanged
  expect_equal(sort(myObjectNoOrphans$DE$DEid), expectedDEids)
  expect_equal(sort(myObjectNoOrphans$SD$SDid), expectedSDids)
  expect_equal(
    myObjectNoOrphans$SD[order(SDid), .(SDid, DEid)],
    expectedSDtoDE
  )

  # Downstream tables should be removed
  expect_equal(nrow(myObjectNoOrphans[["VS"]]), 0)
  expect_equal(nrow(myObjectNoOrphans[["FT"]]), 0)
  expect_equal(nrow(myObjectNoOrphans[["FO"]]), 0)
  expect_equal(nrow(myObjectNoOrphans[["SS"]]), 0)
  expect_equal(nrow(myObjectNoOrphans[["SA"]]), 0)
  expect_equal(nrow(myObjectNoOrphans[["FM"]]), 0)
  expect_equal(nrow(myObjectNoOrphans[["BV"]]), 0)
})

test_that("findAndKillOrphans removes the units below removed TE rows in H8", {

  # Keep the first quarter: 6 of the 11 weeks (TE). The vessels (VS) of the
  # other weeks, and the landings, samples and fish below them, are orphans.
  # Currently fails: findOrphansByTable() only flags a row whose foreign keys
  # ALL point to missing records, and the VS rows also have an SDid whose SD
  # row still exists, so a VS whose TE was removed is not an orphan.
  myObject <- filterRDBESDataObject(H8ExampleEE1, "TEstratumName",
                                    month.name[1:3])
  myObjectNoOrphans <- findAndKillOrphans(myObject, verbose = FALSE)

  expect_true(all(myObjectNoOrphans$VS$TEid %in% myObjectNoOrphans$TE$TEid))
  expect_true(all(myObjectNoOrphans$LE$VSid %in% myObjectNoOrphans$VS$VSid))
  expect_true(all(myObjectNoOrphans$SS$LEid %in% myObjectNoOrphans$LE$LEid))
  expect_true(all(myObjectNoOrphans$SA$SSid %in% myObjectNoOrphans$SS$SSid))
  expect_true(all(myObjectNoOrphans$BV$SAid %in% myObjectNoOrphans$SA$SAid))

  expect_equal(nrow(myObjectNoOrphans$TE), 6)
  expect_equal(nrow(myObjectNoOrphans$VS), 9)
  expect_equal(nrow(myObjectNoOrphans$LE), 9)
  expect_equal(nrow(myObjectNoOrphans$SS), 9)
  expect_equal(nrow(myObjectNoOrphans$SA), 9)
  expect_equal(nrow(myObjectNoOrphans$BV), 2125)
})

}) ## end capture.output
