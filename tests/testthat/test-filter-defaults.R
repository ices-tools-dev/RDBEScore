# AI-assisted: Yes
# Review: rix133
# Notes/scope: Synthetic H1 links with explicitly specified expected record ids.

makeFilterDefaultsFixture <- function() {
  myObject <- newRDBESDataObject()
  myTables <- c("DE", "SD", "VS", "FT", "FO", "SS", "SA", "FM", "BV",
                "VD", "SL", "IS")
  # Reuse only the schemas. All values and links below are synthetic.
  for (myTable in myTables) {
    numRows <- if (myTable %in% c("DE", "SD", "VD")) {
      2L
    } else if (myTable %in% c("SL", "IS")) {
      1L
    } else {
      3L
    }
    myObject[[myTable]] <- data.table::copy(
      H1Example[[myTable]][rep(NA_integer_, numRows)])
    data.table::set(myObject[[myTable]], j = paste0(myTable, "id"),
                    value = seq_len(numRows))
  }
  myObject$DE[, `:=`(DEhierarchy = 1L, DEyear = 2025L)]
  myObject$SD[, `:=`(DEid = 1:2, SDctry = "EE")]
  myObject$VS[, `:=`(SDid = c(1L, 2L, 1L), VDid = c(1L, 2L, 99L))]
  myObject$FT[, VSid := 1:3]
  myObject$FO[, FTid := 1:3]
  myObject$SS[, `:=`(FOid = 1:3,
                     SSspecListName = c("test-list", "test-list", "missing-list"),
                     SScatchFra = "Lan")]
  myObject$SA[, `:=`(SSid = 1:3, SAseqNum = 1:3, SAparSequNum = 0L)]
  myObject$FM[, SAid := 1:3]
  myObject$BV[, FMid := 1:3]
  myObject$SL[, `:=`(SLspeclistName = "test-list", SLcatchFrac = "Lan",
                     SLyear = 2025L, SLcou = "EE")]
  myObject$IS[, SLid := 1L]
  for (myTable in c("FT", "FO", "SA")) {
    if ("VDid" %in% names(myObject[[myTable]])) {
      data.table::set(myObject[[myTable]], j = "VDid", value = c(1L, 2L, 99L))
    }
  }
  for (myTable in myTables) {
    data.table::setkeyv(myObject[[myTable]], paste0(myTable, "id"))
  }
  myObject
}

test_that("filtering removes downstream orphans by default without changing input", {
  myObject <- makeFilterDefaultsFixture()
  myOriginal <- data.table::copy(myObject)
  expect_error(validateRDBESDataObject(myObject), NA)

  myResult <- filterRDBESDataObject(myObject, "DEid", 1L)

  expect_s3_class(myResult, "RDBESDataObject")
  expect_equal(myResult$DE$DEid, 1L)
  expect_equal(myResult$SD$SDid, 1L)
  for (myTable in c("VS", "FT", "FO", "SS", "SA", "FM", "BV")) {
    expect_equal(myResult[[myTable]][[paste0(myTable, "id")]], c(1L, 3L))
  }
  # Ordinary orphan removal leaves the broken VD and species-list links alone.
  expect_equal(myResult$VS$VDid, c(1L, 99L))
  expect_equal(myResult$SS$SSspecListName, c("test-list", "missing-list"))
  expect_equal(myResult$VD, myOriginal$VD)
  expect_equal(myResult$SL, myOriginal$SL)
  expect_equal(myObject, myOriginal)
})

test_that("killOrphans FALSE retains records downstream of filtered designs", {
  myObject <- makeFilterDefaultsFixture()
  myResult <- filterRDBESDataObject(myObject, "DEid", 1L, killOrphans = FALSE)

  expect_equal(myResult$DE$DEid, 1L)
  for (myTable in setdiff(names(myObject), "DE")) {
    expect_equal(myResult[[myTable]], myObject[[myTable]])
  }
})

test_that("filtering a child does not remove upstream records", {
  myObject <- makeFilterDefaultsFixture()
  myResult <- filterRDBESDataObject(myObject, "VSid", 1L)

  expect_equal(myResult$DE, myObject$DE)
  expect_equal(myResult$SD, myObject$SD)
  for (myTable in c("VS", "FT", "FO", "SS", "SA", "FM", "BV")) {
    expect_equal(myResult[[myTable]][[paste0(myTable, "id")]], 1L)
  }
})

test_that("filtering VD or SL does not silently clean their special links", {
  myObject <- makeFilterDefaultsFixture()
  myVessels <- filterRDBESDataObject(myObject, "VDid", 1L)
  expect_equal(myVessels$VD$VDid, 1L)
  expect_equal(myVessels$VS, myObject$VS)

  mySpecies <- filterRDBESDataObject(myObject, "SLspeclistName", "absent-list")
  expect_equal(nrow(mySpecies$SL), 0L)
  expect_equal(mySpecies$IS, myObject$IS)
  expect_equal(mySpecies$SS, myObject$SS)
})

test_that("the deprecated wrapper warns and retains full cleanup", {
  myObject <- makeFilterDefaultsFixture()
  myOriginal <- data.table::copy(myObject)
  myResult <- expect_warning(
    filterAndTidyRDBESDataObject(myObject, "DEid", 1L),
    "filterAndTidyRDBESDataObject.*deprecated", class = "deprecatedWarning")

  for (myTable in c("DE", "SD", "VS", "FT", "FO", "SS", "SA", "FM", "BV")) {
    expect_equal(myResult[[myTable]][[paste0(myTable, "id")]], 1L)
  }
  expect_equal(myObject, myOriginal)
})

test_that("the deprecated wrapper honors killOrphans FALSE", {
  myObject <- makeFilterDefaultsFixture()
  myResult <- expect_warning(
    filterAndTidyRDBESDataObject(myObject, "VSid", 1L, killOrphans = FALSE),
    class = "deprecatedWarning")

  expect_equal(myResult$VS$VSid, 1L)
  expect_equal(myResult$FT$FTid, 1:2)
  expect_equal(myResult$FO$FOid, 1:3)
  expect_equal(myResult$SS$SSid, 1L)
  expect_equal(myResult$SA$SAid, 1:3)
  expect_equal(myResult$FM$FMid, 1:3)
  expect_equal(myResult$BV$BVid, 1:3)
})

test_that("the deprecated wrapper still permits cleanup without filtering", {
  myResult <- expect_warning(
    filterAndTidyRDBESDataObject(makeFilterDefaultsFixture()),
    class = "deprecatedWarning")

  for (myTable in c("DE", "SD", "VS", "FT", "FO", "SS", "SA", "FM", "BV")) {
    expect_equal(myResult[[myTable]][[paste0(myTable, "id")]], 1:2)
  }
})
