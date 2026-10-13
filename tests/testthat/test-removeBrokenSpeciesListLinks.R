# AI-assisted: Yes
# Review: rix133
# Notes/scope: Issue #269 imported H1 data, NULL/empty SL, and a synthetic data
capture.output({  ## suppresses printing of console output when running test()

test_that("removeBrokenSpeciesListLinks runs without errors
          or warnings on imported H1 data",  {

    myPath <- "./h1_v_20250211"
    myH1RawObject <-
    importRDBESDataCSV(rdbesExtractPath = myPath)

    expect_warning(
      removeBrokenSpeciesListLinks(objectToCheck = myH1RawObject,
                                   verbose = FALSE), NA)
})

test_that("removeBrokenSpeciesListLinks produces an error
          if the SL table is NULL",  {

    myPath <- "./h1_v_20250211"
    myH1RawObject <-
    importRDBESDataCSV(rdbesExtractPath = myPath)
    myH1RawObject["SL"] <- list(NULL)

    expect_error(
    removeBrokenSpeciesListLinks(objectToCheck = myH1RawObject,
                                           verbose = FALSE)
              ,"The SL entry in in objectToCheck is null - cannot check for broken species list links")

})
test_that("removeBrokenSpeciesListLinks removes all SS rows
          without warnings when SL is empty",  {

    myPath <- "./h1_v_20250211"
    myH1RawObject <-
      importRDBESDataCSV(rdbesExtractPath = myPath)
    myFields <- c("SLspeclistName")
    myValues <- c("WGRDBES-EST TEST 5 - sprat data" )
    myFilteredObject <- filterRDBESDataObject(myH1RawObject,
                                               fieldsToFilter = myFields,
                                               valuesToFilter = myValues )
    expect_equal(nrow(myFilteredObject$SL), 0L)
    myExpectedObject <- data.table::copy(myFilteredObject)
    myExpectedObject$SS <- myExpectedObject$SS[0]

    myObjectValidSpeciesListLinks <- expect_warning(
        removeBrokenSpeciesListLinks(objectToCheck = myFilteredObject
                                     ,verbose = FALSE)
                                    ,NA)
    expect_equal(myObjectValidSpeciesListLinks, myExpectedObject)
})

#' Build a synthetic H1 fixture with one valid and one broken species-list link.
#' @return An RDBESDataObject with one SL record and two SS rows.
# AI-assisted: Yes
# Review: pending human review
makeSpeciesListLinksFixture <- function() {
    myObject <- newRDBESDataObject()
    myTables <- c("DE", "SD", "VS", "FT", "FO", "SS", "SL", "IS")
    # Reuse column schemas only; all fixture values are synthetic.
    for (myTable in myTables) {
      myObject[[myTable]] <- data.table::copy(H1Example[[myTable]][NA_integer_])
    }

    myObject$DE[, `:=`(DEid = 1L, DEhierarchy = 1L, DEyear = 2025L)]
    myObject$SD[, `:=`(SDid = 1L, DEid = 1L, SDctry = "EE")]
    myObject$VS[, `:=`(VSid = 1L, SDid = 1L)]
    myObject$FT[, `:=`(FTid = 1L, VSid = 1L)]
    myObject$FO[, `:=`(FOid = 1L, FTid = 1L)]
    myObject$SS <- myObject$SS[c(1L, 1L)]
    myObject$SS[, `:=`(SSid = 1:2, FOid = 1L,
                      SSspecListName = c("test-list", "missing-list"),
                      SScatchFra = "Lan")]
    myObject$SL[, `:=`(SLid = 1L, SLspeclistName = "test-list",
                      SLcatchFrac = "Lan", SLyear = 2025L, SLcou = "EE")]
    # Validation requires a non-empty IS table whenever SL is non-empty.
    myObject$IS[, `:=`(ISid = 1L, SLid = 1L)]
    for (myTable in myTables) {
      data.table::setkeyv(myObject[[myTable]], paste0(myTable, "id"))
    }

    myObject
}

test_that("removeBrokenSpeciesListLinks keeps a valid SS row
          and removes a broken species-list link", {

    myObject <- makeSpeciesListLinksFixture()
    myOriginalObject <- data.table::copy(myObject)
    myExpectedObject <- data.table::copy(myObject)
    myExpectedObject$SS <- myExpectedObject$SS[SSid == 1L]

    myObjectValidSpeciesListLinks <- expect_warning(
      removeBrokenSpeciesListLinks(objectToCheck = myObject,
                                   verbose = FALSE), NA)

    expect_equal(myObjectValidSpeciesListLinks, myExpectedObject)
    expect_equal(myObject, myOriginalObject)
})

test_that("removeBrokenSpeciesListLinks uses SSid when SS columns are reordered", {

    myObject <- makeSpeciesListLinksFixture()
    # Both rows use the same list name; the second has no matching catch fraction.
    myObject$SS[SSid == 2L, `:=`(SSspecListName = "test-list",
                                SScatchFra = "Dis")]
    data.table::setcolorder(myObject$SS,
      c("SSspecListName", setdiff(names(myObject$SS), "SSspecListName")))

    # Column order does not make the input invalid.
    expect_error(validateRDBESDataObject(myObject), NA)
    myExpectedObject <- data.table::copy(myObject)
    myExpectedObject$SS <- myExpectedObject$SS[SSid == 1L]

    myObjectValidSpeciesListLinks <- expect_warning(
      removeBrokenSpeciesListLinks(objectToCheck = myObject,
                                   verbose = FALSE), NA)

    expect_equal(myObjectValidSpeciesListLinks, myExpectedObject)
})

}) ## end capture.output
