# AI-assisted: Yes
# Review: pending
source('data-raw/downloadExampleData.R')
source('data-raw/prepareDownloadedExample.R')
ddir <- './data-raw/exampleData/'

dataDate <- '2026_10_15'
# Change dataDate to today's YYYY_MM_DD to download a fresh combined export.
zipFile <- file.path(ddir, 'H1-H5-roundtrip', paste0('H1_', dataDate, '.zip'))
if (!file.exists(zipFile)) {
  if (dataDate != format(Sys.time(), '%Y_%m_%d', tz = 'Europe/Tallinn')) {
    stop('Missing historical ZIP. Set dataDate to today to download current data.')
  }
  zipFile <- downloadExampleData(1, 1965,
    samplingScheme = 'WGRDBES-EST TEST 1', ddir = dirname(zipFile))
}

H1_Example <- RDBEScore::createRDBESDataObject(zipFile, castToCorrectDataTypes = TRUE)
original <- RDBEScore::createRDBESDataObject(file.path(ddir, 'H1_2025_02_11.zip'))
H1_Example <- prepareDownloadedExample(H1_Example, original)
RDBEScore::validateRDBESDataObject(H1_Example)
H1Example <- H1_Example
usethis::use_data(H1Example, overwrite = TRUE)

outputDir <- file.path('tests', 'testthat', paste0('h1_v_', gsub('_', '', dataDate)))
dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
writeExampleFixtures(H1Example, 1, outputDir)
