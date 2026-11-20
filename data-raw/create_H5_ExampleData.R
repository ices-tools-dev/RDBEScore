# AI-assisted: Yes
# Review: pending
source('data-raw/downloadExampleData.R')
source('data-raw/prepareDownloadedExample.R')
## Load the csv files from our data-raw
ddir <- "./data-raw/exampleData/"
dataDate <- '2026_10_15'
# Change dataDate to today's YYYY_MM_DD to refresh CS, VD, SL, CL and CE.
zipFile <- file.path(ddir, 'H1-H5-roundtrip', paste0('H5_', dataDate, '.zip'))
if (!file.exists(zipFile)) {
  if (dataDate != format(Sys.time(), '%Y_%m_%d', tz = 'Europe/Tallinn')) {
    stop('Missing historical ZIP. Set dataDate to today to download current data.')
  }
  source('data-raw/downloadExampleData.R')
  zipFile <- downloadExampleData(5, 1965, samplingScheme = 'WGRDBES-EST TEST 1',
                                  ddir = dirname(zipFile), includeCLCE = TRUE)
}

H5Example <- createRDBESDataObject(zipFile,
                                       castToCorrectDataTypes = TRUE)
original <- RDBEScore::createRDBESDataObject(file.path(ddir, 'H5_2025_02_11.zip'))
H5Example <- prepareDownloadedExample(H5Example, original)
validateRDBESDataObject(H5Example)

# Save the data
usethis::use_data(H5Example, overwrite = TRUE)

outputDir <- file.path('tests', 'testthat', paste0('h5_v_', gsub('_', '', dataDate)))
dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
writeExampleFixtures(H5Example, 5, outputDir)
