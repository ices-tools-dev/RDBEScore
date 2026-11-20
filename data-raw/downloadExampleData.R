#' Download a hierarchy with vessel details and species lists into one ZIP.
#'
#' @param hierarchy Hierarchy number, from 1 to 13.
#' @param year Data year to download.
#' @param country Country code. Defaults to the test country ZW.
#' @param samplingScheme Optional sampling-scheme codes for the CS export.
#' @param ddir Directory for the dated ZIP.
#' @param production Use production RDBES rather than the sandbox.
#' @param includeCLCE Also download commercial landings and effort for the same
#'   country and year. Defaults to FALSE.
#' @return Path to H< hierarchy >_YYYY_MM_DD.zip, dated at download time in Tallinn.
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: rix133
#'
#' @examples
#' # Run manually with your normal icesRDBES authentication:
#' # downloadExampleData(1, 1965,
#' #   samplingScheme = c('WGRDBES-EST TEST 1', 'National Routine'))
downloadExampleData <- function(hierarchy, year, country = 'ZW',
                                 samplingScheme = NULL,
                                 ddir = file.path('data-raw', 'exampleData'),
                                 production = TRUE, includeCLCE = FALSE) {
  stopifnot(length(hierarchy) == 1L, !is.na(hierarchy), hierarchy %in% 1:13)
  dir.create(ddir, recursive = TRUE, showWarnings = FALSE)
  downloadDate <- format(Sys.time(), '%Y_%m_%d', tz = 'Europe/Tallinn')
  output <- file.path(normalizePath(ddir),
                      paste0('H', hierarchy, '_', downloadDate, '.zip'))
  if (file.exists(output)) stop('Download file already exists: ', output)

  payloads <- list(
    CS = list(dataType = 'CS', format = 'CsvFilePerTable',
              hierarchies = list(paste0('H', hierarchy)),
              csFilters = list(deYear = as.list(as.character(year)),
                               sdCountry = as.list(country))),
    VD = list(dataType = 'VD', format = 'CsvFilePerTable', hierarchies = list('HVD'),
              vdFilters = list(vdYear = as.list(as.character(year)), vdCountry = as.list(country))),
    SL = list(dataType = 'SL', format = 'CsvFilePerTable', hierarchies = list('HSL'),
              slFilters = list(slYear = as.list(as.character(year)), slCountry = as.list(country)))
  )
  if (!is.null(samplingScheme)) {
    payloads$CS$csFilters$deSamplingScheme <- as.list(samplingScheme)
  }
  if (includeCLCE) {
    payloads$CL <- list(dataType = 'CL', format = 'CsvFilePerTable', hierarchies = list('HCL'),
                       clFilters = list(clYear = as.list(as.character(year)),
                                        clVesselFlagCountry = as.list(country)))
    payloads$CE <- list(dataType = 'CE', format = 'CsvFilePerTable', hierarchies = list('HCE'),
                       ceFilters = list(ceYear = as.list(as.character(year)),
                                        ceVesselFlagCountry = as.list(country)))
  }
  stageDir <- tempfile('rdbes-example-')
  dir.create(stageDir)
  folders <- c(CS = paste0('H', hierarchy), VD = 'HVD', SL = 'HSL')
  if (includeCLCE) folders <- c(folders, CL = 'HCL', CE = 'HCE')
  for (type in names(payloads)) {
    downloaded <- icesRDBES::rdbes_download_data(
      payloads[[type]], dest_dir = stageDir, production = production
    )
    utils::unzip(downloaded, exdir = file.path(stageDir, folders[[type]]))
  }
  zip::zipr(output, files = unname(folders), root = stageDir)
  message('Saved: ', output)
  output
}

#' Extract an example ZIP and make flat copies of its CSV files.
#' @param x Input ZIP, with flat files or hierarchy subfolders.
#' @param tmp Extraction directory.
#' @return CSV basenames used by the test-fixture preparation.
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
unzipFile <- function(x, tmp) {
  if (!file.exists(x)) stop('Missing example ZIP: ', x)
  extracted <- utils::unzip(x, exdir = tmp)
  for (path in extracted) {
    target <- file.path(tmp, basename(path))
    if (!file.exists(target) && !dir.exists(path)) file.copy(path, target)
  }
  unique(basename(extracted[grepl('\\.csv$', extracted, ignore.case = TRUE)]))
}

#' Create the alternative ZIP layouts used by the H1 import tests.
#' @param zipFile Input example ZIP.
#' @param outputDir Directory for generated test ZIPs.
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
prepareH1ExampleZips <- function(zipFile, outputDir) {
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)
  outputDir <- normalizePath(outputDir)
  tmp <- tempfile('h1-example-zips-')
  dir.create(tmp)
  # Cleanup is restricted to this function's own directory under tempdir().
  tmp <- normalizePath(tmp, winslash = '/')
  if (!startsWith(tmp, paste0(normalizePath(tempdir(), winslash = '/'), '/'))) {
    stop('Unexpected temporary directory: ', tmp)
  }
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  files <- unzipFile(zipFile, tmp)
  groups <- list(HVD = 'VesselDetails.csv',
                 HSL = c('SpeciesList.csv', 'IndividualSpeciesInSpeciesList.csv'))
  groups$H1 <- setdiff(files, unlist(groups, use.names = FALSE))
  for (folder in names(groups)) {
    dir.create(file.path(tmp, folder), showWarnings = FALSE)
    for (file in groups[[folder]]) {
      if (!file.copy(file.path(tmp, file), file.path(tmp, folder, file), overwrite = TRUE)) {
        stop('Could not stage test-fixture file: ', file)
      }
    }
  }
  unlink(file.path(tmp, files))
  zip::zipr(file.path(outputDir, 'HVD_Example.zip'),
             files = list.files(file.path(tmp, 'HVD'), full.names = TRUE), mode = 'cherry-pick')
  zip::zipr(file.path(outputDir, 'HSL_Example.zip'),
             files = list.files(file.path(tmp, 'HSL'), full.names = TRUE), mode = 'cherry-pick')
  zip::zipr(file.path(outputDir, 'H1_Example_fd.zip'),
             files = c('H1', 'HVD', 'HSL', intersect('Disclaimer.txt', list.files(tmp))), root = tmp)
  dir.create(file.path(tmp, 'H2'))
  zip::zipr(file.path(outputDir, 'H1_H2_Example_fd.zip'),
             files = c('H1', 'H2', 'HVD', 'HSL', intersect('Disclaimer.txt', list.files(tmp))), root = tmp)
  invisible(outputDir)
}
