capture.output({

test_that("buildSimpleRDBESDownloadPayload builds CS payloads", {
  payload <- buildSimpleRDBESDownloadPayload(
    datatype = "CS",
    year = 2025,
    country = "EE",
    hierarchy = 8,
    export_format = "CsvFilePerTable"
  )

  expect_equal(payload$dataType, "CS")
  expect_equal(payload$format, "CsvFilePerTable")
  expect_equal(payload$hierarchies, list("H8"))
  expect_equal(payload$csFilters$deYear, list("2025"))
  expect_equal(payload$csFilters$sdCountry, list("EE"))
})

test_that("buildSimpleRDBESDownloadPayload builds CL payloads", {
  payload <- buildSimpleRDBESDownloadPayload(
    datatype = "cl",
    year = "2024",
    country = "DK",
    hierarchy = NULL,
    export_format = "CsvFilePerTable"
  )

  expect_equal(payload$dataType, "CL")
  expect_equal(payload$hierarchies, list("HCL"))
  expect_equal(payload$clFilters$clYear, list("2024"))
  expect_equal(payload$clFilters$clVesselFlagCountry, list("DK"))
})

test_that("buildSimpleRDBESDownloadPayload validates required inputs", {
  expect_error(
    buildSimpleRDBESDownloadPayload(NULL, 2025, "EE", NULL, "CsvFilePerTable"),
    "Provide datatype, year, and country"
  )
  expect_error(
    buildSimpleRDBESDownloadPayload("XX", 2025, "EE", NULL, "CsvFilePerTable"),
    "Unsupported RDBES datatype"
  )
})

test_that("helper functions normalize hierarchy and zip paths", {
  expect_equal(normalizeRDBESHierarchy(8), "H8")
  expect_equal(normalizeRDBESHierarchy("hcl"), "HCL")
  expect_equal(normalizeRDBESZipPath("data", "download"), file.path("data", "download.zip"))
  expect_equal(normalizeRDBESZipPath("data", "download.zip"), file.path("data", "download.zip"))
})

test_that("moveRDBESDownload moves or renames downloaded files", {
  tmp <- tempdir()
  source_file <- tempfile(tmpdir = tmp, fileext = ".zip")
  target_file <- tempfile(tmpdir = tmp, fileext = ".zip")
  writeLines("zip placeholder", source_file)

  moveRDBESDownload(source_file, target_file)

  expect_false(file.exists(source_file))
  expect_true(file.exists(target_file))
  expect_equal(readLines(target_file), "zip placeholder")
})

test_that("downloadRDBESDataZip delegates to rdbes_download_data", {
  downloaded_file <- tempfile(fileext = ".zip")
  calls <- list()
  local_mocked_bindings(
    rdbes_download_data = function(payload, dest_dir, production, verbose) {
      calls[[1]] <<- list(
        payload = payload,
        dest_dir = dest_dir,
        production = production,
        verbose = verbose
      )
      writeLines("downloaded", downloaded_file)
      downloaded_file
    }
  )

  target_dir <- tempfile()
  result <- downloadRDBESDataZip(
    datatype = "CL",
    year = 2025,
    country = "EE",
    dir = target_dir,
    file_name = "custom_name",
    verbose = FALSE
  )

  expect_true(result)
  expect_equal(calls[[1]]$payload$dataType, "CL")
  expect_equal(calls[[1]]$payload$format, "CsvFilePerTable")
  expect_equal(calls[[1]]$dest_dir, target_dir)
  expect_false(calls[[1]]$verbose)
  expect_true(file.exists(file.path(target_dir, "custom_name.zip")))
})

test_that("downloadRDBESDataZip accepts a full payload", {
  downloaded_file <- tempfile(fileext = ".zip")
  custom_payload <- list(
    dataType = "CS",
    format = "CsvFilePerTable",
    hierarchies = list("H8"),
    csFilters = list(deYear = list("2025"), sdCountry = list("EE"))
  )
  calls <- list()
  local_mocked_bindings(
    rdbes_download_data = function(payload, dest_dir, production, verbose) {
      calls[[1]] <<- payload
      writeLines("downloaded", downloaded_file)
      downloaded_file
    }
  )

  target_dir <- tempfile()
  result <- downloadRDBESDataZip(
    payload = custom_payload,
    dir = target_dir,
    file_name = "payload_name",
    verbose = FALSE
  )

  expect_true(result)
  expect_equal(calls[[1]], custom_payload)
  expect_true(file.exists(file.path(target_dir, "payload_name.zip")))
})

})
