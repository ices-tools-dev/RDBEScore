#' Download RDBES data as a zip file
#'
#' Downloads RDBES data through `icesRDBES::rdbes_download_data()`.
#'
#' @param datatype One of `"CS"`, `"CL"`, `"CE"`, `"SL"`, or `"VD"`.
#' @param year Numeric value for year of data to download.
#' @param country Two-letter country code.
#' @param hierarchy Optional CS or CL hierarchy, for example `"H8"` or `8`.
#' @param dir Directory where the zip file is saved.
#' @param file_name Optional file name to save the downloaded zip file as. The
#'  `.zip` suffix is added if omitted.
#' @param verbose Passed to `icesRDBES::rdbes_download_data()`.
#' @param payload Optional full RDBES download payload. If supplied, `datatype`,
#'  `year`, `country` and `hierarchy` are ignored.
#'
#' @return Logical `TRUE` if the download completed.
#'
#' @importFrom icesRDBES rdbes_download_data
#'
#' @export
#'
downloadRDBESDataZip <- function(datatype = NULL, year = NULL, country = NULL,
                                 hierarchy = NULL,
                                 dir = ".",
                                 file_name = NULL,
                                 verbose = TRUE,
                                 payload = NULL) {
  if (is.null(payload)) {
    payload <- buildSimpleRDBESDownloadPayload(
      datatype = datatype,
      year = year,
      country = country,
      hierarchy = hierarchy,
      export_format = "CsvFilePerTable"
    )
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  downloaded_file <- rdbes_download_data(
    payload = payload,
    dest_dir = dir,
    production = getOption("rdbes.production", TRUE),
    verbose = verbose
  )

  if (is.null(file_name) && !is.null(datatype) && !is.null(year) &&
      !is.null(country)) {
    file_name <- paste0(datatype, "_", country, "_", year)
    if (!is.null(hierarchy)) {
      file_name <- paste0(file_name, "_", normalizeRDBESHierarchy(hierarchy))
    }
  }

  if (!is.null(file_name)) {
    target_file <- normalizeRDBESZipPath(dir, file_name)
    moveRDBESDownload(downloaded_file, target_file)
  }

  TRUE
}

buildSimpleRDBESDownloadPayload <- function(datatype, year, country, hierarchy,
                                            export_format) {
  if (is.null(datatype) || is.null(year) || is.null(country)) {
    stop("Provide datatype, year, and country, or pass a full payload.")
  }

  datatype <- toupper(datatype)
  filter_name <- switch(datatype,
                        CS = "csFilters",
                        CL = "clFilters",
                        CE = "ceFilters",
                        SL = "slFilters",
                        VD = "vdFilters",
                        NULL)
  if (is.null(filter_name)) {
    stop("Unsupported RDBES datatype: ", datatype)
  }

  payload <- list(dataType = datatype, format = export_format)

  if (!is.null(hierarchy)) {
    payload$hierarchies <- rdbesPayloadValues(
      normalizeRDBESHierarchy(hierarchy)
    )
  } else if (datatype != "CS") {
    payload$hierarchies <- rdbesPayloadValues(paste0("H", datatype))
  }

  year_field <- switch(datatype,
                       CS = "deYear",
                       CL = "clYear",
                       CE = "ceYear",
                       SL = "slYear",
                       VD = "vdYear")
  country_field <- switch(datatype,
                          CS = "sdCountry",
                          CL = "clVesselFlagCountry",
                          CE = "ceVesselFlagCountry",
                          SL = "slCountry",
                          VD = "vdCountry")

  payload[[filter_name]] <- list()
  payload[[filter_name]][[year_field]] <- rdbesPayloadValues(year)
  payload[[filter_name]][[country_field]] <- rdbesPayloadValues(country)

  payload
}

rdbesPayloadValues <- function(values) {
  as.list(as.character(values))
}

normalizeRDBESHierarchy <- function(hierarchy) {
  hierarchy <- as.character(hierarchy)
  ifelse(grepl("^H", hierarchy, ignore.case = TRUE),
         toupper(hierarchy),
         paste0("H", hierarchy))
}

normalizeRDBESZipPath <- function(dir, file_name) {
  if (!grepl("\\.zip$", file_name, ignore.case = TRUE)) {
    file_name <- paste0(file_name, ".zip")
  }

  file.path(dir, file_name)
}

moveRDBESDownload <- function(downloaded_file, target_file) {
  if (normalizePath(downloaded_file, winslash = "/", mustWork = FALSE) ==
      normalizePath(target_file, winslash = "/", mustWork = FALSE)) {
    return(invisible(target_file))
  }

  if (file.exists(target_file)) {
    unlink(target_file)
  }

  ok <- file.rename(downloaded_file, target_file)
  if (!ok) {
    ok <- file.copy(downloaded_file, target_file, overwrite = TRUE)
    if (ok) {
      unlink(downloaded_file)
    }
  }
  if (!ok) {
    stop("Could not move downloaded RDBES zip to ", target_file)
  }

  invisible(target_file)
}
