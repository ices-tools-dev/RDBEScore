#' Title
#'
#' need checks on argument format
#'
#' it seems that datatype = "CS" require a hierarchy value
#'
#' @param datatype one of CS, CL, CE, SL, VD?
#' @param year numeric value for year of data to download (e.g. 2023)
#' @param country 2 letter code for country (e.g. "DK", "FR", "DE", etc.)
#' @param hierarchy numeric value for hierarchy in the range 1-13.
#'  Only required if `datatype` is "CS". If `datatype` is "CS" and `hierarchy`
#'  is not provided, the function will attempt to download the data without
#'  the hierarchy parameter.
#' @param export_format description
#' @param verbose description
#' @param dir description
#' @param file_name (Optional) String. The name to save the downloaded zip file
#'  as, without the .zip extension. If not provided, the file name will be
#'   generated based on the request parameters (e.g. "CS_DK_2023_H1.zip").
#'
#' @return boolean. `TRUE` if the download was successful, `FALSE` otherwise.
#'
#' @importFrom AzureAuth get_azure_token
#' @importFrom httr GET add_headers content status_code http_status parse_url
#'
#' @export
#'
downloadRDBESDataZip <- function(datatype, year, country,
                                 hierarchy = NULL,
                                 export_format = "TableWithIdsFormat",
                                 dir = ".",
                                 file_name=NULL,
                                 verbose = TRUE) {
  # Authenticate and get token
  az <- AzureAuth::get_azure_token(
    resource   = "api://18ab5ebb-1794-4e83-83f1-8fbd3dd5b152/rdbes.api.access",
    tenant     = "e0b220ce-5735-4468-91df-05cae5ff1fdc",
    app        = "b6347a7e-5f73-463a-81b1-3781d163de19",
    version    = 2
  )

  # Extract the access token
  access_token <- az$credentials$access_token

  base_url <- "https://rdbes.ices.dk/api/taf/export/data"

  year_qry <- paste0("?year=", year)
  country_qry <- paste0("&country=", country)
  export_format_qry <- paste0("&exportformat=", export_format)
  datatype_qry <- paste0("&datatype=", datatype)

  mandatory_params <- paste0(year_qry, country_qry, datatype_qry, export_format_qry)

  url <- paste0(base_url, mandatory_params)

  if (datatype == "CS" && !is.null(hierarchy) && hierarchy %in% seq(1, 13)) {
    hierarchy_qry <- paste0("&cshierarchy=", paste0("H",hierarchy))
    url <- paste0(base_url, mandatory_params, hierarchy_qry)
  }

  response <- httr::GET(
    url = url,
    httr::add_headers(Authorization = paste("Bearer", access_token))
  )

  if (!httr::status_code(response) == 200) {
    cat("Failed to download:\n")
    cat("  Status code      :", httr::status_code(response), "\n")
    cat("  Http status      :", httr::http_status(response)$reason, "\n")
    cat("  Detailed message :", httr::content(response, "text"), "\n")
    return(FALSE)
  }

  params_request <- httr::parse_url(response$url)$query
  if (is.null(file_name)) {
    file_name <- paste0(params_request$datatype, "_", params_request$country, "_", params_request$year)
    if ("cshierarchy" %in% names(params_request)) {
      file_name <- paste0(file_name, "_", params_request$cshierarchy)
    }
  }

  saving_file <- file.path(dir, paste0(file_name, ".zip"))

  writeBin(httr::content(response, "raw"), saving_file)

  if (verbose) {
    cat("Downloaded:", saving_file, "\n")
    cat("  Status code      :", httr::status_code(response), "\n")
    cat("  Http status      :", httr::http_status(response)$reason, "\n")
  }

  return(TRUE)
}
