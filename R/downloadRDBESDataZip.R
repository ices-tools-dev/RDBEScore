#' Title
#'
#' need checks on argument format
#'
#' it seems that datatype = "CS" require a hierarchy value
#'
#' @param datatype description
#' @param year description
#' @param hierarchy description
#' @param country description
#' @param export_format description
#' @param verbose description
#' @param dir description
#'
#' @return result
#'
#' @importFrom AzureAuth get_azure_token
#' @importFrom httr GET add_headers content status_code http_status parse_url
#'
#' @export
#'
downloadRDBESDataZip <- function(datatype = c("CE", "CL", "VD", "SL"), year, hierarchy = NULL, country = "FR", export_format = "TableWithIdsFormat", dir = ".", verbose = TRUE) {
  # Authenticate and get token
  az <- get_azure_token(
    resource   = "api://18ab5ebb-1794-4e83-83f1-8fbd3dd5b152/rdbes.api.access",
    tenant     = "e0b220ce-5735-4468-91df-05cae5ff1fdc",
    app        = "b6347a7e-5f73-463a-81b1-3781d163de19",
    version    = 2
  )

  # Extract the access token
  access_token <- az$credentials$access_token

  base_url <- "https://sboxrdbes.ices.dk/api/taf/export/data"

  year_qry <- paste0("?year=", year)
  country_qry <- paste0("&country=", country)
  export_format_qry <- paste0("&exportformat=", export_format)
  datatype_qry <- paste0("&datatype=", datatype)

  mandatory_params <- paste0(year_qry, country_qry, datatype_qry, export_format_qry)

  url <- paste0(base_url, mandatory_params)

  if (datatype == "CS" && !is.null(hierarchy) && hierarchy %in% paste0("H", seq(1, 13))) {
    hierarchy_qry <- paste0("&cshierarchy=", hierarchy)
    url <- paste0(base_url, mandatory_params, hierarchy_qry)
  }

  response <- GET(
    url = url,
    add_headers(Authorization = paste("Bearer", access_token))
  )

  if (!status_code(response) == 200) {
    cat("Failed to download:\n")
    cat("  Status code      :", status_code(response), "\n")
    cat("  Http status      :", http_status(response)$reason, "\n")
    cat("  Detailed message :", content(response, "text"), "\n")
    return(FALSE)
  }

  params_request <- parse_url(response$url)$query
  if (is.null(file_name)) {
    file_name <- paste0(params_request$datatype, "_", params_request$country, "_", params_request$year)
    if ("cshierarchy" %in% names(params_request)) {
      file_name <- paste0(file_name, "_", params_request$cshierarchy)
    }
  }

  saving_file <- file.path(dir, paste0(file_name, ".zip"))

  writeBin(content(response, "raw"), saving_file)

  if (verbose) {
    cat("Downloaded:", saving_file, "\n")
    cat("  Status code      :", status_code(response), "\n")
    cat("  Http status      :", http_status(response)$reason, "\n")
  }

  return(saving_file)
}
