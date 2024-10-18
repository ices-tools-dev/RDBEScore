#' Add CL data to a lower-level CS table in an RDBESDataObject
#'
#' This function adds data from a `CL` table in an `RDBESDataObject` to a
#' BV or FM table. It combines information from the `CS` and `CL` tables and
#' calculates aggregate statistics such as the sum of the specified fields in the `CL` table.
#'
#' @param rdbes An object of class `RDBESDataObject`. This object contains tables from the RDBES
#'   data structure, including a `CL` table and `CS` data.
#' @param strataListCS A named list of filter criteria for subsetting the `CS` data in the `RDBESDataObject`.
#'   The names of the list should match column names in any of the `CS` tables.
#' @param strataListCL A named list of filter criteria for subsetting the `CL` data in the `RDBESDataObject`.
#'   The names of the list should match column names in the `CL` table.
#' @param combineStrata Logical, if `TRUE`, strata in the `strataListCS` are combined using a vertical bar (`|`).
#' @param lowerHierarchy A character string specifying the level of the lower hierarchy table to which the CL data will be added.
#'   Currently, only "C" is supported ie BV data only.
#' @param CLfields A character vector of field names from the `CL` table that will be summed and added as new columns to the lower-level biological data.
#' @param verbose Logical, if `TRUE`, the function prints informative text.
#'
#' @return A data.table containing the biological data from the lower hierarchy with added strata information from the `CL` table and
#'   the sum of the specified fields from the `CL` data.
#'
#' @details The function first subsets the biological data in the `RDBESDataObject` based on the criteria in `strataListCS`. It then retrieves
#'   the corresponding `CL` data based on the criteria in `strataListCL`, sums the fields specified in `CLfields`, and adds them as new columns
#'   to the biological data. If `combineStrata` is `TRUE`, strata columns from the `CS` data are collapsed using a vertical bar (`|`). The function
#'   currently supports only biological data at the "C" hierarchy level.
#'
#' @examples
#' \dontrun{
#' strataListCS <- list(LEarea="27.3.d.28.1",
#'                      LEmetier6 = "OTM_SPF_16-31_0_0",
#'                      TEstratumName = month.name[1:3],
#'                      SAspeCodeFAO = "SPR")
#' strataListCL <- list(CLarea="27.3.d.28.1",
#'                      CLquar = 1,
#'                      CLmetier6 = "OTM_SPF_16-31_0_0",
#'                      CLspecFAO = "SPR")
#' biolCL <- addCLtoLowerCS(rdbesObject, strataListCS, strataListCL,
#'                          combineStrata = TRUE,
#'                          lowerHierarchy = "C",
#'                          CLfields = c("CLoffWeight"))
#' }
#'
#' @seealso \code{\link{getLowerTableSubsets}}, \code{\link{upperTblData}}
#' @export
addCLtoLowerCS <- function(rdbes, strataListCS, strataListCL, combineStrata =T, lowerHierarchy = "C", CLfields = c("CLoffWeight"), verbose = FALSE){

  # Function to subset data.table based on criteria list
  subset_dt <- function(dt, criteria) {
    for (field in names(criteria)) {
      dt <- dt[get(field) == criteria[[field]], ]
    }
    return(dt)
  }

  #check if the CL in rdbes is not NULL
  if(is.null(rdbes$CL)){ stop("No CL data in the input RDBESDataObject") }

  if(lowerHierarchy  %in% c("A", "B", "D")){
    stop("Lower hierarchy ", lowerHierarchy, " not yet supported")
  }

  if(lowerHierarchy == "C"){
    tblName <- "BV"
    tblId <- "BVid"

  } else{
    stop("Lower hierarchy ", lowerHierarchy, "  is invalid!")
  }

  #get the sampling data
  biolData <- getLowerTableSubsets(strataListCS, tblName, rdbes, combineStrata = combineStrata)


  #get the CL data
  CL <- subset_dt(rdbes$CL, strataListCL)

  #add the CL fields to the lower table
  strataList <- lapply(strataListCL, function(x) {
    x_new <- ifelse(length(x) == 1, x, paste0(x, collapse = "|"))
    rep(x_new, nrow(biolData))
    })

  strataList <- data.table::as.data.table(strataList)
  biolData <- cbind(biolData, strataList)

  for(field in CLfields){
    # Calculate the sum of the specified field in the CL table
    sum_value <- sum(as.numeric(CL[[field]]), na.rm = TRUE)
    # Add the sum as a new column to biolData
    biolData[, paste0("sum", field) := sum_value]

  }

  return(biolData)
}

