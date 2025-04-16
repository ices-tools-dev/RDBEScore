#' Returns the tables for a given hierarchy
#'
#' @param hierarchy Integer value between 1 and 13 inclusive
#' @param includeOptTables Include any optional tables?  Default value is
#'  TRUE
#' @param includeLowHierTables Include the lower hierarchy tables?
#' Default value is TRUE
#' @param includeTablesNotInSampHier Include tables that aren't
#' sampling units in that hierarcy?  Default value is TRUE
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#'
#' @return A vector containing the 2-letter names of the tables in the
#' requested hierarchy
#' @export
#'
#' @examples
#' getTablesInRDBESHierarchy(5)
getTablesInRDBESHierarchy <- function(hierarchy,
                                      includeOptTables = TRUE,
                                      includeLowHierTables = TRUE,
                                      includeTablesNotInSampHier = TRUE,
                                      verbose = FALSE) {
  if (!is.numeric(hierarchy)) {
    stop("hierarchy parameter must be an integer")
  }

  # Can't just use is.integer() - that only checks if the number is explicitly
  # stored as an integer
  if (!as.integer(hierarchy) == hierarchy) {
    stop("hierarchy parameter must be an integer")
  }

  if (hierarchy < 1 | hierarchy > 13) {
    stop("hierarchy parameter must be between 1 and 13")
  }

  # Make sure we have an int
  hierarchy <- as.integer(hierarchy)
  hierarchyName <- paste0("H", hierarchy)
  hierarchyData <- RDBEScore::tablesInRDBESHierarchies
  hierarchyData <- hierarchyData[hierarchyData$hierarchy == hierarchyName, ]
  hierarchyData <- hierarchyData[order(hierarchyData$sortOrder), ]


  if (!includeOptTables) {
    hierarchyData <- hierarchyData[!hierarchyData$optional, ]
  }

  if (!includeLowHierTables) {
    hierarchyData <- hierarchyData[!hierarchyData$lowerHierarchy, ]
  }

  if (!includeTablesNotInSampHier) {
    hierarchyData <- hierarchyData[hierarchyData$samplingUnit, ]
  }


  tablesToReturn <- hierarchyData$table

  tablesToReturn
}
