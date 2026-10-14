#' Retrieve Linked Data Between RDBES Tables at a Specified Level
#'
#' The `getLinkedDataFromLevel` function facilitates the retrieval of linked data between different levels of RDBES tables. Depending on the relative positions of the source and target tables within the `RDBESDataObject`, the function determines whether to traverse "up" or "down" the data hierarchy to obtain the desired linked data.
#'
#' @param field A character string specifying the field name from which to retrieve linked data. The first two characters of this field indicate the source table.
#' @param values A vector of values corresponding to the specified `field` for which linked data is to be retrieved.
#' @param rdbesTables An `RDBESDataObject` containing the relevant RDBES tables. This object should include all tables that may be linked based on the provided `field` and `level`.
#' @param level A character string specifying the target table level from which to retrieve linked data. This must be one of the names within the `rdbesTables` object.
#' @param verbose Logical flag indicating whether to print detailed information about the data retrieval process. Default is `FALSE`.
#'
#' @return A data.table containing the requested `field` and all columns of the
#'   table at `level`, with one row per unique requested value and linked record.
#'   Intermediate hierarchy IDs are not added. Column order is not guaranteed.
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: rix133
#' - Notes/scope: examples only
#'
#' @export
#'
#' @examples
#' H8DataObj <- H8ExampleEE1
#' # Go up from a biological measurement to its design
#' linkedDesign <- getLinkedDataFromLevel("BVid", 1, H8DataObj, "DE")
#' linkedDesign[, c("BVid", "DEid", "DEstratumName")]
#'
#' # Go down from a design to its samples
#' linkedSamples <- getLinkedDataFromLevel("DEid", 1, H8DataObj, "SA")
#' head(linkedSamples[, c("DEid", "SAid", "SAspeCode")])
#'
#' # Link a fish identifier to its vessel selection record
#' linkedVessels <- getLinkedDataFromLevel("BVfishId", H8DataObj$BV$BVfishId[1],
#'                                        H8DataObj, "VS")
#' linkedVessels[, c("BVfishId", "VSid", "VDid")]
#'
getLinkedDataFromLevel <- function(field, values, rdbesTables, level, verbose = FALSE){

  # Check if rdbesTables is of class "RDBESDataObject"
  if(inherits(rdbesTables, "RDBESDataObject")) {
    availableTblNames <- names(rdbesTables)
    rdbesTables <- sort(rdbesTables)
  } else {
    stop("rdbesTables must be of class RDBESDataObject")
  }

  # Check if the specified level exists within rdbesTables
  if(!level %in% names(rdbesTables)){
    stop("Table ", level, " not found in the RDBESDataObject")
  }

  # Extract the source table abbreviation from the field name
  sourceTbl <- substr(field, 1, 2)

  #get the hierarchy of the source table
  summary_values <- summary(rdbesTables)
  hierarchy <- NA
  if(!is.null(summary_values$hierarchy)){
    if(length(summary_values$hierarchy)>1){
      stop("Mixed hierarchy RDBESDataObject!", call.=FALSE)
    }
    hierarchy <- summary_values$hierarchy
  } else {
    stop(paste0("No hierarchy, RDBESdataObject:\n "))
  }

  # Get the list of table names with optional if needed
  tblNames <- getTablesInRDBESHierarchy(hierarchy)
  requiredTblNames <- getTablesInRDBESHierarchy(
    hierarchy,
    includeOptTables = FALSE
  )

  keepTblNames <- c(requiredTblNames, sourceTbl, level)
  tblNames <- tblNames[tblNames %in% keepTblNames]

  #stop if tables in the data are missing required tables
  missingTbls <- requiredTblNames[!requiredTblNames %in% availableTblNames]
  if(length(missingTbls) > 0){
    stop(paste0("Missing required tables in the RDBESDataObject: ",
                paste(missingTbls, collapse = ", ")))
  }

  # Determine the positions of sourceTbl and target level in the table list
  sourcePos <- which(tblNames == sourceTbl)
  targetPos <- which(tblNames == level)

  # Determine traversal direction based on table positions
  res <- NULL
  if(sourcePos > targetPos){
    if(verbose){
      cat("Traversing upwards in the table hierarchy from", sourceTbl, "to", level, "\n")
    }
    res <- upperTblData(field, values, rdbesTables, level, verbose)
  } else {
    if(verbose){
      cat("Traversing downwards in the table hierarchy from", sourceTbl, "to", level, "\n")
    }
    res <- lowerTblData(field, values, rdbesTables, level, verbose)
  }

  return(res)
}
