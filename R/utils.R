# This is a file to store general internal utility functions


# Deal with "no visible binding for global variable.." warnings in R CMD CHECK
usedVars <- c("mapColNamesFieldR",
              "rdbesEstimObj", "..targetProbColumns",
              "..targetProbColumns2", "finalInclusionProb_to_su1",
              "targetValue", "su1unitName", "su1inclusionProb",
              "..target_prob_columns2", "su1selectionProb",
              "..varsNeeded", "|>", "parentTableID", "est.total",
              "recType", "parentTableStratum", "stratumName",
              "parentIDandStratum", "studyVariable", "..myColNames",
              "..methColNames", "tblName", "SLid","SAid",
              "..myLevel","parentTable",".", "id", "i.id" )

moreUsedVars  <- c("..clustFields", "DEyear", "SAcatchCat",
               "SAlandCat", "SAsex", "SAspeCode",
               "SAstratumName", "SDctry", "SDinst",
               "SLcatchFrac", "SLcommTaxon", "SLcou",
               "SLinst", "SLrecType", "SLspeclistName",
               "SLyear", "SScatchFra", "SSctry",
               "SSid", "SSspecListName", "SSuseCalcZero",
               "SSyear", "i.parentIDandStratum", "lowerHierarchy",
               "optional", "sortOrder", "tablesInRDBESHierarchies",
               "tmpKey0", "tmpKey1")

evenMoreUsedVars <- c("Weightg", "Lengthmm", "Group", "WeightIndexSum",
                      "WeightIndex", "H8ExampleEE1", "suLevels", "LengthTotal",
                      "WeightMeasured", "LengthClass", "TotCount",
                      "BVNumbersAtLength", "SAauxVarValue", "ISid",
                      "ISrecType", "..x","n_matches","SAlowHierarchy",
                      "SAparSequNum", "i.SAid", "SAparentID",
                      "BVTotCount", "Age", "BVNumbersAtAge",
                      "i.SAparentID", "i.subSampleLevel", "i.topLevelSAid",
                      "subSampleLevel", "topLevelSAid")

globalVariables(unique(c(usedVars, moreUsedVars, evenMoreUsedVars)))


#' as.integer.or.dbl
#'
#' This function checks if any value in a vector is above 2e+09, and if so runs
#' `round(as.double())` on it. If not it uses `as.integer()` instead. This is to
#' get around the 32-bit limitation in R that integers cannot be larger than
#' around 2e+09, in which case `as.integer` would return an `NA`.
#'
#' @param x vector to be coerced to integers or doubles
#'
#' @return a vector of integers or doubles
#' @importFrom stats na.omit
#' @keywords internal

as.integer.or.dbl <- function(x){
  # we apply as.numeric in case it is a character vector
  # we apply as.omit because that causes an error
  if(any(as.numeric(na.omit(x)) > 2e+09)) out <- round(as.double(x)) else
    out <- as.integer(x)
  return(out)
}


#' Check if a column exists in a data table and has unique values
#'
#' This function checks if a specified column exists in a given data table and has unique values.
#' If the column does not exist or has non-unique values, an error is thrown.
#'
#' @param dt A data table to check
#' @param col A character string specifying the name of the column to check
#' @return  nothing if the column exists and has unique values, otherwise an error is thrown
#' @examples
#' \dontrun{
#'   RDBEScore:::check_key_column(H1Example$DE, "DEid")
#' }
check_key_column <- function(dt, col) {
  # Check if column exists in data table
  if (!col %in% colnames(dt)) {
    stop(paste0(col, " does not exist in the input tables."))
  }
  # Check if column has unique values
  if (length(unique(dt[[col]])) != nrow(dt)) {
    stop(paste0(col, " does not have unique values."))
  }
}

#' fileExt
#'
#' This function splits a character vector and returns the second element if the
#' separator is a dot `.`.
#'
#' @param x character vector to be split.
#'
#' @return the second element of the character vector.
#' @keywords internal

fileExt <- function(x) {
  ext <- strsplit(x, ".", fixed = TRUE)[[1]]
  ext <- ext[length(ext)]
  return(ext)
}


#' is.zip
#'
#' This function evaluates if the file extension is .zip.
#'
#' @param x character vector with the file extension
#'
#' @return logical value
#' @keywords internal
is.zip <- function(x) {
  # let's assume that the file extension is .zip to be on the safe side
  ext <- fileExt(x)
  # the zip starts with a local file header signature 50 4b 03 04
  # see: https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
  b <- readBin(x, "raw", 4)
  if (setequal(as.character(b), c("50", "4b", "03", "04")) &
      ext == "zip") {
    return(TRUE)
  }
  FALSE
}


#' convert.col.names
#'
#' Converts table column names to field name or R name. For now, not actually
#' being used anywhere, but will update in future to be more useful.
#'
#' @param table Table to change names of
#' @param new.names "field.name" or "R.name"
#'
#' @return a vector of strings of new column names
#' @keywords internal

convert.col.names <- function(table, new.names = "R.name"){
  # subset mapColNamesFieldR to appropriate table
  mapColNamesFieldR.sub <- RDBEScore::mapColNamesFieldR[RDBEScore::mapColNamesFieldR$Table.Prefix == table,]

  if(new.names == "R.name") nms.new <- mapColNamesFieldR.sub$R.Name
  if(new.names == "field.name") nms.new <- mapColNamesFieldR.sub$Field.Name

  return(nms.new)
}

#' makeDT
#'
#' Converts one element of a list of data.frames into a data.table, leaving
#' `NULL` elements as `NULL`. Used for each RDBES table in
#' `importRDBESDataDFS()`.
#'
#' @param x A data.frame, or `NULL`.
#'
#' @return A data.table, or `NULL` if `x` is `NULL`.
#' @keywords internal
makeDT <- function(x){
  if(is.null(x)) return(NULL)
  data.table::as.data.table(x)
}


#' extractHigherFields
#'
#' Gets data from higher in the hierarchy for the given input `table`. Only SS
#' explicitly supported for now, but could update in the future to support any
#' table (in theory).
#'
#' At present will not work with data which contains more than one hierarchy.
#' Also bypasses any optional tables (e.g. FT in H5).
#'
#' @param object An RDBESDataObject.
#' @param table The table from which to start and go up the hierarchy
#'   (e.g. `"SS"`).
#' @param field The data (column name) to extract from any other table higher
#'   in the hierarchy, e.g. `"DEyear"` or `"SDctry"`.
#'
#' @return One value per row of `table`, in the same order. Rows that are not
#'   linked to the table holding `field` get `NA` (e.g. orphans left by
#'   filtering DE before `findAndKillOrphans()` runs in
#'   `filterAndTidyRDBESDataObject()`).
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Human review: rix133
#'
#' @examples
#' \dontrun{
#'   RDBEScore:::extractHigherFields(H1Example, "SS", "DEyear")
#' }
#' @keywords internal
extractHigherFields <- function(object, table, field){

  # Check for a single hierarchy
  hierarchy <- unique(object$DE$DEhierarchy)
  if (length(hierarchy) != 1) stop("Multiple Upper Hierarchies found.")

  # path from DE to the input table, without optional and lower hierarchy tables
  path.to.table <- getTablesInRDBESHierarchy(hierarchy,
                                             includeOptTables = FALSE,
                                             includeLowHierTables = FALSE)
  path.to.table <- path.to.table[match("DE", path.to.table):match(table, path.to.table)]
  missingTables <- path.to.table[sapply(object[path.to.table], is.null)]
  if (length(missingTables) > 0) stop("Tables not found in the object: ", paste(missingTables, collapse = ", "))

  # Walk up the path from `table`, following each row to its parent row with
  # match(), until reaching the first table that has the field
  rows <- seq_len(nrow(object[[table]]))
  for (i in rev(seq_along(path.to.table))) {
    tbl <- object[[path.to.table[i]]]
    if (field %in% names(tbl)) return(tbl[[field]][rows])
    if (i == 1) break
    parentId <- paste0(path.to.table[i - 1], "id")
    parentIds <- object[[path.to.table[i - 1]]][[parentId]]
    if (anyDuplicated(parentIds) > 0) stop("Duplicated ", parentId, " values in table ", path.to.table[i - 1])
    rows <- match(tbl[[parentId]][rows], parentIds, incomparables = NA)
  }
  stop("'field' not found in higher tables column names")
}
