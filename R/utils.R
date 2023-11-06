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
              "..methColNames", "tblName", "all_of", "SLid","SAid",
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

globalVariables(unique(c(usedVars, moreUsedVars)))


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
  mapColNamesFieldR.sub <- mapColNamesFieldR[mapColNamesFieldR$Table.Prefix == table,]

  if(new.names == "R.name") nms.new <- mapColNamesFieldR.sub$R.Name
  if(new.names == "field.name") nms.new <- mapColNamesFieldR.sub$Field.Name

  return(nms.new)
}

# Convert all elements of a list of data.frames into data.tables
# Leaves existing NULL elements as NULL
makeDT <- function(x){
  if(is.null(x)) return(NULL)
  data.table::as.data.table(x)
}


# Gets data from higher in the hierarchy for the given input `table`. Only SS
# explicitly supported for now, but could update in the future to support any
# table (in theory).
#
# `object` = an RDBESDataObject
#
# `table` = The table from which to start and go up the hierarchy (e.g. `"SS"`)
#
# `field` = The data (column name) to extract from any other table higher in the
# hierarchy, e.g. `"DEyear"` or `"SDctry"`.
#
# At present will not work with data which contains more than one hierarchy.
# Also bypasses any optional tables (e.g. FT in H5).
# e.g.  extractHigherFields(myRDBESObject, "SS", "DEyear")

extractHigherFields <- function(object, table, field){

  # Check for a single hierarchy
  if(length(unique(object$DE$DEhierarchy)) == 1)
    hierarchy <- paste0("H", unique(object$DE$DEhierarchy)) else
      stop("Multiple Upper Hierarchies found.")

  # object hierarchy
  h <- paste0("H", unique(object$DE$DEhierarchy))

  # get path from DE to input table
  h.all <- tablesInRDBESHierarchies |>
    dplyr::filter(h == hierarchy) |>
    dplyr::arrange(sortOrder) |>
    dplyr::filter(FALSE == optional) |>
    dplyr::filter(FALSE == lowerHierarchy)
  path.to.table <- h.all$table
  path.to.table <- path.to.table[match("DE", path.to.table):match(table, path.to.table)] # path to SS

  # Always start with joining DE and SD
  joined_tbl <- dplyr::left_join(object$SD, # 2nd table
                                 object$DE, # 1st table
                                 by = "DEid",
                                 suffix = c('', '.y')) # ensures the second table names are not changed, otherwise the next join might fail

  for(i in 3:length(path.to.table)){ # 3 since first 2 tables always DE and SD

    # Join to next table in path
    joined_tbl <- dplyr::inner_join(object[[path.to.table[i]]], # next table
                                    joined_tbl,
                                    by = paste0(path.to.table[i-1], "id"), # previous table id column
                                    suffix = c('', '.y'))
  }

  # Output column
  if(!(field %in% names(joined_tbl))) stop("'field' not found in higher tables column names") else
    output <- joined_tbl[[which(names(joined_tbl) == field)]]

  return(output)
}
