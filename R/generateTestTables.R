#' Generate a Data Table
#'
#' @param tblName Name of the table
#' @param prevTbls list of data.tables upstream of the generated table.
#'  Defaults to empty list
#' @param rows  numeric number of rows per parent record. Defaults to 4.
#' @param propSamp  numeric proportion of how many of total are
#'  sampled. This is ignored for "CENSUS". Defaults to 0.5
#' @param selMeth character selection method used. Defaults to "CENSUS".
#'  Others like SRSWR or SRSSWOR can be used as well
#' @param stratums character vector of the stratum names to be created.
#' Defaults to c("U"), meaning not stratified.
#' @param mean numeric the expected mean of the target variable.
#' The variable is created using \code{\link[stats]{rnorm}} and saved under
#'  column ending with "y". Defaults to 5.
#'
#' @import data.table
#' @importFrom stats rnorm
#'
#' @return a data.table
#'
makeTbl <- function(tblName,
                    prevTbls = list(),
                    rows = 4,
                    propSamp = 0.5,
                    selMeth = "CENSUS",
                    stratums = c("U"),
                    mean = 5) {
  cols <- getColnames(tblName)
  prevTbl <- NULL
  times <- rows
  total <- rows / propSamp
  if (selMeth == "CENSUS") {
    total <- rows
  }
  stratified <- ifelse(length(stratums) == 1, "N", "Y")

  if (length(prevTbls) > 0) {
    prevTbl <- prevTbls[[length(prevTbls)]]
  }
  if (!is.null(prevTbl)) {
    times <- rows * nrow(prevTbl)
    # take the first id column
    cols <- c(cols, grep("id$", colnames(prevTbl), value = TRUE)[1])
  }
  data <- list(
    1:times,
    rep(rows, times),
    rep(total, times),
    rep(selMeth, times),
    rep(stratums, times / length(stratums)),
    rep(stratified, times)
  )
  if (!is.null(prevTbl)) {
    ids <- prevTbl[, get(cols[length(cols)])]
    data[[length(cols)]] <- rep(ids, each = rows)
  }
  names(data) <- cols
  data[[paste0(tblName, "y")]] <- rnorm(n=times, mean=5)
  data[[paste0(tblName, "recType")]] <- tblName
  as.data.table(data)
}
#' Generate a List of Related Data Tables
#'
#' Generates a named list of data tables that follow the structure of
#' RDBESDataObject. The tables only have columns required for testing
#' The generate tables
#'
#' @param tblNames character vector of table names to be created
#' @inheritParams makeTbl
#' @inheritDotParams makeTbl
#'
#' @return a list of named data.table's
#'
#' @examples
#' \dontrun{
#' generateTestTbls(c("A", "B", "C"), selMeth = "SRSWOR")
#' generateTestTbls(LETTERS[1:5]) # makes 5 tables with method CENSUS
#' }
generateTestTbls <- function(tblNames, prevTbls = list(), ...) {
  tbl <- makeTbl(tblNames[1], prevTbls, ...)
  prevTbls[[tblNames[1]]] <- tbl
  if (length(tblNames) == 1) {
    return(prevTbls)
  }
  tblNames <- tblNames[tblNames != tblNames[1]]
  generateTestTbls(tblNames, prevTbls, ...)
}

getColnames <- function(tblName) {
  c(
    idCol = paste0(tblName, "id"),
    numSampledCol = paste0(tblName, "numSamp"),
    numTotalCol = paste0(tblName, "numTotal"),
    selMethodCol = paste0(tblName, "selectMeth"),
    stratumCol = paste0(tblName, "stratumName"),
    stratifiedCol = paste0(tblName, "stratification")
  )
}
