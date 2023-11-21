#' Import RDBES Downloaded Data
#'
#' An internal function used by `createRDBESDataObject`. Reads the .zip files
#' downloadable from the ICES RDBES web page. The function accepts a filepaths
#' to zip files, unzips and import tables as an object of class
#' `RDBESDataObject`.
#'
#' @param filenames - path to zipfile. Multiple paths can be entered if, for
#'   instance, you want to add CL and CE tables to the same object.
#' @param castToCorrectDataTypes Logical. If `TRUE` then the function will
#'   attempt to cast the required columns to the correct data type.  If `FALSE`
#'   then the column data types will be determined by how the csv files are read
#'   in. Default is `TRUE`.
#'
#' @return a list of all the RDBES data tables The table that are not in input
#'   data are NULL
#'
#' @keywords internal
#' @md
#'
#' @examples
#' \dontrun{
#' rdbesExtractPath <- "./2022_FPN_FPE_H7.zip"
#' obj <- importRDBESDataZIP(rdbesExtractPath)
#' }

importRDBESDataZIP <- function(filenames,
                               castToCorrectDataTypes = TRUE) {

  # Generates random number for the temp import folder name
  randInt <- paste0(sample(1:100, 3), collapse = "")
  tmp <- paste0(tempdir(), "/downloadImport", randInt)
  dir.create(tmp)
  all_unzipped <- c()
  unzipFile <- function(x, tmp) {

    if (!file.exists(x)) {
      return()
    }

    if (is.zip(x)) {
      unzipped <- utils::unzip(x, exdir= tmp)
      unzipped <- basename(unzipped)
      unzipped <- unzipped[grep("*.csv", unzipped)]
      intersected <- intersect(unzipped, all_unzipped)
      if(length(intersected) != 0) {
        warning(paste0("Duplicate unzipped files detected:\n",  paste0("\t", intersected, "\n", collapse="\n")))
      }
      all_unzipped <<- c(all_unzipped, unzipped)
      return(unzipped)
    }

  }

  # the files are not used currently but can be if we want to
  files <- unique(unlist(sapply(filenames, unzipFile, tmp)))
  res <- importRDBESDataCSV(tmp,
                            castToCorrectDataTypes = castToCorrectDataTypes)
  unlink(tmp, recursive = T)

  return(res)

}
