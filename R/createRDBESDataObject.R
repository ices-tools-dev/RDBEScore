#' Create an RDBES Data Object
#'
#' This function lets you create an RDBES Data object in your current R
#' environment.
#'
#' The `input` should be either:
#'  - A `zip` file downloaded from RDBES (or multiple zip files if you want to include or overwrite tables, for example CL and CE data). NOTE: Only the downloaded 
#' RDBES data with Table data format with ids is loaded by this function and not the uploaded format.
#'  - A folder containing `csv` files downloaded from RDBES (e.g. the unzipped file), or any set of csv files of the RDBES tables.
#'  - A `list` of data frames in the current environment representing different tables in the hierarchy.
#'  - A `NULL` input will return and empty RDBES data object
#'
#' @details
#'
#' ***ZIP file inputs***
#' This `input` should be a path to a zip file downloaded from RDBES. Multiple
#' zip files can be entered if you want to include additional tables, for
#' example CL and CE. E.g. `input = c("path/to/H1.zip", "path/to/CL.zip"). If
#' any tables in the first input are overwritten by other inputs a warning is
#' given. You should not input different hierarchy files; this function will not
#' combine them.
#'
#' ***CSV file inputs***
#' This `input` should be a path to a folder of `csv` files. These can be the
#' `csv` files downloaded from RDBES (e.g. an unzipped hierarchy), or *any* set
#' of csv files containing RDBES tables. If the files do not have the default
#' RDBES name (e.g. 'Design.csv') the `listOfFileNames` input can by used to
#' specify the file names e.g. `list("DE" = "DE.csv", "SD" = "SD.csv", etc.)`.
#'
#' ***List of data frames inputs***
#' This `input` should be a `list` object containing data frames (or
#' data.tables) for each table in your hierarchy. They should be named with the
#' appropriate 2-letter code (`DE`, `SD`, etc.). Columns within these tables
#' will be renamed to the RDBES model documentation 'R name'. Note if you choose
#' to create an `RDBESDAtaObject` from local data frames these may have not
#' passed the data integrity checks performed when you upload to RDBES!
#'
#' ***NULL inputs***
#' This `input` produces an empty `RDBESDataObject`, i.e. all tables with
#' correct data classes but the tables will be empty.
#'
#' @param input Strings or `list` object. The path to the zip file downloaded
#'   from RDBES (or multiple zip files - see details), or path to a folder of
#'   `csv` files, or a `list` object in the current environment containing data
#'   frames of each table. If `NULL` an empty `RDBESDataObject` is created.
#' @param listOfFileNames `list` of Strings, Optional. For use with `csv` inputs
#'   only, and only required if the csv file names are *not* the default file
#'   names used by RDBES when downloading data (for instance if you created the
#'   files yourself). The actual file names should be a `list` of the two-letter
#'   code for the relevant table e.g. `list("DE" = "DE.csv", "SD" = "SD.csv",
#'   etc.)`.  If not used then it is assumed the files have the default file
#'   names used by the RDBES data download ("Design.csv" etc).
#' @param castToCorrectDataTypes Logical. If `TRUE` then the function will
#'   attempt to cast the required columns to the correct data type.  If `FALSE`
#'   then the column data types will be determined by how the csv files are read
#'   in. Default is `TRUE`.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#'  out, or FALSE if you don't.  The default is FALSE.
#' @param ... parameters passed to validateRDBESDataObject
#' if input is list of data frames e.g.`strict=FALSE`
#' @importFrom utils file_test
#'
#' @return A RDBESDataObject
#' @export
#' @md
#'
#' @examples
#' myEmptyRDBESObject <- createRDBESDataObject(input = NULL)

createRDBESDataObject <- function(input = NULL,
                                  listOfFileNames = NULL,
                                  castToCorrectDataTypes = TRUE,
                                  verbose = FALSE,
                                  ...) {


  # Classify input type
  if(any(is.character(input)) && any(grepl(".zip", input))) {
    if(!(all(grepl(".zip", input)))) stop("You cannot import a mix of 'csv' and 'zip' inputs. To import multiple tables unzip all files and import as a folder of 'csv' files.")
    import.type <- "zip"
    if (verbose) print("Input is a zip file")
    # if input is string and folder/directory assume it contains csv files
  } else if(length(input) == 1 && is.character(input) && file_test("-d", input)) {
    import.type <- "csv"
    if (verbose) print("Input is a folder name which is assumed to contains csv files")
    # if input is a list of data tables
  } else if(is.list(input) && !is.data.frame(input) && all(sapply(input, is.data.table))) {
    import.type <- "list.of.dts"
    if (verbose) print("Input is a list of data.tables")
    # if input is a list of data frames
  } else if(is.list(input) && !is.data.frame(input) && all(sapply(input, class) == "data.frame")) {
    import.type <- "list.of.dfs"
    if (verbose) print("Input is a list of data frames")
    # if input is NULL...
  }else if(is.null(input)) {
    import.type <- "null"
  } else {
    stop("Input type not recognised. Should be a RDBES zip file, folder of csv files, or list of data frames.")
  }

  # -------------------------------------------------------------------------

  if(import.type == "zip") output <- importRDBESDataZIP(filenames = input,
                                                        castToCorrectDataTypes = castToCorrectDataTypes, ...)

  if(import.type == "csv") output <- importRDBESDataCSV(rdbesExtractPath = input,
                                                           listOfFileNames = listOfFileNames,
                                                           castToCorrectDataTypes = castToCorrectDataTypes)


  if(import.type == "list.of.dfs" || import.type == "list.of.dts") {
    warning("NOTE: Creating RDBES data objects from a list of local data frames or data.tables bypasses the RDBES upload data integrity checks.\n")
    output <- importRDBESDataDFS(myList = input,
                                 castToCorrectDataTypes = castToCorrectDataTypes,
                                 verbose = verbose,
                                 ...)
  }

  if(import.type == "null") {
    output <- newRDBESDataObject()
  }

  return(output)
}
