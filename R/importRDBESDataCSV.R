#' Create an RDBES Raw Object
#'
#' Create an R object from a folder containing .csv data files downloaded from
#' the RDBES upload/download page.
#'
#' @param rdbesExtractPath (Optional) The path to the csv files produced as an
#' extract by the ICES RDBES.  If no path is suppled then an empty
#' RDBESDataObject will be returned.
#' @param listOfFileNames (Optional) A named list of file names - the list names
#' shoudl be the two-letter code for the relevent table e.g.
#' list("DE" = "DE.csv",... ).  If the parameter is not supplied then the
#' default file names used by the RDBES data download will be used e.g.
#' "Design.csv" etc.
#' @param castToCorrectDataTypes (Optional) If TRUE then the function
#' will attempt to cast the required columns to the correct data type.  If
#' FALSE then the column data types will be determined by how the csv files
#' are read in.  The default is TRUE
#'
#' @keywords internal
#' @md
#'
#' @return A RDBESDataObject.  If a path to RDBES extract files is provided then
#' it will contain the data from those files.  If no path is supplied then
#' an empty RDBESDataObject will be returned.
#'
#' @examples
#' \dontrun{
#' myEmptyRDBESObject <- importRDBESDataCSV()
#' rdbesExtractPath <- "./tests/testthat/h7_v_1_19_18/"
#' obj <- importRDBESDataCSV(rdbesExtractPath)
#' }

importRDBESDataCSV <- function(rdbesExtractPath = NULL,
                               listOfFileNames = NULL,
                               castToCorrectDataTypes = TRUE,
                               ...) {


  # If we have not been passed a list of file names use a default
  if (is.null(listOfFileNames)){

    # A named list with the file names that are produced by the RDBES download
    fileNames <- RDBEScore::DefaultFileNames

    # Stick ".csv" on to each default name
    fileNames <-lapply(fileNames, function(x){paste(x,".csv",sep="")})

  } else {
    fileNames <- listOfFileNames
  }

  # Create a named list using the short names - set all values to NULL
  myList <- stats::setNames(
    as.list(replicate(length(fileNames), NULL)),
    names(fileNames)
  )


  # If we have been supplied with a path to files we will try and read them
  # otherwise we'll just return an empty rdbesRawBoject
  if (!is.na(rdbesExtractPath)) {

    # Determine the files which actually exist
    filesWhichExist <- names(
      fileNames[file.exists(
        paste(rdbesExtractPath, "/", fileNames, sep = "")
      )]
    )

    # If we don't find relevent files in the dir give a warning
    if (length(filesWhichExist) == 0) {
      warning(paste0("No relevant files found in given directory",
      " - an empty object will be created"))
    } else {

  # Read the files which exist
  for (myFile in filesWhichExist) {
        # define R data types. Nessesary is to present fields whith characters
        #longer than 20. Use colClasses to define the data types. If you don't
        #use colClasses BVfishId is convert to scientific format and present as
        #character, when whole information about BVfishId is cutting.
        fieldsFormat <- RDBEScore::mapColNamesFieldR[
          RDBEScore::mapColNamesFieldR$Table.Prefix == myFile, "RDataType"]

        # Read the file with error handling so we can raise a clear message
        filePath <- paste(rdbesExtractPath, "/", fileNames[myFile],  sep = "")
        csvBaseName <- sub("\\.csv$", "", fileNames[myFile])
        readResult <- try(
          utils::read.csv(
            filePath,
            header = TRUE,
            sep = ",",
            quote = "",
            stringsAsFactors = FALSE,
            colClasses = fieldsFormat
          ),
          silent = TRUE
        )

        if (inherits(readResult, "try-error")) {
          stop(
            paste0(
              "The input file has unexpected structure in the table ",
              myFile,
              " (",
              csvBaseName,
              ")"
            ),
            call. = FALSE
          )
        }

        myList[[myFile]] <- readResult

  # Change each entry to a data table
        data.table::setDT(myList[[myFile]])

        # Change database field names to R names where we can
        myNames <- RDBEScore::mapColNamesFieldR[
          RDBEScore::mapColNamesFieldR$Table.Prefix == myFile, ]
        myNameMatches <- match(
          trimws(tolower(names(myList[[myFile]]))),
          trimws(tolower(myNames$Field.Name))
        )
        myNameMatchesNotNA <- myNameMatches[!is.na(myNameMatches)]
        names(myList[[myFile]])[!is.na(myNameMatches)] <-
          myNames[myNameMatchesNotNA, "R.Name"]
      }
    }

    ## DATA FIX - spelling mistake in 1 download file format ...
    if ("CLincidentialByCatchMitigationDevice" %in%
        names(myList[["CL"]])) {
      data.table::setnames(
        myList[["CL"]]
        , "CLincidentialByCatchMitigationDevice"
        , "CLIBmitiDev")
    }

  }

  # Create an RDBESDataObject using the constructor
  myRDBESDataObject <- RDBEScore::newRDBESDataObject(DE = myList[["DE"]],
                                                     SD = myList[["SD"]],
                                                     VS = myList[["VS"]],
                                                     FT = myList[["FT"]],
                                                     FO = myList[["FO"]],
                                                     TE = myList[["TE"]],
                                                     LO = myList[["LO"]],
                                                     OS = myList[["OS"]],
                                                     LE = myList[["LE"]],
                                                     SS = myList[["SS"]],
                                                     SA = myList[["SA"]],
                                                     FM = myList[["FM"]],
                                                     BV = myList[["BV"]],
                                                     VD = myList[["VD"]],
                                                     SL = myList[["SL"]],
                                                     IS = myList[["IS"]],
                                                     CL = myList[["CL"]],
                                                     CE = myList[["CE"]])


  if (castToCorrectDataTypes){
    # Ensure all the columns are the correct data type
    myRDBESDataObject <- setRDBESDataObjectDataTypes(myRDBESDataObject)
  }

  # Set a key on any data tables in myList - use the XXid column as the key
  for(aTable in names(myRDBESDataObject)){
    if ('data.table' %in% class(myRDBESDataObject[[aTable]])){
      data.table::setkeyv(myRDBESDataObject[[aTable]],paste0(aTable,"id"))
    }
  }

  #check the data
  validateRDBESDataObject(myRDBESDataObject,
                          checkDataTypes = castToCorrectDataTypes,
                          ...)

  # Return the object
  myRDBESDataObject
}
