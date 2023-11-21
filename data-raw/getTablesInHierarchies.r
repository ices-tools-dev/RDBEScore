# Use the RDBES xsd files to determine which tables are
# required in the different RDBES hierachies

# Set to TRUE if you want to download the
# lastest xsd files from GitHub
#downloadFromGitHub <- TRUE
#gitHubFileLocation <-
  "https://api.github.com/repos/ices-tools-dev/RDBES/contents/XSD-files"
# The folder to read the files from.  If you are
# downloading from GitHub a copy of the latest files will be saved here
#fileLocation <- "data-raw/xsd/"
#
# # STEP 1) Get the BaseTypes file (if required)
# if (downloadFromGitHub) {
#   myHierarchyFiles <- NULL
#   myResponse <- httr::GET(gitHubFileLocation)
#   filesOnGitHub <- httr::content(myResponse)
#
#   for (myFile in filesOnGitHub) {
#     myGitHubFile <- data.frame(fileName = myFile$name
#                                , downloadURL = myFile$download_url)
#     if (is.null(myHierarchyFiles)) {
#       myHierarchyFiles <- myGitHubFile
#     } else {
#       myHierarchyFiles <- rbind(myHierarchyFiles, myGitHubFile)
#     }
#   }
#   # Sub-set to the files we are interested in
#   myHierarchyFiles <- myHierarchyFiles[grepl("^H.*xsd$"
#                                              , myHierarchyFiles$fileName), ]
#
#   print(paste("Downloading ", nrow(myHierarchyFiles)
#               , " files from GitHub", sep = ""))
#
#   # Download our files
#   for (i in seq_len(nrow(myHierarchyFiles))) {
#     anHierarchyFile <- httr::GET(myHierarchyFiles[i, "downloadURL"])
#     # stop if there was a problem accessing the file
#     httr::stop_for_status(anHierarchyFile$status_code)
#     # save the file locally
#     writeLines(httr::content(anHierarchyFile, "text")
#               , paste(fileLocation,
#                       myHierarchyFiles[i, "fileName"], sep = "")
#     )
#   }
# }
#
# # Read all the H.*xsd files
# filesToRead <- list.files(path = fileLocation
#                           , pattern = "^H.*xsd$"
#                           , recursive = FALSE
#                           , full.names = FALSE)
#
#
# myHierarchyTables <- list()
# for (fileToParse in filesToRead) {
#
#   fileToParse <- paste(fileLocation, fileToParse, sep = "")
#
#   # STEP 2) Parse the XML
#   doc <- XML::xmlTreeParse(fileToParse, useInternal = TRUE)
#   myXML <- XML::xmlToList(doc)
#
#   myResults <- NULL
#   hierachyName <- NULL
#
#   for (myElement in myXML[names(myXML) == "complexType"]) {
#     myAttr <- myElement$.attrs
#     names(myAttr) <- NULL
#
#     if (grepl("^H.*", myAttr)) {
#       hierachyName <- myAttr
#     }
#     if (nchar(myAttr) == 2 & !grepl("^H.*", myAttr)) {
#       if (is.null(myResults)) {
#         myResults <- c(myAttr)
#       } else {
#         myResults <- c(myResults, myAttr)
#       }
#     }
#   }
#
#   # Add to our list of results
#   myHierarchyTables[[hierachyName]] <- myResults
# }

#tablesInRDBESHierarchies <- myHierarchyTables

# 16/10/23 - Easier to hard-code the table order - it doesn't change much
# and my code to read the xsd files wasn't interpreting them correctly.
# This will need updating if the order of the tables in the hierarchies changes.
# Get rid of the code above once we're happy.

myHierarchyTables <- list()
myHierarchyTables[["H1"]] <- c("DE", "SD", "VS" ,"FT" ,"FO" ,"SS" ,"SA" ,"FM" ,"BV")
myHierarchyTables[["H2"]] <- c("DE", "SD", "FT", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H3"]] <- c("DE", "SD", "TE", "VS", "FT", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H4"]] <- c("DE", "SD", "OS", "FT", "LE", "SS", "SA", "FM", "BV")
#myHierarchyTables[["H5"]] <- c("DE", "SD", "OS", "FT", "LE", "SS", "SA", "FM", "BV")
myHierarchyTables[["H5"]] <- c("DE", "SD", "OS", "LE", "FT", "SS", "SA", "FM", "BV")
myHierarchyTables[["H6"]] <- c("DE", "SD", "OS", "FT", "FO", "LE", "SS", "SA", "FM", "BV")
#myHierarchyTables[["H7"]] <- c("DE", "SD", "OS", "LE", "SS", "SA", "FM", "BV")
myHierarchyTables[["H7"]]<- c("DE", "SD", "OS", "SS", "LE", "SA", "FM", "BV")
myHierarchyTables[["H8"]] <- c("DE", "SD", "TE", "VS", "FT", "LE", "SS", "SA", "FM", "BV")
myHierarchyTables[["H9"]] <- c("DE", "SD", "LO", "TE", "SS", "LE", "SA", "FM", "BV")
myHierarchyTables[["H10"]] <- c("DE", "SD", "VS", "TE", "FT", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H11"]] <- c("DE", "SD", "LO", "TE", "FT", "LE", "SS", "SA", "FM", "BV")
#myHierarchyTables[["H12"]] <- c("DE", "SD", "LO", "TE", "FT", "LE", "SS", "SA", "FM", "BV")
myHierarchyTables[["H12"]] <- c("DE", "SD", "LO", "TE", "LE", "SS", "SA", "FM", "BV") # CHECK
#myHierarchyTables[["H13"]] <- c("DE", "SD", "FT", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H13"]] <- c("DE", "SD", "FO", "SS", "SA", "FM", "BV") # CHECK


# Add on some values describing the table
myHierarchyTablesDF <-
  lapply(myHierarchyTables,
       function(x){
         data.frame(table = x,
                    lowerHierarchy = FALSE,
                    optional = FALSE,
                    samplingUnit = TRUE,
                    sortOrder = seq(1:length(x)))
  })

# Combine into a single data frame
myHierarchyTablesDF <- data.table::rbindlist(myHierarchyTablesDF,idcol=TRUE)
names(myHierarchyTablesDF)[names(myHierarchyTablesDF) == ".id"] <- "hierarchy"

# Set FM and BV to lower hierarchy
myHierarchyTablesDF[myHierarchyTablesDF$table %in% c("FM","BV"),"lowerHierarchy"] <- TRUE

## Make some specifc changes for hierarchies with option tables and tables that aren't a sampling unit

# Set FT in H5 to be optional and not a sampling unit
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H5" & myHierarchyTablesDF$table == "FT","optional"] <- TRUE
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H5" & myHierarchyTablesDF$table == "FT","samplingUnit"] <- FALSE

# Set FT in H8 to be optional and not a sampling unit
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H8" & myHierarchyTablesDF$table == "FT","optional"] <- TRUE
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H8" & myHierarchyTablesDF$table == "FT","samplingUnit"] <- FALSE

# Set LE in H7 to not be a sampling unit
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H7" & myHierarchyTablesDF$table == "LE","samplingUnit"] <- FALSE

# Set LE in H9 to not be a sampling unit
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H9" & myHierarchyTablesDF$table == "LE","samplingUnit"] <- FALSE


tablesInRDBESHierarchies <- myHierarchyTablesDF
usethis::use_data(tablesInRDBESHierarchies, overwrite = TRUE)
