capture.output({  ## suppresses printing of console output when running test()




  # common parameters
  # H1 directory
  dirH1 <- "./h1_v_1_19_26/"
  # H5 directory
  dirH5 <- "./h5_v_1_19_26/"
  # H7 directory
  dirH7 <- "./h7_v_1_19_26/"

  # H1 object for comparison
  expObjH1 <- H1Example


  # Test general behavior  --------------------------------------------------

  test_that("createRDBESDataObject will give a warning if given a dir with no relevant  files in it",  {

    myPath <- "."
    expect_warning(createRDBESDataObject(input = myPath),"No relevant files found in given directory - an empty object will be created")


  })


  test_that("Expect warning when the input is not csv, list of dfs, zip files, NULL - Test Integer input", {

    myPath <- as.integer(1)

    expect_error(createRDBESDataObject(myPath), "Input type not recognised. Should be a RDBES zip file, folder of csv files, or list of data frames.")

  })

  test_that("Expect warning when the input is not csv, list of dfs, zip files, NULL - Test dataframe input", {

    myPath <- as.data.frame(1)

    expect_error(createRDBESDataObject(myPath), "Input type not recognised. Should be a RDBES zip file, folder of csv files, or list of data frames.")

  })


  test_that("createRDBESDataObject will throw an error when given multiple inputs", {
    zipFiles <- c(
      "H1_2023_10_16.zip"
    )
    H1 <- paste0(dirH1, zipFiles)
    df <- as.data.frame(1)

    expect_error(createRDBESDataObject(input = c(H1, df, dirH1)), "Input type not recognised. Should be a RDBES zip file, folder of csv files, or list of data frames.")

  })


  # Test ZIP inputs ---------------------------------------------------------

  test_that("importing foldered zipped H1 example data works", {
    zipFiles <- c(
      "H1_2023_10_16_fd.zip"
    )

    genObj <- createRDBESDataObject(paste0(dirH1, zipFiles),
                                    castToCorrectDataTypes = TRUE)

    expect_equal(genObj, expObjH1)

  })

  test_that("importing foldered zipped H1 and H2 gives an error", {
    zipFiles <- c(
      "H1_H2_2023_10_16_fd.zip"
    )

    genObj <- expect_error(
      createRDBESDataObject(paste0(dirH1, zipFiles),
                            castToCorrectDataTypes = TRUE),
      "You cannot import a mix of different hierarchies in one 'zip' input. To import multiple tables unzip all files and import as a folder of 'csv' files."
    )

  })

  test_that("importing zipped H1 example data works", {
    zipFiles <- c(
      "H1_2023_10_16.zip"
    )

    genObj <- createRDBESDataObject(paste0(dirH1, zipFiles),
                                    castToCorrectDataTypes = TRUE)

    expect_equal(genObj, expObjH1)

  })

  test_that("importing some data that is not zipped with some data that are zipped H1 example data should not work", {
    zipFiles <- c(
      "H1_2023_10_16.zip",
      "HSL_2023_10_16.zip",
      "VesselDetails.csv"
    )

    genObj <- expect_error(
      createRDBESDataObject(paste0(dirH1, zipFiles),
                            castToCorrectDataTypes = TRUE),
      "You cannot import a mix of 'csv' and 'zip' inputs. To import multiple tables unzip all files and import as a folder of 'csv' files."
    )

  })

  test_that("importing subset H1 example data works", {
    zipFiles <- c(
      "HSL_2023_10_16.zip",
      "HVD_2023_10_16.zip"
    )

    genObj <- createRDBESDataObject(paste0(dirH1, zipFiles),
                                    castToCorrectDataTypes = TRUE)
    expect_equal(genObj$VD, expObjH1$VD)
    expect_equal(genObj$SS, NULL)
    expect_equal(genObj$SL, expObjH1$SL)
  })

  test_that("Overwriting a table from a zip file produces a warning", {
    zipFiles <- c(
      "H1_2023_10_16.zip",
      "HVD_2023_10_16.zip")


    expect_warning(
      createRDBESDataObject(paste0(dirH1, zipFiles),
                            castToCorrectDataTypes = FALSE),
      "Duplicate unzipped files detected:\n")


  })

  test_that("The order of importing should not matter", {
    zipFiles1 <- c(
      "H1_2023_10_16.zip",
      "HVD_2023_10_16.zip")

    genObj1 <- suppressWarnings(
      createRDBESDataObject(paste0(dirH1, zipFiles1),
                            castToCorrectDataTypes = TRUE))

    zipFiles2 <- c("HVD_2023_10_16.zip",
                   "H1_2023_10_16.zip")

    genObj2 <- suppressWarnings(
      createRDBESDataObject(paste0(dirH1, zipFiles2),
                            castToCorrectDataTypes = TRUE))

    expect_equal(genObj1$VD, genObj2$VD)
    expect_equal(genObj1$SS, genObj2$SS)
    expect_equal(genObj1$SL, genObj2$SL)
    expect_equal(genObj1$FT, genObj2$FT)
    expect_equal(genObj1, genObj2)
  })


  test_that("createRDBESDataObject creates an object with the correct data types",  {

    zipFiles <- c(
      "H1_2023_10_16.zip",
      "HVD_2023_10_16.zip")

    genObj <- suppressWarnings(createRDBESDataObject(input =  paste0(dirH1, zipFiles),
                                                     castToCorrectDataTypes = TRUE))

    myDiffs <- validateRDBESDataObjectDataTypes(genObj)

    numberOfDifferences <- nrow(myDiffs)
    expect_equal(numberOfDifferences,0)
  })


  # Test CSV inputs ---------------------------------------------------------


  test_that("createRDBESDataObject can create an object from a H1 data extract
          without errors or warnings",  {

            csvFilesH1 <- dirH1

            expect_warning(createRDBESDataObject(input = csvFilesH1), NA)
            expect_error(createRDBESDataObject(input = csvFilesH1), NA)


          })



  test_that("createRDBESDataObject can create an object from a H1 data extract
          without errors or warnings (when castToCorrectDataTypes = FALSE)",  {

            csvFilesH1 <- dirH1

            expect_warning(
              createRDBESDataObject(input = csvFilesH1,
                                    castToCorrectDataTypes = FALSE), NA)
            expect_error(
              createRDBESDataObject(input = csvFilesH1,
                                    castToCorrectDataTypes = FALSE), NA)
          })

  test_that("createRDBESDataObject can create an object from a H1 data extract
          with the right number of rows",  {

            csvFilesH1 <- dirH1

            myH1 <- createRDBESDataObject(input = csvFilesH1)

            # Get the default file names
            fileNames <- RDBEScore::DefaultFileNames
            # Remove any unnecessary files
            fileNames[names(fileNames) %in% c("TE","LO","OS","LE")] <- NULL
            # Append the right path
            fileNames <-lapply(fileNames, function(x){paste(csvFilesH1,x,".csv",sep="")})
            # Read the number of rows in each file
            fileNumberOfRows <- lapply(fileNames, function(x){length(readLines(x))-1})

            # For each file
            for(x in names(fileNumberOfRows)){
              # check if the value of nrow is equal to the length of readlines()-1
              expect_true(nrow(myH1[[x]]) == fileNumberOfRows[x])
            }

          })


  test_that("createRDBESDataObject can create an object from an H5 data extract
          without errors or warnings",  {

            csvFilesH5 <- dirH5

            expect_warning(createRDBESDataObject(input = csvFilesH5), NA)
            expect_error(createRDBESDataObject(input = csvFilesH5), NA)
          })


  test_that("createRDBESDataObject can create an object from an H5 data extract
          without errors or warnings (when castToCorrectDataTypes = FALSE)",  {

            csvFilesH5 <- dirH5

            expect_warning(
              createRDBESDataObject(input = csvFilesH5,
                                    castToCorrectDataTypes = FALSE), NA)
            expect_error(
              createRDBESDataObject(input = csvFilesH5,
                                    castToCorrectDataTypes = FALSE), NA)
          })

  test_that("createRDBESDataObject can create an object from a H5 data extract
          with the right number of rows",  {

            csvFilesH5 <- dirH5

            myH5 <- createRDBESDataObject(input = csvFilesH5)

            # Get the default file names
            fileNames <- RDBEScore::DefaultFileNames
            # Remove any unnecessary files
            fileNames[names(fileNames) %in% c("TE","LO","VS","FO")] <- NULL
            # Append the right path
            fileNames <-lapply(fileNames, function(x){paste(csvFilesH5,x,".csv",sep="")})
            # Read the number of rows in each file
            fileNumberOfRows <- lapply(fileNames, function(x){length(readLines(x))-1})

            # For each file
            for(x in names(fileNumberOfRows)){
              # check if the value of nrow is equal to the length of readlines()-1
              expect_true(nrow(myH5[[x]]) == fileNumberOfRows[x])
            }

          })


  test_that("createRDBESDataObject can create an object from an H1 data extract by specifying file names without errors or warnings",  {

    csvFilesH1 <- dirH1
    myFileNames <- list("DE"="DE.csv","SD"="SD.csv")

    myObject <- expect_warning(createRDBESDataObject(input = csvFilesH1, listOfFileNames = myFileNames), NA)
    myObject <- expect_error(createRDBESDataObject(input = csvFilesH1, listOfFileNames = myFileNames), NA)

  })


  test_that("createRDBESDataObject returns warning when input = NULL & the listofFileNames is not",  {

    csvFilesH1 <- NULL
    myFileNames <- list("DE"="DE.csv","SD"="SD.csv")

    expect_warning(createRDBESDataObject(input = csvFilesH1, listOfFileNames = myFileNames), NA)

  })

  test_that("createRDBESDataObject creates an object with the correct data types",  {

    csvFilesH1 <- dirH1

    myRDBESDataObject <- createRDBESDataObject(input = csvFilesH1,
                                               castToCorrectDataTypes = TRUE)

    myDiffs <- validateRDBESDataObjectDataTypes(myRDBESDataObject)

    numberOfDifferences <- nrow(myDiffs)
    expect_equal(numberOfDifferences,0)
  })




  test_that("createRDBESDataObject creates an H1 object with keys on the data tables",  {

    csvFilesH1 <- dirH1

    myRDBESDataObject <- createRDBESDataObject(input = csvFilesH1)

    # Not all of the RDBES table types are in the sample data
    expectedNumberOfTablesWithKeys <- 13
    actualNumberOfTablesWithKeys <- 0

    for(aTable in names(myRDBESDataObject)){
      if ('data.table' %in% class(myRDBESDataObject[[aTable]])){
        if (!is.null(data.table::key(myRDBESDataObject[[aTable]]))){
          actualNumberOfTablesWithKeys <- actualNumberOfTablesWithKeys + 1
        }
      }
    }

    expect_equal(expectedNumberOfTablesWithKeys,actualNumberOfTablesWithKeys)

  })



  test_that("createRDBESDataObject creates an H5 object with keys on the data tables",  {

    csvFilesH5 <- dirH5

    myRDBESDataObject <- createRDBESDataObject(input = csvFilesH5)

    # Not all of the RDBES table types are in the sample data
    expectedNumberOfTablesWithKeys <- 13
    actualNumberOfTablesWithKeys <- 0

    for(aTable in names(myRDBESDataObject)){
      if ('data.table' %in% class(myRDBESDataObject[[aTable]])){
        if (!is.null(data.table::key(myRDBESDataObject[[aTable]]))){
          actualNumberOfTablesWithKeys <- actualNumberOfTablesWithKeys + 1
        }
      }
    }

    expect_equal(expectedNumberOfTablesWithKeys,actualNumberOfTablesWithKeys)

  })


  test_that("createRDBESDataObject creates an H7 object with keys on the data tables",  {

    csvFilesH7 <- dirH7

    myRDBESDataObject <- createRDBESDataObject(input = csvFilesH7)

    # Not all of the RDBES table types are in the sample data
    expectedNumberOfTablesWithKeys <- 13
    actualNumberOfTablesWithKeys <- 0

    for(aTable in names(myRDBESDataObject)){
      if ('data.table' %in% class(myRDBESDataObject[[aTable]])){
        if (!is.null(data.table::key(myRDBESDataObject[[aTable]]))){
          actualNumberOfTablesWithKeys <- actualNumberOfTablesWithKeys + 1
        }
      }
    }

    expect_equal(expectedNumberOfTablesWithKeys,actualNumberOfTablesWithKeys)

  })


  test_that("createRDBESDataObject can create an object from a H1 data extract
          with the right number of rows, when CL has partially quoted vessel identifiers (issue #210)",  {

            # Test related to following issue
            #https://github.com/ices-tools-dev/RDBEScore/issues/210

            csvFilesH1 <- paste0(dirH1, "BadVesselIdentifiers/")
            print(csvFilesH1)

            myH1 <- createRDBESDataObject(input = csvFilesH1)
            print(myH1)

            # Get the default file names
            fileNames <- RDBEScore::DefaultFileNames
            # Remove any unnecessary files
            fileNames[!names(fileNames) %in% c("CL")] <- NULL
            # Append the right path
            fileNames <-lapply(fileNames, function(x){paste(csvFilesH1,x,".csv",sep="")})
            # Read the number of rows in each file
            fileNumberOfRows <- lapply(fileNames, function(x){length(readLines(x))-1})

            # For each file
            for(x in names(fileNumberOfRows)){
              # check if the value of nrow is equal to the length of readlines()-1
              expect_true(nrow(myH1[[x]]) == fileNumberOfRows[x])
            }

          })

  # Test list of dfs ---------------------------------------------------------

  # Create list of dfs for comparison
  list_with_nulls  <-  createRDBESDataObject(paste0(dirH1, "H1_2023_10_16.zip"))
  list_of_dfs <- list_with_nulls[!(sapply(list_with_nulls, is.null))]
  list_of_dfs <- lapply(list_of_dfs, as.data.frame)
  colMapping <-   stats::setNames(mapColNamesFieldR$Field.Name, mapColNamesFieldR$R.Name)
  list_of_dfs_long_names <- lapply(list_of_dfs, function(df, colMapping){
    colnames(df) <- colMapping[colnames(df)]
    df
  }, colMapping)
  # convert list_of_dfs and list_of_dfs_long_names to lists of data.tables
  list_of_dts <- lapply(list_of_dfs, as.data.table)
  list_of_dts_long_names <- lapply(list_of_dfs_long_names, as.data.table)


  test_that("Importing list of dfs works",{
    genObj <- suppressWarnings(createRDBESDataObject(list_of_dfs, castToCorrectDataTypes = FALSE))

    expect_equal(genObj, expObjH1)

  })

  test_that("Importing list of data.tables works",{
    genObj <- suppressWarnings(createRDBESDataObject(list_of_dts, castToCorrectDataTypes = FALSE))

    expect_equal(genObj, expObjH1)

  })

  test_that("Long column names are converted to R names (using list of data frames as input)",{
    genObj <- suppressWarnings(createRDBESDataObject(list_of_dfs_long_names, castToCorrectDataTypes = FALSE))

    expect_equal(genObj, expObjH1)

  })

  test_that("Long column names are converted to R names (using list of data tables as input)",{
    genObj <- suppressWarnings(createRDBESDataObject(list_of_dts_long_names, castToCorrectDataTypes = FALSE))

    expect_equal(genObj, expObjH1)

  })

  test_that("Extra column names are ignored (using list of data frames as input)",{
    colnames(list_of_dfs_long_names[["VS"]])[27] <- "wrong"
    genObj <- suppressWarnings(createRDBESDataObject(list_of_dfs_long_names, castToCorrectDataTypes = FALSE, strict=FALSE))
    colnames(expObjH1[["VS"]])[27] <- "wrong"
    expect_equal(genObj, expObjH1)

  })

  test_that("Extra column names are ignored (using list of data tables as input)",{
    colnames(list_of_dts_long_names[["VS"]])[27] <- "wrong"
    genObj <- suppressWarnings(createRDBESDataObject(list_of_dts_long_names, castToCorrectDataTypes = FALSE, strict=FALSE))
    colnames(expObjH1[["VS"]])[27] <- "wrong"
    expect_equal(genObj, expObjH1)

  })

  test_that("Importing list of dfs with castToCorrectDataTypes = TRUE works",{
    genObj <- suppressWarnings(createRDBESDataObject(list_of_dfs, castToCorrectDataTypes = TRUE))

    myDiffs <- validateRDBESDataObjectDataTypes(genObj)

    numberOfDifferences <- nrow(myDiffs)
    expect_equal(numberOfDifferences,0)

  })

  test_that("Importing list of data tables with castToCorrectDataTypes = TRUE works",{
    genObj <- suppressWarnings(createRDBESDataObject(list_of_dts, castToCorrectDataTypes = TRUE))

    myDiffs <- validateRDBESDataObjectDataTypes(genObj)

    numberOfDifferences <- nrow(myDiffs)
    expect_equal(numberOfDifferences,0)

  })

  test_that("Importing list of dfs with different names than the ones requested does not work",{

    names(list_of_dfs)[1] <- "Design"
    expect_error(expect_warning(createRDBESDataObject(list_of_dfs, castToCorrectDataTypes = FALSE),
                                "NOTE: Creating RDBES data objects from a list of local data frames or data.tables bypasses the RDBES upload data integrity checks.\n"),
                 "You have given list names that are not valid:\nDesign")

  })

  test_that("Importing list of data.tables with different names than the ones requested does not work",{

    names(list_of_dts)[1] <- "Design"
    expect_error(expect_warning(createRDBESDataObject(list_of_dts, castToCorrectDataTypes = FALSE),
                                "NOTE: Creating RDBES data objects from a list of local data frames or data.tables bypasses the RDBES upload data integrity checks.\n"),
                 "You have given list names that are not valid:\nDesign")
  })

  test_that("Importing list of dfs with duplicate table names does not work",{

    names(list_of_dfs)[1] <- "DE"
    names(list_of_dfs)[2] <- "DE"
    expect_error(expect_warning(createRDBESDataObject(list_of_dfs, castToCorrectDataTypes = FALSE), "NOTE: Creating RDBES data objects from a list of local data frames or data.tables bypasses the RDBES upload data integrity checks.\n"),
                 "You have given list names that have duplicate table names:\nDE")

  })

  test_that("Importing list of data.tables with duplicate table names does not work",{

    names(list_of_dts)[1] <- "DE"
    names(list_of_dts)[2] <- "DE"
    expect_error(expect_warning(createRDBESDataObject(list_of_dts, castToCorrectDataTypes = FALSE), "NOTE: Creating RDBES data objects from a list of local data frames or data.tables bypasses the RDBES upload data integrity checks.\n"),
                 "You have given list names that have duplicate table names:\nDE")

  })

  test_that("Importing list of dfs with different names than the ones requested & duplicate table names does not work",{

    names(list_of_dfs)[1] <- "Design"
    names(list_of_dfs)[2] <- "DE"
    names(list_of_dfs)[3] <- "DE"
    expect_error(expect_warning(createRDBESDataObject(list_of_dfs,
                                                      castToCorrectDataTypes = FALSE),
                                "NOTE: Creating RDBES data objects from a list of local data frames or data.tables bypasses the RDBES upload data integrity checks."),
                 "You have given list names that are not valid:\nDesign")

  })

  test_that("Importing list of data.tables with different names than the ones requested & duplicate table names does not work",{

    names(list_of_dts)[1] <- "Design"
    names(list_of_dts)[2] <- "DE"
    names(list_of_dts)[3] <- "DE"
    expect_error(expect_warning(createRDBESDataObject(list_of_dts,
                                                      castToCorrectDataTypes = FALSE),
                                "NOTE: Creating RDBES data objects from a list of local data frames or data.tables bypasses the RDBES upload data integrity checks."),
                 "You have given list names that are not valid:\nDesign")

  })

  test_that("Importing multiple lists does not work",{

   list1 <- list_of_dfs
   list2 <- list_of_dfs
    expect_error(expect_warning(createRDBESDataObject(input = c(list1, list2),
                                                      castToCorrectDataTypes = FALSE),
                                "NOTE: Creating RDBES data objects from a list of local data frames or data.tables bypasses the RDBES upload data integrity checks.\n"),
                 "You have given list names that have duplicate table names:\nDE, SD, VS, FT, FO, SS, SA, FM, BV, VD, SL")

  })

  test_that("Importing dataframe does not work",{

    df <- as.data.frame(list_of_dfs[["DE"]])
    expect_error(createRDBESDataObject(input = df,
                                       castToCorrectDataTypes = FALSE),
                 "Input type not recognised. Should be a RDBES zip file, folder of csv files, or list of data frames.")

  })

  test_that("Giving an empty dataframe within the list does not work",{


    list_with_nulls <- lapply(list_with_nulls, as.data.frame)


    expect_error(expect_warning(createRDBESDataObject(input = list_with_nulls, castToCorrectDataTypes = FALSE),
                                "NOTE: Creating RDBES data objects from a list of local data frames bypasses the RDBES upload data integrity checks.\n"),
                 "id does not exist in the input tables.")

  })




}) ## end capture.output



