#read the downloaded zip file
#this file is downlaoded from the RDEBS the contents of this file is prepared in
# the files in the directory exampleData/TextBookExamples and uploaded to RDBES

zipFname <- "./data-raw/exampleData/madeUpExamples/downloadsFromRDBES/ZW_1965_WGRDBES-EST_TEST_3.zip"
data <- RDBEScore::createRDBESDataObject(zipFname)

#keep only the selected sampling scheme
samp_scheme <-"WGRDBES-EST TEST 3"
data <- RDBEScore::filterRDBESDataObject(data,
                                         fieldsToFilter = "DEsampScheme",
                                         valuesToFilter = samp_scheme,
                                         killOrphans = T)
datasetNames <- unique(data$DE$DEstratumName)
#seems that only the v1 is currently used
datasetNames <- datasetNames[grepl("_v1_", datasetNames)]
#add all stratums to package data
for(dname in datasetNames){
  #fix the naming removing strange characters
  datasetName <- gsub("[[:punct:]]|[[:space:]]", "_",
                      paste0(dname))
  deData <-  RDBEScore::filterRDBESDataObject(data,
                                              fieldsToFilter = "DEstratumName",
                                              valuesToFilter = dname,
                                              killOrphans = T)
  assign(datasetName, deData)
  do.call(eval(parse(text="usethis::use_data")),
          list(as.name(datasetName), overwrite = TRUE))
}
