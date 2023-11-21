#read the downloaded zip file
#this file is downloaded from the RDBES the contents of this file is prepared in
# the files in the directory exampleData/TextBookExamples and uploaded to RDBES

zipFname <- "./data-raw/exampleData/TextBookExamples/DownloadsFromRDBES/ZW_1965_WGRDBES-EST_TEST_1.zip"
data <- RDBEScore::createRDBESDataObject(zipFname)

#keep only the selected sampling scheme
samp_scheme <-"WGRDBES-EST TEST 1"
data <- RDBEScore::filterRDBESDataObject(data,
                                         fieldsToFilter = "DEsampScheme",
                                         valuesToFilter = samp_scheme,
                                         killOrphans = T)
datasetNames <- unique(data$DE$DEstratumName)

#fix wrong named data these are from survey actually
wrongDataSets <- c('Pckg_SDAResources_apiclus1_v2_H1',
                    'Pckg_SDAResources_apiclus2_v2_H1')
#add all stratums to package data
for(dname in datasetNames){
  #fix the naming removing strange characters
  datasetName <- gsub("[[:punct:]]|[[:space:]]", "_",
                      paste0(dname))
  #replace the name
  if(dname %in% wrongDataSets){
    datasetName <- sub("SDAResources", "survey", datasetName)
    }
  deData <-  RDBEScore::filterRDBESDataObject(data,
                                              fieldsToFilter = "DEstratumName",
                                              valuesToFilter = dname,
                                              killOrphans = T)
  # restricts species list
  deData$SL<-deData$SL[grepl(deData$SL$SLspeclistName, pat=dname)]

  assign(datasetName, deData)
  do.call(eval(parse(text="usethis::use_data")),
          list(as.name(datasetName), overwrite = TRUE))
}
