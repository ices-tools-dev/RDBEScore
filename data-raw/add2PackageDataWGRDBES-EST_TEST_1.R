#read the downloaded zip file
#this file is downloaded from the RDBES the contents of this file is prepared in
#the files in the directory exampleData/TextBookExamples and uploaded to RDBES

zipFname <- "./data-raw/exampleData/TextBookExamples/DownloadsFromRDBES/ZW_1965_WGRDBES-EST_TEST_1.zip"
data <- RDBEScore::createRDBESDataObject(zipFname)

#keep only the selected sampling scheme
samp_scheme <-"WGRDBES-EST TEST 1"
data <- RDBEScore::filterRDBESDataObject(data,
                                         fieldsToFilter = "DEsampScheme",
                                         valuesToFilter = samp_scheme,
                                         killOrphans = T)

# dataset Pckg_SDAResources_coots_multistage_H1 has NA as VSnumTotal which
# creates a warning message when data is printed,. The following code
# checks that dataset is the only case, otherwise stops
aux<-RDBEScore:::createTableOfRDBESIds(data)
VSid_without_VSnumTotal<-unique(
  aux[aux$DEid==data$DE$DEid[
      data$DE$DEstratumName=="Pckg_SDAResources_coots_multistage_H1"],"VSid"])
if(!nrow(data$VS[is.na(data$VS$VSnumTotal)])==length(VSid_without_VSnumTotal)){
  stop()
}

datasetNames <- unique(data$DE$DEstratumName)

#add all stratums to package data
for(dname in datasetNames){
  #fix the naming removing strange characters
  datasetName <- gsub("[[:punct:]]|[[:space:]]", "_",
                      paste0(dname))
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
