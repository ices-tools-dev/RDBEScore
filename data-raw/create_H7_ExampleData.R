# AI-assisted: Yes
# Review: pending
## Load the csv files from our data-raw
ddir <- "./data-raw/exampleData/"
dataDate <- '2025_10_13'
# Change dataDate to today's YYYY_MM_DD to download a fresh combined export.
zipFile <- file.path(ddir, paste0('H7_', dataDate, '.zip'))
legacyFile <- file.path(ddir, paste0('H7_', dataDate, '.rds'))

#this data is created by Richard Meitern (contact: richard.meitern@ut.ee)
# internally it resembles Estonian onshore sampling data
if (file.exists(zipFile)) {
  H7Example <- RDBEScore::createRDBESDataObject(zipFile, castToCorrectDataTypes = TRUE)
} else if (file.exists(legacyFile)) {
  H7Example <- readRDS(legacyFile)
} else {
  if (dataDate != format(Sys.time(), '%Y_%m_%d', tz = 'Europe/Tallinn')) {
    stop('Missing historical input. Set dataDate to today to download current data.')
  }
  source('data-raw/downloadExampleData.R')
  zipFile <- downloadExampleData(7, 2022, country = 'EE',
                                  samplingScheme = 'OnShoreCommercialMarkets', ddir = ddir)
  H7Example <- RDBEScore::createRDBESDataObject(zipFile, castToCorrectDataTypes = TRUE)
}

H7Example$VD <- NULL
H7Example$DE$DEsampScheme <- gsub("EST_","", H7Example$DE$DEsampScheme)
H7Example$SL <- H7Example$SL[H7Example$SL$SLspeclistName %in%
                               unique(H7Example$SS$SSspecListName),]

H7Example$IS <- H7Example$IS[H7Example$IS$SLid %in% H7Example$SL$SLid,]
#add 0 row VD
H7Example$VD <- data.table::copy(H1Example$VD[F,])


RDBEScore::validateRDBESDataObject(H7Example)
# Save the data
usethis::use_data(H7Example, overwrite = TRUE)
