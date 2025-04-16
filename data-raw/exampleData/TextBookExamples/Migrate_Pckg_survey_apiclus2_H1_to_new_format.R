# 12/2/2025
# Update "Pckg_survey_apiclus2_H1" to the new data format

library(RDBEScore)
# Get the data
load(file='./data/Pckg_survey_apiclus2_H1.rda')
validateRDBESDataObject(Pckg_survey_apiclus2_H1, strict = TRUE, verbose = TRUE)
newH1 <- Pckg_survey_apiclus2_H1
validateRDBESDataObject(newH1, strict = TRUE, verbose = TRUE)

# Add an entry for IS and create the entries using SL
newH1$IS <- newH1$SL
newH1$IS <- newH1$IS[,c("SLid","SLrecType","SLcommTaxon","SLsppCode")]
names(newH1$IS) <- c("ISid","ISrecType","IScommTaxon","ISsppCode"  )
#head(newH1$IS)
newH1[["IS"]]$ISrecType <- "IS"
newH1[["IS"]]$SLid <- 47891
newH1[["IS"]]$ISid <- 1001
newH1$IS <- newH1$IS[,c("ISid", "SLid", "ISrecType", "IScommTaxon", "ISsppCode")]
data.table::setkey(newH1[["IS"]],ISid)
# Remove the unneccessary columns from SL
newH1$SL <- newH1$SL[,c("SLid","SLrecType","SLcou","SLinst","SLspeclistName","SLyear","SLcatchFrac")]
# Add column to FT
newH1[["FT"]]$FTdomLanDate <- NA
# Add columns to FO
newH1[["FO"]]$FOfishDuraDatBas <- NA
newH1[["FO"]]$FOgeoDatBas <- NA
newH1[["FO"]]$FOgeoSou <- NA
newH1[["FO"]]$FOgeaDatBas <- NA
newH1[["FO"]]$FOgearSou <- NA
# Add columns to SS
newH1[["SS"]]$SStimeTotalDatBas <- NA
newH1[["SS"]]$SSnumTotalDatBas <- NA
# Add columns to SA
newH1[["SA"]]$SAgeoDatBas <- NA
newH1[["SA"]]$SAgeoSou <- NA
newH1[["SA"]]$SAgeaDatBas <- NA
newH1[["SA"]]$SAgearSou <- NA
newH1[["SA"]]$SAtotWtMeaDatBas <- NA
# Rename cols in BV
newH1[["BV"]]$BVspecType <- newH1[["BV"]]$BVmethod
newH1[["BV"]]$BVanalysisType <- newH1[["BV"]]$BVmeasEquip
newH1[["BV"]]$BVmethod <- NULL
newH1[["BV"]]$BVmeasEquip <- NULL
validateRDBESDataObject(newH1, strict = TRUE, verbose = TRUE)

# Save the data
Pckg_survey_apiclus2_H1 <- newH1
usethis::use_data(Pckg_survey_apiclus2_H1, overwrite = TRUE)

