# 15/10/2025
# Update "Pckg_SDAResources_agstrat_H1" to the new data format

library(RDBEScore)
# Get aux data
load(file='./data/Pckg_survey_apiclus2_H1.rda')
# Get the data
load(file='./data/Pckg_SDAResources_agstrat_H1.rda')
validateRDBESDataObject(Pckg_SDAResources_agstrat_H1, strict = FALSE, verbose = TRUE)
newH1 <- Pckg_SDAResources_agstrat_H1

# Add an entry for IS and create the entries using SL
newH1$IS <- readRDS(paste0(baseDir,"aux_TextBookExamples/IS_base.rds"))
names(newH1$IS) <- c("ISrecordType","IScommTaxon","ISsppCode")
#head(newH1$IS)
newH1[["IS"]]$ISrecType <- "IS"
newH1[["IS"]]$SLid <- newH1[["SL"]]$SLid
newH1[["IS"]]$ISid <- 1001
newH1$IS <- newH1$IS[,c("ISid", "SLid", "ISrecType", "IScommTaxon", "ISsppCode")]
data.table::setkey(newH1[["IS"]],ISid)
# Remove the unneccessary columns from SL
#newH1$SL <- newH1$SL[,c("SLid","SLrecType","SLcou","SLinst","SLspeclistName","SLyear","SLcatchFrac")]
# Add column to FT
#newH1[["FT"]]$FTdomLanDate <- NA
# Add columns to FO
#newH1[["FO"]]$FOfishDuraDatBas <- NA
#newH1[["FO"]]$FOgeoDatBas <- NA
#newH1[["FO"]]$FOgeoSou <- NA
#newH1[["FO"]]$FOgeaDatBas <- NA
#newH1[["FO"]]$FOgearSou <- NA
# Add columns to SS
#newH1[["SS"]]$SStimeTotalDatBas <- NA
#newH1[["SS"]]$SSnumTotalDatBas <- NA
# Add columns to SA
#newH1[["SA"]]$SAgeoDatBas <- NA
#newH1[["SA"]]$SAgeoSou <- NA
#newH1[["SA"]]$SAgeaDatBas <- NA
#newH1[["SA"]]$SAgearSou <- NA
#newH1[["SA"]]$SAtotWtMeaDatBas <- NA
# generate empty BV and FM
newH1[["FM"]]<-Pckg_survey_apiclus2_H1$FM
newH1[["BV"]]<-Pckg_survey_apiclus2_H1$BV


validateRDBESDataObject(newH1, strict = TRUE, verbose = TRUE)

# Save the data
Pckg_SDAResources_agstrat_H1 <- newH1
usethis::use_data(Pckg_SDAResources_agstrat_H1, overwrite = TRUE)

