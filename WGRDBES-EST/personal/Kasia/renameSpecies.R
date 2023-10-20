#'Function which rename SAspeciesCode -> SAnewSpeciesCode

#packages
library(data.table)
library(RDBEScore)
library(dplyr)

# data
# myObject <- createRDBESDataObject(input = "./tests/testthat/h1_v_1_19_18")
# myObject[["SA"]]
H1_SA <- read.csv("WGRDBES-EST/personal/Kasia/H1_SA.csv")
H1_SL <- read.csv("WGRDBES-EST/personal/Kasia/H1_SL.csv")
H1_SS <- read.csv("WGRDBES-EST/personal/Kasia/H1_SS.csv")

aphiaRecords <- RDBEScore::wormsAphiaRecord

# Change the AphiaID to a char (so it is easier to join to the RDBES specCode field)
aphiaRecords$AphiaID <- as.character(aphiaRecords$AphiaID)

#1st
# Append ahpia records to SA data
H1_SA_new <- dplyr::left_join(H1_SA,
                              wormsAphiaRecord,
                              by=c("SAspeciesCode"="AphiaID"))

#Append ahpia records to SL data
H1_SL_new <- dplyr::left_join(H1_SL,
                              wormsAphiaRecord,
                              by=c("SLspeciesCode"="AphiaID"))


#SS only information about SLspecieslistName
H1_SS<-H1_SS[,c('SSid','SSspeciesListName')]
H1_SA_new<-merge(H1_SA_new,H1_SS, by='SSid')

#key of species list name and species

H1_SA_new$SAkey<-paste(H1_SA_new$SAspeciesCode,H1_SA_new$SSspeciesListName,sep='_')
H1_SL_new$SLkey<-paste(H1_SL_new$SLspeciesCode,H1_SL_new$SLspeciesListName,sep='_')

newest_SA<-data.frame(NULL)

unique(H1_SA_new$SSid)->sequence_SSid
unique(H1_SA_new$SAcatchCategory)->sequence_catchCategory

for (k in sequence_SSid){ #c(61,62,63,64,65,66,67)
  for (l in sequence_catchCategory){
    # subset of all SA by SSid for all catch category!!! (remember not only of Landing)
    ex1<-H1_SA_new[H1_SA_new$SSid==k & H1_SA_new$SAcatchCategory==l,]
    ex1$SAnewSpeciesCode <- NA
    #check correct list of Species
    if (unique(ex1$SSspeciesListName)%in%H1_SL_new$SLspeciesListName){
      SL_Subset<-H1_SL_new[H1_SL_new$SLspeciesListName%in%ex1$SSspeciesListName,]
      #loopup in with level you have species and species are inside
      ex1$speciesExistsInList<-ifelse(ex1$SAkey %in% SL_Subset$SLkey,'Y','N')
      for (i in 1:nrow(ex1)){
        for (j in 1:nrow(SL_Subset)){
          if (ex1[i,c('speciesExistsInList')]=='Y'){
            ex1[i,c('SAnewSpeciesCode')]<-ex1[i,c('SAspeciesCode')]
          }else{
            if ((ex1[i,c('taxonRankID')]>SL_Subset[j,c('taxonRankID')]) &
                (SL_Subset[j,c('scientificname')] %in% ex1[i,c('kingdom')]|
                 SL_Subset[j,c('scientificname')] %in% ex1[i,c('phylum')]|
                 SL_Subset[j,c('scientificname')] %in% ex1[i,c('class')]|
                 SL_Subset[j,c('scientificname')] %in% ex1[i,c('order')]|
                 SL_Subset[j,c('scientificname')] %in% ex1[i,c('family')]|
                 SL_Subset[j,c('scientificname')] %in% ex1[i,c('genus')]
                )&
                ex1[i,c('SAspeciesCode')]!=SL_Subset[j,c('SLspeciesCode')]
            ){
              ex1[i,c('SAnewSpeciesCode')]<-SL_Subset$SLspeciesCode[j]
            }
            if((ex1[i,c('taxonRankID')]<SL_Subset[j,c('taxonRankID')]) ){
              ex1[i,c('SAnewSpeciesCode')]<-ex1$SAspeciesCode[i]
            }
          }
        }#j
        if(is.na(ex1[i,c('SAnewSpeciesCode')])){
          ex1[i,c('SAnewSpeciesCode')]<-ex1$SAspeciesCode[i]
        }
      }#i
      newest_SA<-rbind(newest_SA,ex1)
    }
  }#l
}#k
SA<-newest_SA
SA<-SA[,c(names(H1_SA),'SAnewSpeciesCode')]
