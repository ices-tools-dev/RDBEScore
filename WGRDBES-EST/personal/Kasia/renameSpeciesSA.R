#'Function which rename SAspeCode
#'
#' @param RDBESDataObject An RDBESDataObject.
#' @param validate Set to TRUE if you want validation to be carried out. The
#' default if TRUE.
#' @param verbose (Optional) Set to TRUE if you want informative text on validation printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) This function can validate its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return RDBES data object where species in SA were renaming for species
#' occuring in SL for level of species rank. If in SA is
#' Sprat(126425), in SL Clupeidae (125464) function renameSpeciesSA rename Sprat
#' from SA to Clupeidae. Clupeidae(family rank) is higher rank than Sprat(species rank).
#'
#' @example
#' \dontrun{
#' myObject <- createRDBESDataObject(input = "WGRDBES-EST/personal/Kasia/vignettes/vignetteData")
#' renameSpeciesSA(RDBESDataObject=myObject,validate,verbose,strict)}

renameSpeciesSA<-function(RDBESDataObject, validate=TRUE, verbose=FALSE, strict=TRUE){
  if(validate){
    validateRDBESDataObject(RDBESDataObject,
                            verbose = verbose,
                            strict = strict
    )
    }

H1_SA <- RDBESDataObject[["SA"]]
H1_SL <- fixSLids(RDBESDataObject)$SL
H1_SS <- RDBESDataObject[["SS"]]

aphiaRecords <- RDBEScore::wormsAphiaRecord

# Change the AphiaID to a char (so it is easier to join to the RDBES specCode field)
aphiaRecords$AphiaID <- as.character(aphiaRecords$AphiaID)

#1st
# Append ahpia records to SA data
H1_SA_new <- dplyr::left_join(H1_SA,
                              aphiaRecords,
                              by=c("SAspeCode"="AphiaID"))

#Append ahpia records to SL data
H1_SL$SLsppCode<-as.character(H1_SL$SLsppCode)
H1_SL_new <- dplyr::left_join(H1_SL,
                              aphiaRecords,
                              by=c("SLsppCode"="AphiaID"))


#SS only information about SLspecieslistName
H1_SS<-H1_SS[,c('SSid','SLid','SSspecListName','SScatchFra')]
H1_SA_new<-merge(H1_SA_new,H1_SS, by='SSid')

#key of species list name and species

H1_SA_new$SAkey<-paste(H1_SA_new$SLid,H1_SA_new$SAspeCode,H1_SA_new$SSspecListName,H1_SA_new$SScatchFra,sep='_')
H1_SL_new$SLkey<-paste(H1_SL_new$SLid,H1_SL_new$SLsppCode,H1_SL_new$SLspeclistName,H1_SL_new$SLcatchFrac,sep='_')

newest_SA<-data.frame(NULL)

unique(H1_SA_new$SSid)->sequence_SSid
#unique(H1_SA_new$SAcatchCat)->sequence_catchCategory

for (k in sequence_SSid){
    # subset of all SA by SSid for all catch category!!!
    temp<-H1_SA_new[H1_SA_new$SSid==k,]
    temp$SAnewSpeciesCode <- NA
    temp$SAnewSpeciesCode <- as.character( temp$SAnewSpeciesCode)
    #check correct list of Species
    if (unique(temp$SSspecListName)%in%H1_SL_new$SLspeclistName){
      SL_Subset<-H1_SL_new[H1_SL_new$SLspeclistName%in%temp$SSspecListName,]
      #loopup in with level you have species and species are inside
      temp$speciesExistsInList<-ifelse(temp$SAkey %in% SL_Subset$SLkey,'Y','N')
      for (i in 1:nrow(temp)){
        for (j in 1:nrow(SL_Subset)){
          if (temp[i,c('speciesExistsInList')]=='Y'){
            temp[i,c('SAnewSpeciesCode')]<-temp[i,c('SAspeCode')]
          }else{
            if ((temp[i,c('taxonRankID')]>SL_Subset[j,c('taxonRankID')]) &
                (SL_Subset[j,c('scientificname')] %in% temp[i,c('kingdom')]|
                 SL_Subset[j,c('scientificname')] %in% temp[i,c('phylum')]|
                 SL_Subset[j,c('scientificname')] %in% temp[i,c('class')]|
                 SL_Subset[j,c('scientificname')] %in% temp[i,c('order')]|
                 SL_Subset[j,c('scientificname')] %in% temp[i,c('family')]|
                 SL_Subset[j,c('scientificname')] %in% temp[i,c('genus')]
                )&
                temp[i,c('SAspeCode')]!=SL_Subset[j,c('SLsppCode')]
            ){
              temp[i,c('SAnewSpeciesCode')]<-SL_Subset$SLsppCode[j]
            }
            if((temp[i,c('taxonRankID')]<SL_Subset[j,c('taxonRankID')]) ){
              temp[i,c('SAnewSpeciesCode')]<-temp$SAspeCode[i]
            }
          }
        }#j
        if(is.na(temp[i,c('SAnewSpeciesCode')])){
          temp$SAnewSpeciesCode<-as.character(temp$SAnewSpeciesCode)
          temp[i,c('SAnewSpeciesCode')]<-temp$SAspeCode[i]
        }
      }#i
      newest_SA<-rbind(newest_SA,temp)
    }

}#k
#rename SAspeCode
newest_SA$SAspeCode<-newest_SA$SAnewSpeciesCode
x<-colnames(H1_SA)
newest_SA<-data.table::setDT(newest_SA[,..x])
RDBESDataObject[["SA"]] <- newest_SA
setkey(RDBESDataObject[["SA"]],"SAid")
RDBESDataObject
}
