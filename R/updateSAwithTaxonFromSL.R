#' Function which changes the value of SAspeCode in SA.
#'
#' Function checks the rank of aphia id in both of tables SA and SL, and tries
#' to replace a more accurate rank in SA with a broader rank from SL.
#' There are 3 possible situations:
#' 1) If the aphiaid id in the SA table is more accurate than in the SL table
#' and the aphiaids are from the same kingdom, phylum, class, order, family,
#' or genus then the aphiaid in the SA table is changed to the aphia id from
#' the SL table.
#' 2) If the aphia Ids are from different kingdom, phylum, class, order, family,
#' or genus then the function retains the original SA species code.
#' 3) If the SL table has a more accurate rank than in the SA table, the
#' function also retains the original SA species code.
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
#' @export
#'
#' @examples
#' \dontrun{
#' myObject <- createRDBESDataObject(input = "WGRDBES-EST/personal/Kasia/vignettes/vignetteData")
#' renameSpeciesSA(RDBESDataObject=myObject,validate,verbose,strict)}

updateSAwithTaxonFromSL <- function(RDBESDataObject,
                            validate = TRUE,
                            verbose = FALSE,
                            strict = TRUE) {
  if (validate) {
    validateRDBESDataObject(RDBESDataObject,
      verbose = verbose,
      strict = strict
    )
  }

  H1_SA <- RDBESDataObject[["SA"]]
  #H1_SL <- fixSLids(RDBESDataObject)$SL
  H1_SL <- RDBESDataObject[["SL"]]
  H1_IS <- fixSLids(RDBESDataObject)$IS
  H1_SS <- RDBESDataObject[["SS"]]

  aphiaRecords <- RDBEScore::wormsAphiaRecord

  # Change the AphiaID to a char (so it is easier to join to the RDBES specCode field)
  aphiaRecords$AphiaID <- as.character(aphiaRecords$AphiaID)

  # 1st
  # Append ahpia records to SA data
  H1_SA_new <- dplyr::left_join(H1_SA,
    aphiaRecords,
    by = c("SAspeCode" = "AphiaID")
  )

  # Append ahpia records to IS data
  #H1_SL$SLsppCode <- as.character(H1_SL$SLsppCode)
  #H1_SL_new <- dplyr::left_join(H1_SL,
  #  aphiaRecords,
  #  by = c("SLsppCode" = "AphiaID")
  #)
  H1_IS$ISsppCode <- as.character(H1_IS$ISsppCode)
  H1_IS_new <- dplyr::left_join(H1_IS,
                                aphiaRecords,
                                by = c("ISsppCode" = "AphiaID")
  )
  # Combine SL with IS
  H1_SL_new <- dplyr::left_join(H1_SL,
                                H1_IS_new,
                                by = "SLid")


  # SS only information about SLspecieslistName
  H1_SS <- H1_SS[, c("SSid", "SLid", "SSspecListName", "SScatchFra")]
  H1_SA_new <- merge(H1_SA_new, H1_SS, by = "SSid")

  # key of species list name and species

  H1_SA_new$SAkey <- paste(H1_SA_new$SLid, H1_SA_new$SAspeCode, H1_SA_new$SSspecListName, H1_SA_new$SScatchFra, sep = "_")

  #H1_SL_new$SLkey <- paste(H1_SL_new$SLid, H1_SL_new$SLsppCode, H1_SL_new$SLspeclistName, H1_SL_new$SLcatchFrac, sep = "_")
  H1_SL_new$SLkey <- paste(H1_SL_new$SLid, H1_SL_new$ISsppCode, H1_SL_new$SLspeclistName, H1_SL_new$SLcatchFrac, sep = "_")

  newest_SA <- data.frame(NULL)

  # unique(H1_SA_new$SSid)->sequence_SSid
  sequence_SSid <- unique(H1_SA_new$SSid)
  # unique(H1_SA_new$SAcatchCat)->sequence_catchCategory

  for (k in sequence_SSid) {
    # subset of all SA by SSid for all catch category!!!
    temp <- H1_SA_new[H1_SA_new$SSid == k, ]
    temp$SAnewSpeciesCode <- NA
    temp$SAnewSpeciesCode <- as.character(temp$SAnewSpeciesCode)
    temp$SAnewSpeciesCodeTaxonRank <- -1
    # check correct list of Species
    if (unique(temp$SSspecListName) %in% H1_SL_new$SLspeclistName) {
      SL_Subset <- H1_SL_new[H1_SL_new$SLspeclistName %in% temp$SSspecListName, ]
      # loopup in with level you have species and species are inside
      temp$speciesExistsInList <- ifelse(temp$SAkey %in% SL_Subset$SLkey, "Y", "N")
      for (i in 1:nrow(temp)) {
        for (j in 1:nrow(SL_Subset)) {
          if (temp[i, c("speciesExistsInList")] == "Y") {
            temp[i, c("SAnewSpeciesCode")] <- temp[i, c("SAspeCode")]
            #if (verbose) print("Species exists in list. No change needed.")
          } else {
            #if (verbose)  print("Species does not exist in list.")
            if ((temp[i, c("taxonRankID")] > SL_Subset[j, c("taxonRankID")]) &
              (SL_Subset[j, c("scientificname")] %in% temp[i, c("kingdom")] |
                SL_Subset[j, c("scientificname")] %in% temp[i, c("phylum")] |
                SL_Subset[j, c("scientificname")] %in% temp[i, c("class")] |
                SL_Subset[j, c("scientificname")] %in% temp[i, c("order")] |
                SL_Subset[j, c("scientificname")] %in% temp[i, c("family")] |
                SL_Subset[j, c("scientificname")] %in% temp[i, c("genus")]
              ) &
              #temp[i, c("SAspeCode")] != SL_Subset[j, c("SLsppCode")]
              temp[i, c("SAspeCode")] != SL_Subset[j, c("ISsppCode")]
            ) {
              #if (verbose) print("SA species rank is higher than SL species rank. Changing species code.")
              # There is the possibility of finding mutiple matches in the SL table - this
              # can lead to updating SAnewSpeciesCode mutiple times.
              # We want to end up with the broadest taxon rank from the SL table at the end
              # of our matching.
              if (is.na(temp[i, c("SAnewSpeciesCode")])){
                # if we don't yet have a new species code, set it to the SL species code
                #temp[i, c("SAnewSpeciesCode")] <- SL_Subset$SLsppCode[j]
                temp[i, c("SAnewSpeciesCode")] <- SL_Subset$ISsppCode[j]
                temp[i, c("SAnewSpeciesCodeTaxonRank")] <- SL_Subset[j, c("taxonRankID")]
                #if (verbose) print("We don't have a new species code yet. Changing species code.")
              } else {
                # check if SAnewSpeciesCodeTaxonRank is greater than SL taxonRankID
                if (temp[i, c("SAnewSpeciesCodeTaxonRank")] > SL_Subset[j, c("taxonRankID")]) {
                  #temp[i, c("SAnewSpeciesCode")] <- SL_Subset$SLsppCode[j]
                  temp[i, c("SAnewSpeciesCode")] <- SL_Subset$ISsppCode[j]
                  temp[i, c("SAnewSpeciesCodeTaxonRank")] <- SL_Subset[j, c("taxonRankID")]
                  #if (verbose) print("New SA species rank is higher than SL species rank. Changing species code again.")
                } else {
                  #if (verbose) print("New SA species rank is lower than SL species rank. Not changing species code.")
                }
              }
            }
            if ((temp[i, c("taxonRankID")] < SL_Subset[j, c("taxonRankID")])) {
              #if (verbose) print("SA species rank is lower than SL species rank. Not changing species code.")
              temp[i, c("SAnewSpeciesCode")] <- temp$SAspeCode[i]
            }
          }
        } # j
        if (is.na(temp[i, c("SAnewSpeciesCode")])) {
          temp$SAnewSpeciesCode <- as.character(temp$SAnewSpeciesCode)
          temp[i, c("SAnewSpeciesCode")] <- temp$SAspeCode[i]
        }
      } # i
      newest_SA <- rbind(newest_SA, temp)
      #if (verbose) print(paste0("Number of rows in newest_SA: ", nrow(newest_SA)))
    }
  } # k


  # find all rows from newest_SA where SAspecCode is different from SAnewSpeciesCode
  changedSpecies <- newest_SA[newest_SA$SAspeCode != newest_SA$SAnewSpeciesCode, ]
  if (verbose) print(paste0("Number of rows changed: ", nrow(changedSpecies)))

  # rename SAspeCode
  newest_SA$SAspeCode <- newest_SA$SAnewSpeciesCode
  x <- colnames(H1_SA)
  newest_SA <- data.table::setDT(newest_SA[, ..x])
  RDBESDataObject[["SA"]] <- newest_SA
  setkey(RDBESDataObject[["SA"]], "SAid")
  RDBESDataObject
}
