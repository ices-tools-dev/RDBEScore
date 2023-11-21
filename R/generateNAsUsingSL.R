#' Generate NAs in samples using Species List information
#'
#' @param RDBESDataObject An RDBESDataObject.
#' @param targetAphiaId a vector of aphiaId.
#' @param overwriteSampled (Optional) should SAtotalWtMes and SAsampWtMes be set to 0 if
#' spp recorded but absent from SL? The default is TRUE.
#' @param validate (Optional) Set to TRUE if you want validation to be carried out. The
#' default if TRUE.
#' @param verbose (Optional) Set to TRUE if you want informative text on validation printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) This function can validate its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return RDBES data object where SA was complemented with NAs for species not looked for
#' (sensu in SL)
#' @export
#'
#' @examples
#' # To be added

generateNAsUsingSL<-function(RDBESDataObject, targetAphiaId, overwriteSampled=TRUE, validate = TRUE, verbose=FALSE, strict=TRUE){

if(validate){
  validateRDBESDataObject(RDBESDataObject,
    verbose = verbose,
    strict = strict
  )
}

	  # Take a copy of SA and SL since we'll change some column data types and
	  # we don't want to update the original version
	  tmpSS <- data.table::copy(RDBESDataObject[["SS"]])
	  tmpSL <- data.table::copy(fixSLids(RDBESDataObject)$SL)
	  tmpSA <- data.table::copy(RDBESDataObject[["SA"]])	
			
		tmpSSwithSL<-merge(tmpSS, tmpSL, by="SLid", all.x=T)

		ls1 <- split(tmpSSwithSL, tmpSSwithSL$SSid)
		ls2 <- lapply(ls1, function(tmpSSwithSLrow, targetAphiaId1=targetAphiaId, rdbesSA=tmpSA){
		  # Now convert some columns from int to numeric
			colsToConvertToNumeric <- c("SAid", "SAseqNum")
			rdbesSA[, (colsToConvertToNumeric) := lapply(.SD, as.double),
				.SDcols = colsToConvertToNumeric]
		
		# determines aphias that need generation
		aphiaNeedingGenerateNAs <- targetAphiaId1[!targetAphiaId1 %in% tmpSSwithSLrow$SLcommTaxon]
	
		if(length(aphiaNeedingGenerateNAs)>0)
		{
		# checks if they already exist in SA [case of the exceptional observer]
		inSA <- aphiaNeedingGenerateNAs %in% rdbesSA[SSid == tmpSSwithSLrow$SSid,]$SAspeCode
		if(any(!inSA))
		# creates a new row
		{
		draftNewRows <- do.call("rbind", replicate(n=length(aphiaNeedingGenerateNAs[!inSA]), 
							rdbesSA[SSid == tmpSSwithSLrow$SSid,][1,], simplify = FALSE))

		draftNewRows$SAspeCode <- aphiaNeedingGenerateNAs[!inSA]
		draftNewRows$SAspeCodeFAO <- ""
		draftNewRows$SAsamp <- 'N'
		draftNewRows[,c("SAtotalWtLive","SAsampWtLive","SAtotalWtMes","SAsampWtMes")] <- NA
		draftNewRows$SAid <- max(rdbesSA$SAid) + (1:length(inSA[!inSA]))*0.001 # maintain a count
		draftNewRows$SAseqNum <- max(rdbesSA$SAseqNum) + (1:length(inSA[!inSA]))*0.001 # maintain a count
		draftNewRows$SAunitName <- paste0("NAgen_", max(rdbesSA$SAid) + (1:length(inSA[!inSA]))*0.001) # maintain a count
		# updates the table
			rdbesSA <- rbind(rdbesSA, draftNewRows)
		# checks if spp were added ok
		#browser()
			test_fail <- length(aphiaNeedingGenerateNAs <- targetAphiaId1[!targetAphiaId1 %in% rdbesSA$SAspeCode])>0	
			if(test_fail) stop()
		} 
		
		if(any(inSA)){
			# if they do and overwrite==T give him/her bonus points and but overwrite their data with NAs
			# if they do and overwrite==F give him/her bonus points and and keep their data
			if(overwriteSampled==T) {
				rdbesSA[SSid == tmpSSwithSLrow$SSid & SAspeCode %in% aphiaNeedingGenerateNAs,c("SAtotalWtLive",
															"SAsampWtLive","SAtotalWtMes","SAsampWtMes")] <- NA 
			} else {"do nothing"}}
		}
		#browser()		
		rdbesSA} 	
		)
		RDBESDataObject[["SA"]] <- data.table::setDT(do.call("rbind", ls2))
		setkey(RDBESDataObject[["SA"]],"SAid")
		RDBESDataObject
}	
