#' Generate zeros in samples using Species List information
#'
#' examples for now see
#' https://github.com/ices-eg/WK_RDBES/tree/master/WKRDB-EST2/chairs/Nuno
#'
#' @param x RDBES data frame
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return RDBES data frame where SA was complemented with species looked for
#' (sensu in sampling objectives) but not registered in sample
#' @export
#'


generateZerosUsingSL <- function(x,
                                 verbose = FALSE,
                                 strict = TRUE) {


  #x <-myH1DataObject1
  #verbose <- FALSE
  #strict <- TRUE

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(x, verbose = verbose, strict = strict)

  if (!(nrow(x[["SA"]]) >= 1 && nrow(x[["SL"]]) >= 1)) stop("no SA and/or SL")

  # Take a copy of SA and SL since we'll change some column data types and
  # we don't want to update the original version
  tmpSA <- data.table::copy(x[["SA"]])
  tmpSL <- data.table::copy(x[["SL"]])
  tmpIS <- data.table::copy(x[["IS"]])
  #tmpSL <- dplyr::left_join(tmpSL,tmpIS, by = "SLid")
  #tmpSL <- merge(tmpSL, tmpIS,  by = "SLid", all.x = TRUE )
  tmpSL <- merge(tmpSL, tmpIS,  by = "SLid")
  # Now convert some columns from int to numeric
  colsToConvertToNumeric <- c("SAid", "SAseqNum")
  tmpSA[, (colsToConvertToNumeric) := lapply(.SD, as.double),
        .SDcols = colsToConvertToNumeric]

# create aux id_table
	aux<-createTableOfRDBESIds(x = x, addSAseqNums=FALSE)

	tmpSA$SDctry <- x$SD$SDctry[match(aux$SDid[match(tmpSA$SAid,aux$SAid)], x$SD$SDid)]
	tmpSA$SDinst <- x$SD$SDinst[match(aux$SDid[match(tmpSA$SAid,aux$SAid)], x$SD$SDid)]
	tmpSA$SSspecListName <- x$SS$SSspecListName[match(aux$SSid[match(tmpSA$SAid,aux$SAid)], x$SS$SSid)]
	tmpSA$DEyear <- x$DE$DEyear[match(aux$DEid[match(tmpSA$SAid,aux$SAid)], x$DE$DEid)]
	tmpSA$SScatchFra <- x$SS$SScatchFra[match(aux$SSid[match(tmpSA$SAid,aux$SAid)], x$SS$SSid)]
	tmpSA$SLid<-x$SS$SLid[match(aux$SSid[match(tmpSA$SAid,aux$SAid)], x$SS$SSid)]
	tmpSA$SSuseCalcZero<-x$SS$SSuseCalcZero[match(aux$SSid[match(tmpSA$SAid,aux$SAid)], x$SS$SSid)]
	tmpSA$SLspeclistName<-x$SL$SLspeclistName[match(tmpSA$SLid, x$SL$SLid)]
	tmpSA[ ,tmpKey0 := paste(DEyear, SDctry, SDinst, SSspecListName, SScatchFra)]
	tmpSA[ ,tmpKey1 := paste(DEyear, SDctry, SDinst, SSspecListName, SScatchFra, SAspeCode)]

	# restricts to SSuseCalcZero=='Y'
	tmpSA<-tmpSA[SSuseCalcZero=='Y',]

if(nrow(tmpSA)>0)
{


	# creates tmpKey in SL
	tmpSL[, tmpKey0 := paste(SLyear, SLcou, SLinst, SLspeclistName, SLcatchFrac)]
	#tmpSL[, tmpKey1 := paste(SLyear, SLcou, SLinst, SLspeclistName, SLcatchFrac, SLcommTaxon)]
	tmpSL[, tmpKey1 := paste(SLyear, SLcou, SLinst, SLspeclistName, SLcatchFrac, IScommTaxon)]

  # stop: (rare?) situation still to be considered [multiple SAcatchCat, SAsex, SAlandCat per id]
  if (any(tmpSA[, .N, .(SSid,SAstratumName, SAcatchCat, SAsex, SAlandCat)][
			,.N, .(SSid,SAstratumName)]$N>1)) stop("cannot generateZerosUsingSL because >1 SAcatchCat
								OR SAsex OR SAlandCat in same SSid*SAstratumName: situation
										still to be analyzed - likely you should have them ")

tmpSS <- data.table::copy(x[["SS"]])
tmpSS$tmpKey0<-tmpSL$tmpKey0[match(tmpSS$SLid, tmpSL$SLid)]

# Case 1)
# SS exists and was sampled but there is no child SA record - all SL is added

#print(tmpSS$SSid)
#print(tmpSA$SSid)
 check <- !tmpSS$SSid %in% tmpSA$SSid
 if(any(check))
	for (targetSLid in tmpSS$SLid[check])
	{
	  #print("Got here! Row 80")
	  #print(paste0("targetSLid: ", targetSLid))
	# in the following, the max SAid is determined. That SAid + a decimal will constitute the SAid of the new 0 rows
	# determine upper table (parentIdVar) and its value (parentIdValue)
		# note, the use of which is an attempt to make this hierarchy independent - there may be better forms to achieve this.
		parentIdVar<-c("FOid","TEid","LEid","FTid")[which(!is.na(tmpSS[tmpSS$SLid == targetSLid,c("FOid","TEid","LEid","FTid")]))]
		parentIdValue<-tmpSS[[parentIdVar]][tmpSS$SLid == targetSLid]
		# associates the parentIdVar to tmpSA
		tmpSA$parentIdVar<-x[[gsub("id","",parentIdVar)]][[parentIdVar]][match(parentIdValue, x[[gsub("id","",parentIdVar)]][[parentIdVar]])]
		# find the max SAid to be used
		maxSAid<-max(tmpSA[tmpSA$parentIdVar==parentIdValue,]$SAid)
	# adding the zeros
		# vector of species to add
		#sppToAdd<-tmpSL$SLcommTaxon[tmpSL$SLid %in% tmpSS$SLid[!check]]
		#sppToAdd<-tmpSL$IScommTaxon[tmpSL$SLid %in% tmpSS$SLid[!check]]
		sppToAdd<-tmpSL$IScommTaxon[tmpSL$SLid %in% tmpSS$SLid[check]]
		#print(paste0("sppToAdd: ",sppToAdd))
		# picks up a row to be used as dummy
		dummyRows<-do.call("rbind", replicate(n=length(sppToAdd), tmpSA[SAid == maxSAid,][1,], simplify = FALSE))
		# fills in with NA (some vars will be specified below
		dummyRows[,10:31]<-NA # an alternative here could be "NotAvailable" or "NotApplicable" or source from other tables with assumptions
		# handling of a few specific variables (probably will need some tunning later on)
		dummyRows$SAid <- maxSAid+0.001*c(1:length(sppToAdd))
		dummyRows$SSid <- tmpSS$SSid[tmpSS$SLid == targetSLid]
		dummyRows$SAseqNum <- 1:length(sppToAdd)
		dummyRows$SAunitName <- 1:length(sppToAdd)
		dummyRows$SAstratification <- 'N'
		dummyRows$SAstratumName <- 'U'
		dummyRows$SAspeCode<-sppToAdd
		dummyRows[,c("SAtotalWtLive","SAsampWtLive","SAtotalWtMes","SAsampWtMes","SAspecState")]<-0
        dummyRows$SAnumTotal <- ifelse(dummyRows$SAunitType=="Individuals", 0, dummyRows$SAnumTotal)
        dummyRows$SAnumSamp <- ifelse(dummyRows$SAunitType=="Individuals", 0, dummyRows$SAnumSamp)
		dummyRows$SAselProb <- 1
        dummyRows$SAincProb <- 1
        dummyRows$SAlowHierarchy <- "D"
		dummyRows$SAsamp <- "N"
		dummyRows$SAcatchCat <- tmpSS$SScatchFra[tmpSS$SLid == targetSLid]
        dummyRows$SAsex <- 'U'
		dummyRows$SAstateOfProc <- 'UNK'
		dummyRows$SApres <- 'Unknown'
		dummyRows$SAstateOfProc <- 'Unknown'
		dummyRows$SAspecState <- 'Unknown'
	print(paste0("Adding ",nrow(dummyRows)," rows for SS without SA"))
	#print(dummyRows$SAid)
	tmpSA<-rbind(dummyRows, tmpSA)
	tmpSA[ ,tmpKey1 := paste(DEyear, SDctry, SDinst, SSspecListName, SAcatchCat, SAspeCode)]
	# cleans up parentIdVar
	tmpSA$parentIdVar<-NULL
	}

 # Case 2
 # SS exists and was sampled and there are child SA records but they don't include
 # all the species in SL/IS.  The additional species from SL/IS should be added.

 # For each SS record linked to an SA record
 for (mySSid in sort(unique(tmpSA$SSid))){
   #mySSid <- 227695
   # Get the SA records liked to this SSid
   mySA <- tmpSA[ tmpSA$SSid ==  mySSid ,]
   maxSAid <- max(mySA$SAid)
   mySS <- tmpSS[tmpSS$SSid == mySSid,]
   mySL <- tmpSL[tmpSL$SLid == mySS$SLid,]
   targetSLid <- mySS$SLid
   targetCatchCat <- mySS$SScatchFra
   if (nrow(mySA)>0){
     # See if there are any SL/IS records not in these SA records
     tmpSLNotinSA <- mySL[!(mySL$tmpKey1 %in% mySA$tmpKey1),]
     # If there are some records we will try and add them to SA
     if (nrow(tmpSLNotinSA)>0){
       sppToAdd<-tmpSLNotinSA$IScommTaxon
       # picks up a row to be used as dummy
       dummyRows<-do.call("rbind", replicate(n=length(sppToAdd), mySA[SAid == maxSAid,][1,], simplify = FALSE))
       # fills in with NA (some vars will be specified below
       dummyRows[,10:31]<-NA
       # handling of a few specific variables (probably will need some tunning later on)
       dummyRows$SAid <- maxSAid+0.001*c(1:length(sppToAdd))
       dummyRows$SSid <- mySSid
       dummyRows$SAseqNum <- 1:length(sppToAdd)
       dummyRows$SAunitName <- 1:length(sppToAdd)
       dummyRows$SAstratification <- 'N'
       dummyRows$SAstratumName <- 'U'
       dummyRows$SAspeCode<-sppToAdd
       dummyRows[,c("SAtotalWtLive","SAsampWtLive","SAtotalWtMes","SAsampWtMes","SAspecState")]<-0
       dummyRows$SAnumTotal <- ifelse(dummyRows$SAunitType=="Individuals", 0, dummyRows$SAnumTotal)
       dummyRows$SAnumSamp <- ifelse(dummyRows$SAunitType=="Individuals", 0, dummyRows$SAnumSamp)
       dummyRows$SAselProb <- 1
       dummyRows$SAincProb <- 1
       dummyRows$SAlowHierarchy <- "D"
       dummyRows$SAsamp <- "N"
       dummyRows$SAcatchCat <- targetCatchCat
       dummyRows$SAsex <- 'U'
       dummyRows$SAstateOfProc <- 'UNK'
       dummyRows$SApres <- 'Unknown'
       dummyRows$SAstateOfProc <- 'Unknown'
       dummyRows$SAspecState <- 'Unknown'
       #print(dummyRows)
       print(paste0("Adding ", nrow(dummyRows), " rows for SS with SA, but unmatched spp"))
       tmpSA<-rbind(dummyRows, tmpSA)
       tmpSA[ ,tmpKey1 := paste(DEyear, SDctry, SDinst, SSspecListName, SAcatchCat, SAspeCode)]

     }
   }
 }



# handles remaining (more usual) cases
  ls1 <- split(tmpSA, paste(tmpSA$SSid, tmpSA$SAstratumName))
  #print(ls1)
  ls2 <- lapply(ls1, function(x, z=tmpSS) {
    #print(tmpSL$tmpKey1[tmpSL$tmpKey0==z$tmpKey0[z$SSid==x$SSid]])
    for (w in tmpSL$tmpKey1[tmpSL$tmpKey0==z$tmpKey0[z$SSid==x$SSid]]) {

		if (!is.na(w) & !w %in% tmpSA$tmpKey1) {
		  #print("Got here! Row 137")
		  #print(paste0("w: ",w))
		  #print(tmpSA$tmpKey1)
		   # duplicates SA row
          y <- x[1, ]
  		  # handles the key
		  y$SAspeCode <- as.integer(unlist(strsplit(w," "))[6])
		  # handles the remainder
		  y$SAspeCodeFAO <- NA
		  y$SAstateOfProc <- 'UNK'
		  y$SApres <- 'Unknown'
		  y$SAstateOfProc <- 'Unknown'
		  y$SAspecState <- 'Unknown'
		  y$SAtotalWtLive <- 0
          y$SAsampWtLive <- 0
          y$SAtotalWtMes <- 0
          y$SAsampWtMes <- 0
          y$SAnumTotal <- ifelse(y$SAunitType=="Individuals", 0, y$SAnumTotal)
          y$SAnumSamp <- ifelse(y$SAunitType=="Individuals", 0, y$SAnumSamp)
          y$SAselProb <- 1
          y$SAincProb <- 1
		  y$SAid <- min(x$SAid) - 0.001 # maintain a count
          y$SAseqNum <- min(x$SAseqNum) - 0.001 # maintain a count
          y$SAunitName <- min(x$SAid) - 0.001 # maintain a count
          y$SAsex <- 'U'
          y$SAlowHierarchy <- "D"
          y$SAsamp <- "N"
          x <- rbind(y, x)
          x
        } else {
          x
        }
    }
    x <- x[order(x$SAid, decreasing = F), ]
    # cleans tmpKey1
	x$tmpKey0<-NULL
	x$tmpKey1<-NULL
	x
  })
  x[["SA"]] <- data.table::setDT(do.call("rbind", ls2))

  # delete aux var
colsToDelete<-c("SDctry", "SDinst","SSspecListName","DEyear","SScatchFra","SSuseCalcZero","SLspeclistName","SLid")

	 x[["SA"]][, (colsToDelete) := lapply(.SD, function(x) x<-NULL),
        .SDcols = colsToDelete]
 # Ensure key is set on SA
  setkey(x[["SA"]],SAid)
  # orders
  x[["SA"]] <-  x[["SA"]][order(x[["SA"]]$SSid, x[["SA"]]$SAid, decreasing = F), ]

}
  x
}
