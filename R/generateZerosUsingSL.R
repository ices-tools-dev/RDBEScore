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

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(x, verbose = verbose, strict = strict)

  if (!(nrow(x[["SA"]]) >= 1 && nrow(x[["SL"]]) >= 1)) stop("no SA and/or SL")

  # Take a copy of SA and SL since we'll change some column data types and
  # we don't want to update the original version
  tmpSA <- data.table::copy(x[["SA"]])
  SArowCountBefore <- nrow(tmpSA)
  tmpSL <- data.table::copy(x[["SL"]])
  tmpIS <- data.table::copy(x[["IS"]])
  tmpSL <- merge(tmpSL, tmpIS,  by = "SLid")
  # Now convert some columns from int to numeric
  colsToConvertToNumeric <- c("SAid", "SAseqNum")
  tmpSA[, (colsToConvertToNumeric) := lapply(.SD, as.double),
        .SDcols = colsToConvertToNumeric]

  if(nrow(tmpSA)>0)
  {

    # stop: (rare?) situation still to be considered [multiple SAcatchCat, SAsex, SAlandCat per id]
    if (any(tmpSA[, .N, .(SSid,SAstratumName, SAcatchCat, SAsex, SAlandCat)][
  			,.N, .(SSid,SAstratumName)]$N>1)) stop("cannot generateZerosUsingSL because >1 SAcatchCat
  								OR SAsex OR SAlandCat in same SSid*SAstratumName: situation
  										still to be analyzed - likely you should have them ")

    tmpSS <- data.table::copy(x[["SS"]])
    # Only consider SS rows that can be used to calculate zero
    tmpSS<-tmpSS[SSuseCalcZero=='Y',]

    if (nrow(tmpSS)> 0){

      # Main loop to use SL/IS to add zero rows to SA
      for (i in 1:nrow(tmpSS))
      {
        mySS <- tmpSS[i,]
        targetSLid <- mySS$SLid

        # in the following, the max SAid is determined. That SAid + a decimal will constitute the SAid of the new 0 rows
        # determine upper table (parentIdVar) and its value (parentIdValue)
        # note, the use of which is an attempt to make this hierarchy independent - there may be better forms to achieve this.
        parentIdVar<-c("FOid","TEid","LEid","FTid")[which(!is.na(tmpSS[tmpSS$SLid == targetSLid,c("FOid","TEid","LEid","FTid")]))]
        parentIdValue<-tmpSS[[parentIdVar]][tmpSS$SLid == targetSLid]
        # associates the parentIdVar to tmpSA
        tmpSA$parentIdVar<-x[[gsub("id","",parentIdVar)]][[parentIdVar]][match(parentIdValue, x[[gsub("id","",parentIdVar)]][[parentIdVar]])]
        # find the max SAid to be used
        maxSAid<-max(tmpSA[tmpSA$parentIdVar==parentIdValue,]$SAid)

        #  Check which species/fraction are present in SA
        SApresent <- tmpSA[tmpSA$SSid == mySS$SSid,c("SAcatchCat","SAspeCode")]
        # For each "Catch" row also add a Lan and Dis row
        LanToAdd <- SApresent[SApresent$SAcatchCat == "Catch",]
        LanToAdd$SAcatchCat <- "Lan"
        SApresent <- rbind(SApresent, LanToAdd)
        DisToAdd <- SApresent[SApresent$SAcatchCat == "Catch",]
        DisToAdd$SAcatchCat <- "Dis"
        SApresent <- rbind(SApresent, DisToAdd)
        # Now get rid of the Catch rows
        SApresent <- SApresent[SApresent$SAcatchCat != "Catch",]
        SApresent$SArowNum <- seq.int(nrow(SApresent))

        # Check which species/fraction should be present from SL
        sppFromSL <- tmpSL[tmpSL$SLid == targetSLid,c("SLcatchFrac","IScommTaxon")]
        sppFromSL$IScommTaxon <- as.character(sppFromSL$IScommTaxon)
        # For each "Catch" row also add a Lan and Dis row
        LanToAdd2 <- sppFromSL[sppFromSL$SLcatchFrac == "Catch",]
        LanToAdd2$SLcatchFrac <- "Lan"
        sppFromSL <- rbind(sppFromSL, LanToAdd2)
        DisToAdd2 <- sppFromSL[sppFromSL$SLcatchFrac == "Catch",]
        DisToAdd2$SLcatchFrac <- "Dis"
        sppFromSL <- rbind(sppFromSL, DisToAdd2)
        # Now get rid of the Catch rows
        sppFromSL <- sppFromSL[sppFromSL$SLcatchFrac != "Catch",]
        sppFromSL$SLrowNum <- seq.int(nrow(sppFromSL))

        # See which species/fraction need adding
        sppToAdd <- dplyr::left_join(sppFromSL, SApresent,
                                  by=c("SLcatchFrac" = "SAcatchCat",
                                       "IScommTaxon" = "SAspeCode"))
        sppToAdd <- sppToAdd[is.na(sppToAdd$SArowNum),]
        sppToAdd <- sppToAdd[,c("SLcatchFrac","IScommTaxon")]
        sppToAdd <- unique(sppToAdd)

        # Add rows (if we need to)
        if (nrow(sppToAdd) >0) {
          # picks up a row to be used as dummy
          dummyRows<-do.call("rbind", replicate(n=nrow(sppToAdd), tmpSA[SAid == maxSAid,][1,], simplify = FALSE))
          # fills in with NA (some vars will be specified below
          dummyRows[,10:31]<-NA # an alternative here could be "NotAvailable" or "NotApplicable" or source from other tables with assumptions
          # handling of a few specific variables (probably will need some tunning later on)
          dummyRows$SAid <- maxSAid+0.001*c(1:nrow(sppToAdd))
          dummyRows$SSid <- tmpSS$SSid[tmpSS$SLid == targetSLid]
          dummyRows$SAseqNum <- 1:nrow(sppToAdd)
          dummyRows$SAunitName <- 1:nrow(sppToAdd)
          dummyRows$SAstratification <- 'N'
          dummyRows$SAstratumName <- 'U'
          dummyRows$SAspeCode<-sppToAdd$IScommTaxon
          dummyRows[,c("SAtotalWtLive","SAsampWtLive","SAtotalWtMes","SAsampWtMes","SAspecState")]<-0
          dummyRows$SAnumTotal <- ifelse(dummyRows$SAunitType=="Individuals", 0, dummyRows$SAnumTotal)
          dummyRows$SAnumSamp <- ifelse(dummyRows$SAunitType=="Individuals", 0, dummyRows$SAnumSamp)
          dummyRows$SAselProb <- 1
          dummyRows$SAincProb <- 1
          dummyRows$SAlowHierarchy <- "D"
          dummyRows$SAsamp <- "N"
          dummyRows$SAcatchCat <- sppToAdd$SLcatchFrac
          dummyRows$SAsex <- 'U'
          dummyRows$SAstateOfProc <- 'UNK'
          dummyRows$SApres <- 'Unknown'
          dummyRows$SAstateOfProc <- 'Unknown'
          dummyRows$SAspecState <- 'Unknown'

          # Add the new rows to the data
          tmpSA<-rbind(dummyRows, tmpSA)
          #tmpSA[ ,tmpKey1 := paste(DEyear, SDctry, SDinst, SSspecListName, SAcatchCat, SAspeCode)]
          # cleans up parentIdVar
          tmpSA$parentIdVar<-NULL
        }
      }
    }

    SArowCountAfter <- nrow(tmpSA)

    if (verbose){
      print(paste0("Added ",SArowCountAfter-SArowCountBefore, " rows to SA"))
    }

  }

  # return the data with any new SA rows added
	x[["SA"]] <- tmpSA
	setkey(x[["SA"]],SAid)
	x
}
