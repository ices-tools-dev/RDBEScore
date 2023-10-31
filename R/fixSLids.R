#' Fixes SLid in SL table (facilitating SS-SL joins).
#'
#' @details RDBES SL can be seen as a join of two tables - one that identifies the
#' species list in terms of \emph{SLcou} * \emph{SLinst} * \emph{SLspeclistName} *
#' \emph{SLyear} * \emph{SLcatchFrac} and one that specifies the
#' taxa (\emph{SLcommTaxon} * \emph{SLsppCode}) in the list. In SS, SLid
#' remits to the 1st taxa in a species list and not - as it would be expected -
#' to the species list itself. This function fixes this by creating a new
#' SLtaxaId variable in SL and assigning all taxa in a species to a single SSid.
#'
#' @param RDBESDataObject A valid RDBESDataObject
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#' @param strict (Optional) This function validates its input data - should
#' the validation be strict? The default is TRUE.
#'
#' @return an RDBESDataObject with SL ids reworked
#' @export
#'
#' @examples
#' # To add


fixSLids<-function(RDBESDataObject, verbose = FALSE, strict = TRUE){

  # Check we have a valid RDBESDataObject before doing anything else
  validateRDBESDataObject(RDBESDataObject,
    verbose = verbose,
    strict = strict
  )

# issues error if fixSLids already been run
if("SLtaxaId" %in% colnames(RDBESDataObject[["SL"]])) stop ("SLtaxaId already added - maybe function has already been run?")

# makes a copy
tmpSL <- data.table::copy(RDBESDataObject[["SL"]])

tmpSL <- merge(tmpSL[,list(SLtaxaId=SLid, SLid=SLid[1]), list(SLrecType,SLcou,SLinst,
				SLspeclistName,SLyear,SLcatchFrac)], tmpSL[,c("SLid","SLcommTaxon","SLsppCode")],
					by.x="SLtaxaId", by.y="SLid", all.x=T)

tmpSL<-cbind(tmpSL[,c("SLid","SLtaxaId")], tmpSL[,!c("SLid","SLtaxaId")])

RDBESDataObject[["SL"]]<-tmpSL

data.table::setkeyv(RDBESDataObject[["SL"]], c("SLid","SLtaxaId"))

RDBESDataObject
}

