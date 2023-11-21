#' Create a table of RDBES Ids
#'
#' examples for now see
#' https://github.com/ices-eg/WK_RDBES/tree/master/WKRDB-EST2/chairs/Nuno
#'
#' @param x RDBESdataObject
#' @param addSAseqNums should SAseqNum be included? Default value is TRUE
#'
#' @return data frame of Ids of all tables in sampling hierarchy
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'   importRDBESDataCSV(rdbesExtractPath = "tests\\testthat\\h1_v_1_19_13")
#'   
#' myTableOfIds<- createTableOfRDBESIds(myH1RawObject)
#' }

createTableOfRDBESIds<-function(x, addSAseqNums=TRUE)
{

# note: needs developments for different lower hierarchies

# x is RDBESobj
# hierarchy is hierarchy (integer)
# outputs a table with ids for matching 


CStableNames<- getTablesInRDBESHierarchy(hierarchy = x$DE$DEhierarchy[1],
                                      includeOptTables = FALSE,
                                      includeLowHierTables = TRUE,
                                      includeTablesNotInSampHier = FALSE)

for (i in 1:(length(CStableNames)-1))
{
id_1<-paste0(CStableNames[i],"id")
id_2<-paste0(CStableNames[i+1],"id")
if (i==1) df_1<-data.frame(x[[CStableNames[i]]][,list(get(id_1))]); colnames(df_1)<-id_1
if((CStableNames[i+1] == "SA" & addSAseqNums == TRUE) | CStableNames[i+1] %in% c("BV")){
							if(CStableNames[i+1]=="SA") {df_2<-data.frame(x[[CStableNames[i+1]]][,list(get(id_1), get(id_2), get("SAseqNum"), get("SAparSequNum"))]); colnames(df_2)<-c(id_1,id_2,"SAseqNum","SAparSequNum")}	
							if(CStableNames[i+1]=="BV") {df_2<-data.frame(x[[CStableNames[i+1]]][,list(get(id_1), get(id_2), get("BVfishId"))]); colnames(df_2)<-c(id_1,id_2,"BVfishId")}		
							} else {
								df_2<-data.frame(x[[CStableNames[i+1]]][,list(get(id_1), get(id_2))]); colnames(df_2)<-c(id_1,id_2)
								}

if (i==1) out<-merge(df_1,df_2, all.x=T) else out<-merge(out, df_2, all.x=T)

#colnames(out)<-c(id_1,id_2)

}
# reorders
if(addSAseqNums==TRUE){
	out<-out[,c(paste0(CStableNames,"id"),"BVfishId","SAseqNum","SAparSequNum")]
	} else { 
		out<-out[,c(paste0(CStableNames,"id"),"BVfishId")]
					

		}
out
}

# e.g.,
 ## default adds "SAseqNum","SAparSequNum"
 #head(createTableOfRDBESIds(x = RDBESprepObj))
 ## if addSAseqNums is set to FALSE, "SAseqNum" and "SAparSequNum" are not added to output
 # head(createTableOfRDBESIds(x = RDBESprepObj, addSAseqNums=FALSE))
