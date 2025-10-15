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
#'
#' @examples
#' \dontrun{
#'
#' myH1RawObject <-
#'   importRDBESDataCSV(rdbesExtractPath = "tests\\testthat\\h1_v_1_19_13")
#'
#' myTableOfIds<- createTableOfRDBESIds(myH1RawObject)
#' }

createTableOfRDBESIds<-function(x, addSAseqNums=TRUE){

# note: needs developments for different lower hierarchies

# x is RDBESobj
# hierarchy is hierarchy (integer)
# outputs a table with ids for matching

  
  CStableNames<- getTablesInRDBESHierarchy(hierarchy = x$DE$DEhierarchy[1],
                                        includeOptTables = FALSE,
                                        includeLowHierTables = TRUE,
                                        includeTablesNotInSampHier = FALSE)
  
  for (i in 1:(length(CStableNames)-1)){
    cat("Processing", CStableNames[i], "table. \n")
    cat("Merging", CStableNames[i], " with ", CStableNames[i+1], " tables. \n")
    id_1<-paste0(CStableNames[i],"id")
    id_2<-paste0(CStableNames[i+1],"id")
    
    if(i==1){
      cat("Using", id_1, " from ", CStableNames[i], "table and", id_2, " from ", CStableNames[i+1], "table. \n")
      df_1<-data.frame(x[[CStableNames[i]]][,list(get(id_1))]); colnames(df_1)<-id_1
    }
    
    if((CStableNames[i+1] == "SA" & addSAseqNums == TRUE) | CStableNames[i+1] %in% c("BV")){
      
  		if(CStableNames[i+1]=="SA"){
  		  
  		  cat("Using", id_1, " from ", CStableNames[i], "table and", id_2, "SAseqNum, and SAparSequNum from ", CStableNames[i+1], "table. \n")
  		  df_2<-data.frame(x[[CStableNames[i+1]]][,list(get(id_1), get(id_2), get("SAseqNum"), get("SAparSequNum"))]); colnames(df_2)<-c(id_1,id_2,"SAseqNum","SAparSequNum")
  		  
  		}
      
    	if(CStableNames[i+1]=="BV"){
    	  
    	  cat("Using", id_1, " from ", CStableNames[i], "table and", id_2, "and BVfishId from ", CStableNames[i+1], "table. \n")
    	  df_2<-data.frame(x[[CStableNames[i+1]]][,list(get(id_1), get(id_2), get("BVfishId"))]); colnames(df_2)<-c(id_1,id_2,"BVfishId")
    	  # We also need a reference reporting the SAid, for when the lower hierarchy is C, see below. 
    	  df_2C<-data.frame(rdbesobj[[CStableNames[i+1]]][,list(get("SAid"), get(id_1), get(id_2))]); colnames(df_2C)<-c("SAid", id_1,id_2)
    	  
    	}
    							
    }else{
    		
      cat("Using", id_1, " from ", CStableNames[i], "table and", id_2, " from ", CStableNames[i+1], "table. \n")
      df_2<-data.frame(x[[CStableNames[i+1]]][,list(get(id_1), get(id_2))]); colnames(df_2)<-c(id_1,id_2)
    	
      }
  
    if (i==1){
    
      out<-merge(df_1,df_2, all.x=T) else out<-merge(out, df_2, all.x=T)
  
    }else{
    
      if(CStableNames[i+1]=="BV"){
      
        # The lower hierarchy (A:D) implies that FM is used or not. 
        # Due to this, we need to do a conditional merging by either FMid (where present) or SAid. 
        outTmp = merge(out, rdbesobj$SA[,c("SAid","SAlowHierarchy")])
        
        # Filter the database and merge conditionally on SAlowHierarchy
        keepA <- outTmp %>% dplyr::filter(SAlowHierarchy == "A") %>% pull(SAid)
        keepB <- outTmp %>% dplyr::filter(SAlowHierarchy == "B") %>% pull(SAid)
        keepC <- outTmp %>% dplyr::filter(SAlowHierarchy == "C") %>% pull(SAid)
        keepD <- outTmp %>% dplyr::filter(SAlowHierarchy == "D") %>% pull(SAid)
        
        toMergeA <- out %>% dplyr::filter(SAid %in% keepA)
        toMergeB <- out %>% dplyr::filter(SAid %in% keepB)
        toMergeC <- out %>% dplyr::filter(SAid %in% keepC)
        toMergeD <- out %>% dplyr::filter(SAid %in% keepD)
        
        # The reasoning is based on https://vocab.ices.dk/?ref=1598
        mergedA <- merge(toMergeA, df_2, by = "FMid", all.x = T)
        mergedB <- merge(toMergeB, df_2, by = "FMid", all.x = T)
        mergedC <- merge(toMergeC, df_2C[c("SAid", "BVid")], by = "SAid", all.x = T)
        mergedD <- toMergeD
        
        out <- bind_rows(
          mergedA, 
          mergedB, 
          mergedC, 
          mergedD
        )
        
      }else{
        
        out<-merge(out, df_2, all.x=T)
        
      }
    
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
