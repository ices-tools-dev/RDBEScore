
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

  # x is x
  # hierarchy is hierarchy (integer)
  # outputs a table with ids for matching

  #libraries
  require(data.table)

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
        df_2C<-data.frame(x[[CStableNames[i+1]]][,list(get("SAid"), get(id_1), get(id_2))]); colnames(df_2C)<-c("SAid", id_1,id_2)

      }

    }else{

      cat("Using", id_1, " from ", CStableNames[i], "table and", id_2, " from ", CStableNames[i+1], "table. \n")
      df_2<-data.frame(x[[CStableNames[i+1]]][,list(get(id_1), get(id_2))]); colnames(df_2)<-c(id_1,id_2)

    }

    if (i==1){

      out<-merge(df_1,df_2, all.x=T)

    }else{

      if(CStableNames[i+1]=="BV"){

        # The lower hierarchy (A:D) implies that FM is used or not.
        # Due to this, we need to do a conditional merging by either FMid (where present) or SAid.
        outTmp = merge(out, x$SA[,c("SAid","SAlowHierarchy")])

        # Convert to data.table if not already
        setDT(outTmp)
        setDT(out)
        setDT(df_2)
        setDT(df_2C)

        # Filter and get SAid groups
        keepA <- outTmp[SAlowHierarchy == "A", SAid]
        keepB <- outTmp[SAlowHierarchy == "B", SAid]
        keepC <- outTmp[SAlowHierarchy == "C", SAid]
        keepD <- outTmp[SAlowHierarchy == "D", SAid]

        # Split 'out' accordingly
        toMergeA <- out[SAid %in% keepA]
        toMergeB <- out[SAid %in% keepB]
        toMergeC <- out[SAid %in% keepC]
        toMergeD <- out[SAid %in% keepD]

        # Conditional merges
        mergedA <- df_2[toMergeA, on = "FMid"]              # left join equivalent (all.x = TRUE)
        mergedB <- df_2[toMergeB, on = "FMid"]              # same for group B
        mergedC <- df_2C[toMergeC, on = "SAid", nomatch = 0][, .SD, .SDcols = c(names(toMergeC), "BVid")]
        mergedD <- toMergeD                                 # unchanged group D

        # Combine back
        out <- rbindlist(list(mergedA, mergedB, mergedC, mergedD), use.names = TRUE, fill = TRUE)

      }else{

        out<-merge(out, df_2, all.x=T)

      }

    }
  }

  out <- as.data.frame(out)

    # reorders
  if(addSAseqNums==TRUE){

    out<-out[,c(paste0(CStableNames,"id"),"BVfishId","SAseqNum","SAparSequNum")]

  } else {

    out<-out[,c(paste0(CStableNames,"id"),"BVfishId")]

  }

  out

}
