#' Export Estimation Results to InterCatch Exchange Format
#'
#'This function transforms the estimation results into the InterCatch format.
#'
#' @param dataToExport A data frame containing the estimation results -
#' this should include the output from the doEstimationForAllStrata function
#'  and already have the the InterCatch columns present.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#'
#' @return A character vector representing the flattened InterCatch exchange
#'  format. The vector includes all fields from the HI, SI, and SD components,
#'  ordered by their associated keys, and is suitable for writing to an
#'  InterCatch-formatted exchange file.
#' @export
#'
exportEstimationResultsToInterCatchFormat <- function(dataToExport,
                                                      verbose = FALSE){

  # HI
  # create a data frame called HIdefinitions with 2 columns: "Name" and "Type"
  HIdefinitions <- data.frame(Name = c("Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","UnitEffort","Effort","AreaQualifier"),
                              Type = c("character","character","character","integer","character","character","character","character","character","integer","character"),
                              Mandatory = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE))
  HIcols <- HIdefinitions$Name

  # SI
  # create a data frame called SIdefinitions with 2 columns: "Name" and "Type"
  SIdefinitions <- data.frame(Name = c("Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","DataToFrom","Usage","SamplesOrigin","QualityFlag","UnitCATON","CATON","OffLandings","varCATON","InfoFleet","InfoStockCoordinator","InfoGeneral"),
                              Type = c("character","character","character","integer","character","character","character","character","character","character","character","character","character","character","character","character","character","numeric","integer","numeric","character","character","character"),
                              Mandatory = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE))
  SIcols <- SIdefinitions$Name

  # SD
  # create a data frame called SDdefinitions with 2 columns: "Name" and "Type"
  SDdefinitions <- data.frame(Name = c('Country','Year','SeasonType','Season','Fleet','AreaType','FishingArea','DepthRange','Species','Stock','CatchCategory','ReportingCategory','Sex','CANUMtype','AgeLength','PlusGroup','SampledCatch','NumSamplesLngt','NumLngtMeas','NumSamplesAge','NumAgeMeas','unitMeanWeight','unitCANUM','UnitAgeOrLength','UnitMeanLength','Maturity','NumberCaught','MeanWeight','MeanLength','varNumLanded','varWgtLanded','varLgtLanded'),
                              Type = c('character','character','character','integer','character','character','character','character','character','character','character','character','character','character','integer','integer','integer','integer','integer','integer','integer','character','character','character','character','character','numeric','numeric','numeric','numeric','numeric','numeric'),
                              Mandatory = c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE))
  SDcols <- SDdefinitions$Name

  # TODO- create SD data (if required)
  # find the values of SIcols that are in the column names of dataToExport
  SDdata <- dataToExport[,intersect(SDcols,names(dataToExport))]
  SD <- createIC_SD(SDdata, SDdefinitions, verbose)

  # find the values of SIcols that are in the column names of dataToExport
  SIdata <- dataToExport[,intersect(SIcols,names(dataToExport))]
  # remove any duplicates from SIdata
  SIdata <- unique(SIdata)

  SI <- createIC_SI(SIdata, SIdefinitions, verbose)

  # find the values of HIcols that are in the column names of dataToExport
  HIdata <- dataToExport[,intersect(HIcols,names(dataToExport))]
  # remove any duplicates from HIdata
  HIdata <- unique(HIdata)

  HI <- createIC_HI(HIdata, HIdefinitions, verbose)

  IC <- rbind(HI[,c("Key","Value")], SI[,c("Key","Value")], SD[,c("Key","Value")])
  # sort IC by the Key column
  IC <- IC[order(IC$Key),]
  # remove the Key column from IC
  IC <- IC$Value

  return(IC)

}

# Function to create HI format data
createIC_HI <- function(HIdata,
                        HIdefinitions,
                        verbose = FALSE){

  HI <- createIC_subtype(HIdata, HIdefinitions,"HI", verbose)

  # create a new column in HI called "Key" - its value will be the following columns concatenated together (seperated by "_") "Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange"
  HI$Key <- paste(HI$Country,HI$Year,HI$SeasonType,HI$Season,HI$Fleet,HI$AreaType,HI$FishingArea,HI$DepthRange, sep = "_")

  return(HI)
}

# Function to create SI format data
createIC_SI <- function(SIdata,
                        SIdefinitions,
                        verbose = FALSE){

  SI <- createIC_subtype(SIdata, SIdefinitions,"SI", verbose)

  # create a new column in SI called "Key" - its value will be the following columns concatenated together (seperated by "_") "Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange"
  SI$Key <- paste(SI$Country,SI$Year,SI$SeasonType,SI$Season,SI$Fleet,SI$AreaType,SI$FishingArea,SI$DepthRange,SI$Species,SI$Stock,SI$CatchCategory,SI$ReportingCategory, sep = "_")

  return(SI)
}


# Function to create SD format data
createIC_SD <- function(SDdata,
                        SDdefinitions,
                        verbose = FALSE){

  SD <- createIC_subtype(SDdata, SDdefinitions,"SD", verbose)

  # create a new column in SD called "Key" - its value will be the following columns concatenated together (seperated by "_") "Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange"
  SD$Key <- paste(SD$Country,SD$Year,SD$SeasonType,SD$Season,SD$Fleet,SD$AreaType,SD$FishingArea,SD$DepthRange,SD$Species,SD$Stock,SD$CatchCategory,SD$ReportingCategory,SD$Sex, SD$CANUMType, SD$AgeLength, sep = "_")

  return(SD)
}

# Base function used by HI, SI, and SD functions to create the relevent data
createIC_subtype <- function(subtypeData,
                             subtypeDefinitions,
                             subtypeName,
                             verbose = FALSE){

  numRows <- nrow(subtypeData)
  recordType <- rep(subtypeName, numRows)
  st <- data.table::data.table(recordType)

  if (verbose) print(paste0("Trying to create ",numRows," rows of ",subtypeName," data"))

  # loop through subtypeDefinitions and add parameter values to st
  for(i in 1:nrow(subtypeDefinitions)){
    # check if the column name is in the column names of subtypeData
    if(subtypeDefinitions$Name[i] %in% names(subtypeData)){
      myParam <- data.frame(subtypeData[[subtypeDefinitions$Name[i]]])
    } else {
      if (subtypeDefinitions$Mandatory[i]){
        warning(paste0("The column ", subtypeDefinitions$Name[i], " is mandatory but is not present in the ",subtypeName," data\n"))
      } else if (verbose){
        print(paste0("The column ", subtypeDefinitions$Name[i], " is not present in the ",subtypeName," data"))
      }
      # if it isn't, create a column of NAs or -9s
      if(subtypeDefinitions$Type[i] == "character"){
        myParam <- data.frame(rep(NA, numRows))
      } else {
        myParam <- data.frame(rep(-9, numRows))
      }
    }
    names(myParam) <- subtypeDefinitions$Name[i]
    # change any NAs in myParam to "NA"
    myParam[is.na(myParam)] <- "NA"
    st <- cbind(st, myParam)
  }

  # create a new column in st called "Value" - it will have the values from all other columns concatenated together with a comma seperator
  st$Value <- apply(st, 1, function(x) paste(x, collapse = ","))

  return(st)

}
