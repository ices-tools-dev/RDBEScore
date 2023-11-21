library(jsonlite)
library(httr)

# Try and get taxonomic information from WoRMS for aphiaids that are in icesSpecWorms
# Example R code https://www.marinespecies.org/aphia.php?p=webservice&type=r
# Worms Web services https://www.marinespecies.org/rest/

# We will use the AphiaRecordsByAphiaIDs function
AphiaRecordsByAphiaIDs <- "https://www.marinespecies.org/rest/AphiaRecordsByAphiaIDs?"

# How many aphiaids shall we ask for in each request (max is 50 apparently)
maxNumberAphiaIds <- 40
# Work out how many times we need to call the web services
totalCalls <- nrow(RDBEScore::icesSpecWoRMS)%/%maxNumberAphiaIds
totalCalls <- totalCalls + 1

# We'll store our results in wormsAphiaRecord
wormsAphiaRecord <- NULL

# For testing
#totalCalls <-3

for(i in 1:(totalCalls)){

  startIndex <- ((i-1)*maxNumberAphiaIds)+1
  endIndex <- startIndex + (maxNumberAphiaIds-1)
  print(i)
  #print(startIndex)
  #print(endIndex)

  # Get a subset of the aphiaids from ICES
  AphiaIDs<-RDBEScore::icesSpecWoRMS[startIndex:endIndex,"Key"]
  # Remove any NAs (should only be relevent when we're at the end of the vector)
  AphiaIDs<-AphiaIDs[!is.na(AphiaIDs)]

  # Format the aphiaids to use as URL parameters
  aphiaURLParam <- paste0("aphiaids%5B%5D=",AphiaIDs)
  aphiaURLParam <- paste(aphiaURLParam, collapse ="&")
  #print(aphiaURLParam)

  #Build the URL to get the data from
  url <- paste0(AphiaRecordsByAphiaIDs,aphiaURLParam);
  #print(url)

  # Get the actual data from the URL
  responseData <- fromJSON(url)

  # Store our results
  if(is.null(wormsAphiaRecord)){
    wormsAphiaRecord <- responseData
  } else {
    wormsAphiaRecord <- rbind(wormsAphiaRecord,responseData)
  }

  # Pause so we don't hammer the Worms server constantly
  Sys.sleep(5)

}

# Add a date field
wormsAphiaRecord$DateDownloaded <- Sys.Date()

# Save the data
usethis::use_data(wormsAphiaRecord, overwrite = TRUE)

