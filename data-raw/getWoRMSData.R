library(jsonlite)
library(httr)

# Try and get taxonomic information from WoRMS for aphiaids that are in icesSpecWorms
# Example R code https://www.marinespecies.org/aphia.php?p=webservice&type=r
# Worms Web services https://www.marinespecies.org/rest/

# We will use the AphiaRecordsByAphiaIDs function
AphiaRecordsByAphiaIDs <- "https://www.marinespecies.org/rest/AphiaRecordsByAphiaIDs?"

# How many aphiaids shall we ask for in each request (max is 50 apparently)
maxNumberAphiaIds <- 40

allAphiaIDs <- unique(stats::na.omit(RDBEScore::icesSpecWoRMS$Key))

# Request the records for a set of aphiaids, retrying on failure
getAphiaRecords <- function(AphiaIDs, retries = 3) {
  aphiaURLParam <- paste(paste0("aphiaids%5B%5D=", AphiaIDs), collapse = "&")
  url <- paste0(AphiaRecordsByAphiaIDs, aphiaURLParam)
  for (attempt in seq_len(retries)) {
    responseData <- tryCatch(fromJSON(url), error = function(e) NULL)
    if (is.data.frame(responseData) && nrow(responseData) > 0) {
      # The service returns NULL for aphiaids it can't find, so drop empty rows
      return(responseData[!is.na(responseData$AphiaID), , drop = FALSE])
    }
    Sys.sleep(5)
  }
  NULL
}

# We'll store our results in a list and combine them at the end
# (fill = TRUE in rbindlist means differing columns between requests don't drop rows)
batches <- split(allAphiaIDs, ceiling(seq_along(allAphiaIDs) / maxNumberAphiaIds))
results <- vector("list", length(batches))
for (i in seq_along(batches)) {
  print(i)
  results[[i]] <- getAphiaRecords(batches[[i]])
  # Pause so we don't hammer the Worms server constantly
  Sys.sleep(5)
}
wormsAphiaRecord <- as.data.frame(data.table::rbindlist(results, fill = TRUE))

# Check that every requested aphiaid was returned and retry any that are missing
missingIDs <- setdiff(allAphiaIDs, as.character(wormsAphiaRecord$AphiaID))
if (length(missingIDs) > 0) {
  extra <- do.call(rbind, lapply(missingIDs, function(id) {
    Sys.sleep(1)
    getAphiaRecords(id)
  }))
  if (!is.null(extra)) {
    wormsAphiaRecord <- as.data.frame(data.table::rbindlist(list(wormsAphiaRecord, extra), fill = TRUE))
  }
  missingIDs <- setdiff(allAphiaIDs, as.character(wormsAphiaRecord$AphiaID))
  if (length(missingIDs) > 0) {
    warning("No WoRMS record found for aphiaids: ", paste(missingIDs, collapse = ", "))
  }
}

# Add a date field
wormsAphiaRecord$DateDownloaded <- Sys.Date()

# Save the data
usethis::use_data(wormsAphiaRecord, overwrite = TRUE)

