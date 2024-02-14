library(openxlsx)

rdbesDataModel <- "./data-raw/dataFormat/RDBES Data Model CS.xlsx"
rdbesDataModelVDSL <- "./data-raw/dataFormat/RDBES Data Model VD SL.xlsx"
rdbesDataModelCLCE <- "./data-raw/dataFormat/RDBES Data Model CL CE.xlsx"

outFile <- "./data/mapColNamesFieldR.rds"

mapColNames <-NULL

for (i in 2:14) {

  dat_0 <- read.xlsx(rdbesDataModel, sheet = i)

  tablePrefix <- NA
  if(length(dat_0)>0){
    tablePrefix <- substr(dat_0[1,"Field.Name"],1,2)
  }
  dat_0$Table.Prefix <- tablePrefix

  mapColNames <-
    rbind(mapColNames, dat_0[,c("Table.Prefix","Field.Name", "R.Name","Type")])
}


for (i in 1:2) {
  dat_1 <- read.xlsx(rdbesDataModelVDSL, sheet = i)

  tablePrefix <- NA
  if(length(dat_0)>0){
    tablePrefix <- substr(dat_1[1,"Field.Name"],1,2)
  }
  dat_1$Table.Prefix <- tablePrefix

  mapColNames <-
    rbind(mapColNames, dat_1[,c("Table.Prefix","Field.Name", "R.Name","Type")])
}

for (i in 1:2) {
  dat_2 <- read.xlsx(rdbesDataModelCLCE, sheet = i)

  tablePrefix <- NA
  if(length(dat_0)>0){
    tablePrefix <- substr(dat_2[1,"Field.Name"],1,2)
  }
  dat_2$Table.Prefix <- tablePrefix

  mapColNames <-
    rbind(mapColNames, dat_2[,c("Table.Prefix","Field.Name", "R.Name","Type")])
}

# Get rid of NA field names
mapColNamesFieldR <- mapColNames[!is.na(mapColNames$Field.Name),]

# Create a field to hold the R data type of the field
mapColNamesFieldR$RDataType <- NA

# Fix for two issues
mapColNamesFieldR[mapColNamesFieldR$R.Name == "Clid","Field.Name"] <- "CLid"
mapColNamesFieldR[mapColNamesFieldR$R.Name == "Clid","R.Name"] <- "CLid"

# Fix for missing LEid column in FT table (remove when Excel model doc is updated from v 1.19.18)
# If no match on this then this column is still missing, so needs added
# So at least if model doc is updated and this is not removed then it shouldn't break anything
if(nrow(mapColNamesFieldR[mapColNamesFieldR$Table.Prefix == "FT" & mapColNamesFieldR$Field.Name == "LEid",]) == 0) {
  # this is the row after which the new row needs to be added
  ind <- intersect(which(mapColNamesFieldR$Table.Prefix == "FT"),
                   which(mapColNamesFieldR$Field.Name == "TEid"))

  mapColNamesFieldR <- rbind(mapColNamesFieldR[1:ind,],
                             c("FT", "LEid", "LEid", "Integer", "integer"),
                             mapColNamesFieldR[(ind+1):nrow(mapColNamesFieldR),])

}
# there is currently no hierarchy where LE directly preceeds SA (without SS) so this is not needed:
#this was a bug in the RDBES download part
# Similar fix to above for LEid in the SA table
#if(nrow(mapColNamesFieldR[mapColNamesFieldR$Table.Prefix == "SA" & mapColNamesFieldR$Field.Name == "LEid",]) == 0) {
#  # this is the row after which the new row needs to be added
#  ind <- intersect(which(mapColNamesFieldR$Table.Prefix == "SA"),
#                   which(mapColNamesFieldR$Field.Name == "SSid"))
#
#  mapColNamesFieldR <- rbind(mapColNamesFieldR[1:ind,],
#                             c("SA", "LEid", "LEid", "Integer", "integer"),
#                             mapColNamesFieldR[(ind+1):nrow(mapColNamesFieldR),])
#
#}

# Another similar fix to above for FOid in the LE table
if(nrow(mapColNamesFieldR[mapColNamesFieldR$Table.Prefix == "LE" & mapColNamesFieldR$Field.Name == "FOid",]) == 0) {
  # this is the row after which the new row needs to be added
  ind <- intersect(which(mapColNamesFieldR$Table.Prefix == "LE"),
                   which(mapColNamesFieldR$Field.Name == "VDid"))

  mapColNamesFieldR <- rbind(mapColNamesFieldR[1:ind,],
                             c("LE", "FOid", "FOid", "Integer", "integer"),
                             mapColNamesFieldR[(ind+1):nrow(mapColNamesFieldR),])

  # Need to also change the order to match order in download SA table (which is different than in the Excel doc)
  # DOWNLOAD:                  LEid > OSid > FTid > VSid > TEid > SAid > VDid > FOid > SSid
  # EXCEL (also missing LEid): LEid > OSid > FTid > VSid > VDid > TEid > SAid > SSid
  rownames(mapColNamesFieldR) <- NULL
  mapColNamesFieldR <- rbind(mapColNamesFieldR[1:254,],
                             mapColNamesFieldR[257,], # TEid
                             mapColNamesFieldR[258,], # SAid
                             mapColNamesFieldR[255,], # VDid
                             mapColNamesFieldR[256,], # FOid
                             mapColNamesFieldR[259,], # SSid
                             mapColNamesFieldR[260:nrow(mapColNamesFieldR),])

}




# Set xxID fields to integer data type
mapColNamesFieldR[
  grepl("^..id$",mapColNamesFieldR$Field.Name, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Field.Name) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "integer"

# Set integer fields to integer data type
mapColNamesFieldR[
  grepl("Int",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "integer"

# Set decimal fields to numeric data type
mapColNamesFieldR[
  grepl("Dec",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "numeric"

# Set string fields to character data type
mapColNamesFieldR[
  grepl("Str",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "character"

# Set Date fields to character data type
mapColNamesFieldR[
  grepl("Date",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "character"

# Set Time fields to character data type
mapColNamesFieldR[
  grepl("Time",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "character"

# Set Y/N fields to character data type
mapColNamesFieldR[
  grepl("Y/N",mapColNamesFieldR$Type, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Type) &
    is.na(mapColNamesFieldR$RDataType),"RDataType"] <- "character"

# Change SAid and SAseqNum to be numeric rather than integer -
# this is required when we generate true zeros
mapColNamesFieldR[
  mapColNamesFieldR$Table.Prefix == "SA" &
    mapColNamesFieldR$Field.Name == "SAid","RDataType"] <- "numeric"
mapColNamesFieldR[
  mapColNamesFieldR$Table.Prefix == "SA" &
    mapColNamesFieldR$Field.Name == "SAsequenceNumber","RDataType"] <- "numeric"
mapColNamesFieldR[
  mapColNamesFieldR$Table.Prefix == "SA" &
    mapColNamesFieldR$Field.Name == "SAparentSequenceNumber","RDataType"] <- "numeric"

# Remove any spaces from the R names
mapColNamesFieldR$R.Name <- gsub(" ", "", mapColNamesFieldR$R.Name)

row.names(mapColNamesFieldR) <- NULL # reset row numbers

# Indicate which are the "essential" fields for estimation - these must always
# be present even if we're not being strict when we validate

mapColNamesFieldR$EssentialForEst <- FALSE

# All DE, SD, SS and SL fields are essential
mapColNamesFieldR[
  mapColNamesFieldR$Table.Prefix %in% c("DE","SD","SS","SL"),
                                        "EssentialForEst"] <- TRUE

# xxID fields are essential
mapColNamesFieldR[
  grepl("^..ID$",mapColNamesFieldR$R.Name, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Field.Name),"EssentialForEst"] <- TRUE

# recType fields are essential
mapColNamesFieldR[
  grepl("^..recType$",mapColNamesFieldR$R.Name, ignore.case = TRUE) &
    !is.na(mapColNamesFieldR$Field.Name),"EssentialForEst"] <- TRUE

# design variables are essential
for (aTable in unique(mapColNamesFieldR$Table.Prefix)){
  designVars <- paste0(aTable, RDBEScore::designVariables)
  mapColNamesFieldR[
    mapColNamesFieldR$R.Name %in% designVars,"EssentialForEst"] <- TRUE
}

# FOcatReg is requied by generateMissingSSRows()
mapColNamesFieldR[mapColNamesFieldR$R.Name == "FOcatReg",
                                          "EssentialForEst"] <- TRUE
# Mandatory fields from VD
mapColNamesFieldR[
  mapColNamesFieldR$R.Name %in%
    c("VDencrVessCode","VDyear","VDctry","VDflgCtry","VDlenCat"),
                                          "EssentialForEst"] <- TRUE

# TODO - what else is essential?


# Save the data
usethis::use_data(mapColNamesFieldR, overwrite = TRUE)
