

# Trying to re-create issue #27

library(RDBEScore)

path_in <- "Q:/mynd/kibi/RDBES/WGRDBES-EST/dnk_data/"


cl <-
  RDBEScore::RDBEScore:::importRDBESDataZIP(paste0(path_in, "HCL_2022_10_12_085635.zip"),
                                     castToCorrectDataTypes = T)
cl <-
  RDBEScore::RDBEScore:::importRDBESDataZIP(paste0(path_in, "HCL_2022_10_12_085635.zip"),
                                     castToCorrectDataTypes = F)

RDBEScore::validateRDBESDataObject(objectToCheck = cl, verbose = T)

# This should be ok now -petri

cl <- RDBEScore::validateRDBESDataObject(objectToCheck = cl, verbose = T)

unique(cl[["CL"]]$CLeconZone)

# Read in CE and test

ce <-
  RDBEScore::RDBEScore:::importRDBESDataZIP(paste0(path_in, "HCE_2022_10_12_085803.zip"),
                                     castToCorrectDataTypes = T)

dat <- ce[["CE"]]

# I get some 'NAs introduced by coercion to integer range.
# It would be nice to know where, but since this is checked in the upload I assume this is correct

ce <-
  RDBEScore::RDBEScore:::importRDBESDataZIP(paste0(path_in, "HCE_2022_10_12_085803.zip"),
                                     castToCorrectDataTypes = F)
as.integer(ce[["CE"]]$CEgTFishHour)
as.numeric(ce[["CE"]]$CEgTFishHour)
as.double(ce[["CE"]]$CEgTFishHour)


as.double(999999999999)
as.double(-999999999999)
as.double(-999999999999)

RDBEScore::validateRDBESDataObject(objectToCheck = ce, verbose = T)

dat <- ce[["CE"]]

str(dat)

x

, 7, 8, 9, 26, 31, 32, 37:49

for (i in c(45)) {
  print(i)
  as.integer(dat[[i]])
}
