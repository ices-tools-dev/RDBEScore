% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findAndKillOrphans.R
\name{findAndKillOrphans}
\alias{findAndKillOrphans}
\title{This function finds and removed any orphan records in an
RDBESDataObject.  Normally data that has been downloaded from the RDBES
will not contain orphan records - however if the data is subsequently
filtered it is possible to introduce orphan records.}
\usage{
findAndKillOrphans(objectToCheck, verbose = FALSE, strict = TRUE)
}
\arguments{
\item{objectToCheck}{an RDBESDataObject.}

\item{verbose}{(Optional) If set to TRUE more detailed text will be printed
out by the function.  Default is FALSE.}

\item{strict}{(Optional) This function validates its input data - should
the validation be strict? The default is TRUE.}
}
\value{
an RDBESDataObject with any orphan records removed
}
\description{
This function finds and removed any orphan records in an
RDBESDataObject.  Normally data that has been downloaded from the RDBES
will not contain orphan records - however if the data is subsequently
filtered it is possible to introduce orphan records.
}
\examples{
\dontrun{

myH1RawObject <-
importRDBESDataCSV(rdbesExtractPath = "tests\\\\testthat\\\\h1_v_1_19")
myFields <- c("SDctry","VDctry","VDflgCtry","FTarvLoc")
myValues <- c("ZW","ZWBZH","ZWVFA" )
myFilteredObject <- filterRDBESDataObject(myH1RawObject,
                                        fieldsToFilter = myFields,
                                        valuesToFilter = myValues )
myObjectNoOrphans <- findAndKillOrphans(objectToCheck = myFilteredObject,
                                       verbose = FALSE)
}
}
