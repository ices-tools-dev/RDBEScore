% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removeBrokenVesselLinks.R
\name{removeBrokenVesselLinks}
\alias{removeBrokenVesselLinks}
\title{Remove rows which are not pointing to a valid VesselDetails (VD) records i.e.
those rows which have a value of VDid that does not exist in the VD table.}
\usage{
removeBrokenVesselLinks(objectToCheck, verbose = FALSE, strict = TRUE)
}
\arguments{
\item{objectToCheck}{an RDBESDataObject.}

\item{verbose}{(Optional) If set to TRUE more detailed text will be printed
out by the function.  Default is TRUE.}

\item{strict}{(Optional) This function validates its input data - should
the validation be strict? The default is TRUE.}
}
\value{
an RDBESDataObject with any records with an invalid VDid removed
}
\description{
Remove rows which are not pointing to a valid VesselDetails (VD) records i.e.
those rows which have a value of VDid that does not exist in the VD table.
}
\examples{
\dontrun{

myH1RawObject <-
  importRDBESDataCSV(rdbesExtractPath = "tests\\\\testthat\\\\h1_v_1_19")
myFields <- c("VDlenCat")
myValues <- c("18-<24")
myFilteredObject <- filterRDBESDataObject(myH1RawObject,
  fieldsToFilter = myFields,
  valuesToFilter = myValues
)
myObjectValidVesselLinks <- removeBrokenVesselLinks(
  objectToCheck = myFilteredObject,
  verbose = FALSE
)
}
}
