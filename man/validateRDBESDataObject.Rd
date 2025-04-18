% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validateRDBESDataObject.R
\name{validateRDBESDataObject}
\alias{validateRDBESDataObject}
\alias{checkRDBESDataObject}
\title{Check Whether an \code{RDBESDataObject} is in a Valid Format}
\usage{
validateRDBESDataObject(
  objectToCheck,
  checkDataTypes = FALSE,
  verbose = FALSE,
  strict = TRUE
)

checkRDBESDataObject(
  objectToCheck,
  checkDataTypes = FALSE,
  verbose = FALSE,
  strict = TRUE
)
}
\arguments{
\item{objectToCheck}{RDBESDataObject i.e. a list of data.tables}

\item{checkDataTypes}{(Optional) Set to TRUE if you want to check that
the data types of the required columns are correct, or FALSE if you don't
care. Default value is FALSE.}

\item{verbose}{(Optional) Set to TRUE if you want informative text printed
out, or FALSE if you don't.  The default is FALSE.}

\item{strict}{(Optional) Set to TRUE if you want to be sure all columns
are present in the data, set to FALSE if you only want to check that
essential columns are present.  The default is TRUE.}
}
\value{
Returns objectToCheck
}
\description{
Perform basic checks on a object.
}
\details{
Checks if 'objectToCheck' parameter is valid. Returns the parameter if it is
valid and otherwise stops on error.
It checks the \code{RDBESDataObject} if:
\itemize{
\item Is this an object of class RDBESDataObject
\item Tables don't have column names that aren't allowed
\item Tables have all the required  column names
}

It does not check if the data is valid.
The RDBES upload system performs an extensive set of checks on the
uploaded data.
}
\examples{
\dontrun{
myH1RawObject <-
importRDBESDataCSV(rdbesExtractPath = "tests/testthat/h1_v_1_19")
validateRDBESDataObject(myH1RawObject)}

}
