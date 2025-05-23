% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filterRDBESEstObject.R
\name{filterRDBESEstObject}
\alias{filterRDBESEstObject}
\title{Filter an RDBESEstObject}
\usage{
filterRDBESEstObject(
  RDBESEstObjectToFilter,
  fieldsToFilter,
  valuesToFilter,
  verbose = FALSE
)
}
\arguments{
\item{RDBESEstObjectToFilter}{The \code{RDBESEstObject} to filter}

\item{fieldsToFilter}{A vector of the field names you wish to check}

\item{valuesToFilter}{A vector of the field values you wish to filter for}

\item{verbose}{(Optional) Set to TRUE if you want informative text printed
out, or FALSE if you don't.  The default is FALSE.}
}
\value{
the filtered input object of the same class as
\code{RDBESEstObjectToFilter}
}
\description{
The returned object will include all rows which include the field names
and have one of the allowed values in \code{valuesToFilter}.
}
\examples{
\dontrun{

 myRawObject <- createRDBESDataObject(input = "tests\\\\testthat\\\\h1_v_1_19_26")
 myEstObject <- createRDBESEstObject(myRawObject,1)
 myFilteredEst <- filterRDBESEstObject(myEst,c("BVid"),c(7349207))

}
}
