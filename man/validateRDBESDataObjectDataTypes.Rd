% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validateRDBESDataObjectDataTypes.R
\name{validateRDBESDataObjectDataTypes}
\alias{validateRDBESDataObjectDataTypes}
\title{Checks the data types of the columns in an
RDBESDataObject against an expected list of data types.  Any differences
are returned}
\usage{
validateRDBESDataObjectDataTypes(objectToCheck)
}
\arguments{
\item{objectToCheck}{An RDBESDataObject to check}
}
\value{
A data frame containing any data type differences (an empty data
frame if there are no differences)
}
\description{
Checks the data types of the columns in an
RDBESDataObject against an expected list of data types.  Any differences
are returned
}
