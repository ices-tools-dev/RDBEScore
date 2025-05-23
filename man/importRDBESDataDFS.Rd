% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importRDBESDataDFS.R
\name{importRDBESDataDFS}
\alias{importRDBESDataDFS}
\title{Convert List of Data Frames to a RDBES Data Object}
\usage{
importRDBESDataDFS(
  myList,
  castToCorrectDataTypes = TRUE,
  verbose = FALSE,
  strict = TRUE,
  addMissingColumns = FALSE,
  ...
)
}
\arguments{
\item{myList}{A \code{list} of data tables. Each element of the list should be a
data frame with an RDBES two-letter name (e.g. "DE").}

\item{castToCorrectDataTypes}{logical. Indicates whether to cast the columns
to the correct data types. Default is \code{TRUE}.}

\item{verbose}{(Optional) Set to TRUE if you want informative text printed
out, or FALSE if you don't.  The default is FALSE.}

\item{strict}{logical. Indicates level of validation of the \code{RDBESDataObject}
it creates - should the validation be strict? Default is \code{TRUE}.}

\item{addMissingColumns}{logical. Indicates whether to add missing columns}
}
\value{
An \code{RDBESDataObject} with each element being a data table.
}
\description{
This function converts a list of data frames into an object of class
\code{RDBESDataObject}.
}
\details{
Tables in the input list should have the correct 2-letter RDBES name
(e.g. "DE", "LE", etc.). The function converts all data frames to
\code{data.table}. \code{NULL} tables are left as \code{NULL}.

If \code{castToCorrectDataTypes = TRUE}, it ensures all columns are of the
correct data type using \code{setRDBESDataObjectDataTypes}.

Column names are replaced with the RDBES 'R names' from the model
documentation.

The function then sets a key on each table using the 'XXid' column as the
key, where 'XX' is the name of that table. It also replaces all empty
strings with \code{NA}.

It then uses the \code{newRDBESDataObject} function to create a new
\code{RDBESDataObject} from the input.

Finally, it validates the RDBESDataObject using
\code{RDBEScore::validateRDBESDataObject} and returns it.
}
\keyword{internal}
