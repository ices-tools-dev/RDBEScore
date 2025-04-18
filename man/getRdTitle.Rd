% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listPackageFunctions.R
\name{getRdTitle}
\alias{getRdTitle}
\title{Extract Title from an Rd Object}
\usage{
getRdTitle(rd)
}
\arguments{
\item{rd}{An Rd object typically obtained via function like \code{utils:::.getHelpFile}.}
}
\value{
A character string containing the title extracted from the Rd object. If the title is not found, an empty string is returned.
}
\description{
This internal helper function traverses an Rd object to extract the text from the \code{\\title} field. It is used by \code{listPackageFunctions} to obtain a concise description for each documented function.
}
\keyword{internal}
