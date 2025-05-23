% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generateZerosUsingSL.R
\name{generateZerosUsingSL}
\alias{generateZerosUsingSL}
\title{Generate zeros in samples using Species List information}
\usage{
generateZerosUsingSL(x, verbose = FALSE, strict = TRUE)
}
\arguments{
\item{x}{RDBES data frame}

\item{verbose}{(Optional) Set to TRUE if you want informative text printed
out, or FALSE if you don't.  The default is FALSE.}

\item{strict}{(Optional) This function validates its input data - should
the validation be strict? The default is TRUE.}
}
\value{
RDBES data frame where SA was complemented with species looked for
(sensu in sampling objectives) but not registered in sample
}
\description{
examples for now see
https://github.com/ices-eg/WK_RDBES/tree/master/WKRDB-EST2/chairs/Nuno
}
