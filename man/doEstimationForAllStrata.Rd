% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doEstimationForAllStrata.R
\name{doEstimationForAllStrata}
\alias{doEstimationForAllStrata}
\title{Estimate totals and means, and try to generate samples variances for all
strata in an RDBESEstObject}
\usage{
doEstimationForAllStrata(RDBESEstObjectForEstim, targetValue, verbose = FALSE)
}
\arguments{
\item{RDBESEstObjectForEstim}{The RDBESEstObject to generate estimates for}

\item{targetValue}{The field to estimate for, for example "SAsampWtLive"}

\item{verbose}{(Optional) If set to TRUE more detailed text will be printed
out by the function.  Default is FALSE}
}
\value{
A data frame containing estimates for all strata
}
\description{
Estimate totals and means, and try to generate samples variances for all
strata in an RDBESEstObject
}
\examples{
\dontrun{

myH1RawObject <-
  importRDBESDataCSV(rdbesExtractPath = "tests\\\\testthat\\\\h1_v_1_19")

# Update our test data with some random sample measurements
myH1RawObject[["SA"]]$SAsampWtLive <-
  round(runif(n = nrow(myH1RawObject[["SA"]]), min = 1, max = 100))

myH1EstObj <- createRDBESEstObject(myH1RawObject, 1)

myStrataEst <- doEstimationForAllStrata(
  RDBESDataObjectForEstim = myH1EstObj,
  targetValue = 'SAsampWtLive'
)
}
}
