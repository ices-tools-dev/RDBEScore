% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/doEstimationRatio.R
\name{doEstimationRatio}
\alias{doEstimationRatio}
\title{Estimate Numbers and Mean Values by Length or Age Class}
\usage{
doEstimationRatio(
  RDBESDataObj,
  targetValue = "LengthComp",
  classUnits = "mm",
  classBreaks = c(100, 300, 10),
  LWparam = NULL,
  lowerAux = NULL,
  verbose = FALSE
)
}
\arguments{
\item{RDBESDataObj}{A validated RDBESDataObject containing hierarchical sampling and biological data. Must include appropriate tables (e.g., CL, CE, SA, FM, or BV) depending on estimation requirements.}

\item{targetValue}{A character string specifying the type of composition to estimate. Options are "LengthComp" or "AgeComp".}

\item{classUnits}{Units of the class intervals for length or age, typically "mm" for millimeters or "cm" for centimeters. Used in defining class intervals.}

\item{classBreaks}{A numeric vector of three values: minimum value, maximum value, and class width (e.g., c(100, 300, 10)). Defines the class intervals for grouping lengths or ages.}

\item{LWparam}{A numeric vector of length two specifying parameters (a, b) for the weight-length relationship (W = a * L^b). Used if no direct weights are available but lengths are provided.}

\item{lowerAux}{A numeric or character vector referencing a variable in the SA table used as an auxiliary variable for ratio estimation (e.g., sample weights, sub-sample expansion factors).}

\item{verbose}{Logical; if TRUE, detailed messages are printed during processing.}
}
\value{
A list or data.table containing the estimated numbers at length or age and associated mean values such as weight and length, depending on input and target type.
}
\description{
The function is under development and does not work yet.
}
