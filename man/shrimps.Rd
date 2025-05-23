% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{shrimps}
\alias{shrimps}
\title{One quarter of sample data from swedish shrimp landings of the
SWE_OTB_CRU_32-69_0_0 fishery}
\format{
A data frame with 10 rows and 95 variables:
\itemize{
\item DEsamplingScheme - Sampling Scheme
\item DEyear - Year of data collection
\item DEstratumName - Fishery code
\item DEhierarchyCorrect - Design Variable of RDBES. More details in RDBES documentation
\item DEhierarchy - Design Variable of RDBES. More details in RDBES documentation
\item DEsampled - Design Variable of RDBES. More details in RDBES documentation
\item DEreasonNotSampled - Design Variable of RDBES. More details in RDBES documentation
\item SDcountry - Country that collected the data
\item SDinstitution - Institution that collected the data
\item su1, su2, su3, su4, su5 - sampling units of RDBES. More details in RDBES documentation
\item XXXnumberSampled, ... - Design Variables of RDBES. More details in RDBES documentation
\item targetValue - estimate of weight landed in each haul (in kg)
\item plus XX other columns
}
}
\source{
Nuno Prista @ SLU Aqua, 2022
}
\usage{
shrimps
}
\description{
A dataset of rdbesEstimObj type containing simplified haul-level
samples (rows) of shrimp landings (targetValue, in kg) observed onboard using
H1 of RDBES with UPWOR on vessels. Data is provided for developing/testing
purposes only.
}
\keyword{datasets}
