#' The tables required for each RDBES hierarchy.
#'
#' A data frame containing the tables required for each RDBES hierachy
#'
#' @format A data frame containing the tables required for each RDBES hierachy.
#' \describe{
#'   \item{hierarchy}{the hierachy this applies to H1 to H13}
#'   \item{table}{the 2-letter table name}
#'   \item{lowerHierarchy}{is this a lower hierarchy table?}
#'   \item{optional}{is this table optional within the hierarchy?}
#'   \item{samplingUnit}{is this table a sampling unit within the hierarchy?}
#'   \item{sortOrder}{the table sort order within the hiaerarchy}
#' }
#' @source \url{https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles}
"tablesInRDBESHierarchies"

#' A dataset containing test RDBES data for H1 in the RDBESDataObject structure
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data frame}
#'   \item{SD}{the Sampling Details data frame}
#'   \item{VS}{the Vessel Selection data frame}
#'   \item{FT}{the Fishing Trip data frame}
#'   \item{FO}{the Fishing Operation data frame}
#'   \item{SS}{the Species Selection data frame}
#'   \item{SA}{the Sample data frame}
#'   \item{FM}{the Frequency Measure data frame}
#'   \item{BV}{the Biological Variable data frame}
#'   \item{VD}{the Vessel Details data frame}
#'   \item{SL}{the Species List data frame}
#'   ...
#' }
"MadeUpData_for_SL_SA_tests_v1_WGRDBES_EST_TEST_3"


#' A dataset containing the mapping from database column names
#' to R field names
#'
#' @format A data frame containing database field names and their equivalent
#' R field name:
#' \describe{
#'   \item{Table.Prefix}{The two letter prefix of the relevent RDBES table}
#'   \item{Field.Name}{The database field names}
#'   \item{R.Name}{The equivalent R field name}
#'   \item{RDataType}{The equivalent R data type (e.g. "integer", "character" etc)}
#'   \item{Type}{The Data type in the RDBES documentation (e.g. "Decimal", etc)}
#'   \item{EssentialForEst}{Is this column considered essential?}
#'   ...
#' }
#' @source \url{https://sboxrdbes.ices.dk}
"mapColNamesFieldR"

#' One quarter of sample data from swedish shrimp landings of the
#' SWE_OTB_CRU_32-69_0_0 fishery
#'
#' A dataset of rdbesEstimObj type containing simplified haul-level
#' samples (rows) of shrimp landings (targetValue, in kg) observed onboard using
#' H1 of RDBES with UPWOR on vessels. Data is provided for developing/testing
#' purposes only.
#'
#' @format A data frame with 10 rows and 95 variables:
#'
#'   - DEsamplingScheme - Sampling Scheme
#'   - DEyear - Year of data collection
#'   - DEstratumName - Fishery code
#'   - DEhierarchyCorrect - Design Variable of RDBES. More details in RDBES documentation
#'   - DEhierarchy - Design Variable of RDBES. More details in RDBES documentation
#'   - DEsampled - Design Variable of RDBES. More details in RDBES documentation
#'   - DEreasonNotSampled - Design Variable of RDBES. More details in RDBES documentation
#'   - SDcountry - Country that collected the data
#'   - SDinstitution - Institution that collected the data
#'   - su1, su2, su3, su4, su5 - sampling units of RDBES. More details in RDBES documentation
#'   - XXXnumberSampled, ... - Design Variables of RDBES. More details in RDBES documentation
#'   - targetValue - estimate of weight landed in each haul (in kg)
#'   - plus XX other columns
#'
#' @source Nuno Prista @ SLU Aqua, 2022
"shrimps"

#' One quarter of sample data from swedish shrimp catches of the
#' SWE_OTB_CRU_32-69_0_0 fishery
#'
#' A dataset of rdbesEstimObj type containing simplified haul-level
#' samples (rows) of shrimp catches (targetValue, in kg) observed onboard using
#' H1 of RDBES with UPWOR on vessels. Catches are divided into three strata (91, 92, 93_94)
#' that correspond to sorting sieves used onboard. Data is provided for developing/testing
#' purposes only.
#'
#' @format A data frame with 10 rows and 95 variables:
#'
#'   - DEsamplingScheme - Sampling Scheme
#'   - DEyear - Year of data collection
#'   - DEstratumName - Fishery code
#'   - DEhierarchyCorrect - Design Variable of RDBES. More details in RDBES documentation
#'   - DEhierarchy - Design Variable of RDBES. More details in RDBES documentation
#'   - DEsampled - Design Variable of RDBES. More details in RDBES documentation
#'   - DEreasonNotSampled - Design Variable of RDBES. More details in RDBES documentation
#'   - SDcountry - Country that collected the data
#'   - SDinstitution - Institution that collected the data
#'   - su1, su2, su3, su4, su5 - sampling units of RDBES. More details in RDBES documentation
#'   - XXXnumberSampled, ... - Design Variables of RDBES. More details in RDBES documentation
#'   - su5stratumName - sieve fraction
#'   - targetValue - estimate of weight fraction in each haul (in kg)
#'   - plus XX other columns
#'
#' @source Nuno Prista @ SLU Aqua, 2022
"shrimpsStrat"

#' A dataset containing the RDBES "design variable" names
#'
#' @format A vector containing the short R names of the RDBES design variables
#' (without any 2 letter table prefixes)
#' R field name:
#' \describe{
#'   \item{designVariables}{The design variable names}
#' }
#' @source \url{https://sboxrdbes.ices.dk}
"designVariables"

#' A dataset containing a copy of the icesSpecWoRMS code list. The latest
#' code list data can be downloaded from https://vocab.ices.dk/
#'
#' @format A data frame
#' \describe{
#'   \item{GUID}{Globally unique identifier assigned by ICES}
#'   \item{Key}{AphiaID}
#'   \item{Description}{Scientific name}
#'   \item{LongDescription}{Ignore}
#'   \item{Modified}{Date when the code was last updated}
#'   \item{Deprecated}{IS this still a valid code.  If FALSE the code is
#'   no longer valid within ICES.}
#'   \item{DateDownloaded}{E.g. "2023-10-18" }
#'   ...
#' }
#' @source \url{https://vocab.ices.dk/}
"icesSpecWoRMS"

#' A dataset containing aphia records for species found in icesSpecWoRMS
#'
#' @format A data frame
#' \describe{
#' \item{AphiaID}{E.g. 100684 }
#' \item{url}{E.g. "https://www.marinespecies.org/aphia.php?p=taxdetails&id=100684"}
#' \item{scientificname}{E.g. "Cerianthidae"  }
#' \item{authority}{E.g. "Milne Edwards & Haime, 1851" }
#' \item{status}{E.g. "accepted"  }
#' \item{unacceptreason}{E.g. NA }
#' \item{taxonRankID}{E.g. 140  }
#' \item{rank}{E.g. "Family" "Genus" "Species" "Species" }
#' \item{valid_AphiaID}{E.g. 100684 }
#' \item{valid_name}{E.g. "Cerianthidae"  }
#' \item{valid_authority}{E.g. "Milne Edwards & Haime, 1851" }
#' \item{parentNameUsageID}{E.g. 151646 }
#' \item{kingdom}{E.g. "Animalia"  }
#' \item{phylum}{E.g. "Cnidaria"  }
#' \item{class}{E.g. "Anthozoa"  }
#' \item{order}{E.g. "Spirularia" }
#' \item{family}{E.g. "Cerianthidae"  }
#' \item{genus}{E.g. NA "Cerianthus"}
#' \item{citation }{E.g. "Molodtsova, T. (2023). World List of Ceriantharia.
#' Cerianthidae Milne Edwards & Haime, 1851. Accessed through: "... }
#' \item{isMarine}{E.g. 1 }
#' \item{isBrackish}{E.g. 1 }
#' \item{isFreshwater}{E.g. 0  }
#' \item{isTerrestrial}{E.g. 0  }
#' \item{isExtinct}{E.g. NA  }
#' \item{match_type}{E.g. "exact" }
#' \item{modified}{E.g. "2018-01-22T17:48:34.063Z" }
#' \item{DateDownloaded}{E.g. "2023-10-18" }
#'   ...
#' }
#' @source \url{https://www.marinespecies.org/}
"wormsAphiaRecord"
