#-----------Package utility data ---------

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
#'   \item{AphiaID}{E.g. 100684 }
#'   \item{url}{E.g. "https://www.marinespecies.org/aphia.php?p=taxdetails&id=100684"}
#'   \item{scientificname}{E.g. "Cerianthidae"  }
#'   \item{authority}{E.g. "Milne Edwards & Haime, 1851" }
#'   \item{status}{E.g. "accepted"  }
#'   \item{unacceptreason}{E.g. NA }
#'   \item{taxonRankID}{E.g. 140  }
#'   \item{rank}{E.g. "Family" "Genus" "Species" "Species" }
#'   \item{valid_AphiaID}{E.g. 100684 }
#'   \item{valid_name}{E.g. "Cerianthidae"  }
#'   \item{valid_authority}{E.g. "Milne Edwards & Haime, 1851" }
#'   \item{parentNameUsageID}{E.g. 151646 }
#'   \item{kingdom}{E.g. "Animalia"  }
#'   \item{phylum}{E.g. "Cnidaria"  }
#'   \item{class}{E.g. "Anthozoa"  }
#'   \item{order}{E.g. "Spirularia" }
#'   \item{family}{E.g. "Cerianthidae"  }
#'   \item{genus}{E.g. NA "Cerianthus"}
#'   \item{citation }{E.g. "Molodtsova, T. (2023). World List of Ceriantharia.
#' Cerianthidae Milne Edwards & Haime, 1851. Accessed through: "... }
#'   \item{lsid}{internal database identifier}
#'   \item{isMarine}{E.g. 1 }
#'   \item{isBrackish}{E.g. 1 }
#'   \item{isFreshwater}{E.g. 0  }
#'   \item{isTerrestrial}{E.g. 0  }
#'   \item{isExtinct}{E.g. NA  }
#'   \item{match_type}{E.g. "exact" }
#'   \item{modified}{E.g. "2018-01-22T17:48:34.063Z" }
#'   \item{DateDownloaded}{E.g. "2023-10-18" }
#'   ...
#' }
#' @source \url{https://www.marinespecies.org/}
"wormsAphiaRecord"




#-------Example Hierarchy datasets---------

#' A dataset containing test RDBES data for H1 in the RDBESDataObject structure
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table}
#'   \item{SD}{the Sampling Details data table}
#'   \item{VS}{the Vessel Selection data table}
#'   \item{FT}{the Fishing Trip data table}
#'   \item{FO}{the Fishing Operation data table}
#'   \item{SS}{the Species Selection data table}
#'   \item{SA}{the Sample data table}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table}
#' }
"H1Example"

#' A dataset containing test RDBES data for H8 in the RDBESDataObject structure
#'
#' This dataset does not have passed the RDBES upload checks,
#'  hence the object might be somewhat invalid, however it resembles real data
#'  from the Estonian Baltic Trawling fleet for 2022 sprat total landings and
#'  commercial sampling
#'
#' @format A list containing entries required for H8 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table}
#'   \item{SD}{the Sampling Details data table}
#'   \item{TE}{the Temporal Event data table}
#'   \item{VS}{the Vessel Selection data table}
#'   \item{LE}{the Landing Event data table}
#'   \item{SS}{the Species Selection data table}
#'   \item{SA}{the Sample data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table}
#'   \item{CL}{the Commertial Landing data table}
#'   \item{CE}{the Commertial Effort data table}
#' }
#' #' @source Richard Meitern @ Estonian Marine Institute, 2023
"H8ExampleEE1"

#' A dataset containing test RDBES data for H5 in the RDBESDataObject structure
#'
#' @format A list containing entries required for H5 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table}
#'   \item{SD}{the Sampling Details data table}
#'   \item{FT}{the Fishing Trip data table}
#'   \item{OS}{the Onshore Event data table}
#'   \item{LE}{the Landing Event data table}
#'   \item{SS}{the Species Selection data table}
#'   \item{SA}{the Sample data table}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table}
#' }
"H5Example"

##-------H1 Data from survey package-------------

#' A Multi-Stage RDBESDataObject converted from package survey dataset apiclus2
#'
#' This data set is derived from the Academic Performance Index computed for all
#' California schools based on standardized testing of students. The original
#' data sets contain information for all schools with at least 100 students and
#' for various probability samples of the data. The design is 2-stage cluster
#' sampling with clusters of unequal sizes. An SRS of 40 districts is selected
#' (psus) from the 757 districts in the population and then up to 5 schools (min
#' 1) were selected from each district (ssus).
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table. Contains 1 DE row with DEstratumName == "Pckg_SDAResources_apiclus2_H1"}
#'   \item{SD}{the Sampling Details data table. Contains 1 child SD row}
#'   \item{VS}{the Vessel Selection data table. Contains 40 child rows (the 40 districts), VSnumberTotal is 757, VSnumberSampled is 40}
#'   \item{FT}{the Fishing Trip data table. Contains 126 child rows (the 126 schools finally observed), each associated to its cluster (dname), FTnumberTotal is the number of schools in district, FTnumberSAmpled is 1...5 schools sampled}
#'   \item{FO}{the Fishing Operation data table. Just 1:1 links to the final data (in SA)}
#'   \item{SS}{the Species Selection data table. Just 1:1 links to the final data (in SA)}
#'   \item{SA}{the Sample data table. SAsampleWeightMeasured is enroll (NB! there are 4 NAs)}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table}
#' }
#' @source \url{https://CRAN.R-project.org/package=survey}
"Pckg_survey_apiclus2_H1_WGRDBES_EST_TEST_1"

#' A RDBESDataObject converted from package survey dataset apiclus1
#'
#' This data set is derived from the Academic Performance Index computed for all
#' California schools based on standardized testing of students. The original
#' data sets contain information for all schools with at least 100 students and
#' for various probability samples of the data. The design is 1-stage cluster
#' sampling with clusters of unequal sizes. An SRS of 15 districts is selected
#' (psus) from the 757 districts in the population. All schools within district
#' are selected (ssus). The weights (pw) do not match 757/15 probably because
#' they have been calibrated. The target variable is enroll.
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table. Contains 1 DE row with DEstratumName == "Pckg_SDAResources_apiclus1_v2_H1"}
#'   \item{SD}{the Sampling Details data table. Contains 1 child SD row}
#'   \item{VS}{the Vessel Selection data table. Contains 183 child rows (the 186 schools finally observed), each associated to its cluster (dname), VSnumberTotalClusters is 757, VSnumberTotal is the number of schools in the cluster (census), calibrated weights are provided as 1/pw in VSinclusionProbCluster}
#'   \item{FT}{the Fishing Trip data table. Just 1:1 links to the final data (in SA)}
#'   \item{FO}{the Fishing Operation data table. Just 1:1 links to the final data (in SA)}
#'   \item{SS}{the Species Selection data table. Just 1:1 links to the final data (in SA)}
#'   \item{SA}{the Sample data table. SAsampleWeightMeasured is enroll}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table}
#' }
#' @source \url{https://CRAN.R-project.org/package=survey}
"Pckg_survey_apiclus1_v2_H1_WGRDBES_EST_TEST_1"

#' A Clustered RDBESDataObject converted from package survey dataset apiclus2
#'
#' This data set is derived from the Academic Performance Index computed for all
#' California schools based on standardized testing of students. The original
#' data sets contain information for all schools with at least 100 students and
#' for various probability samples of the data. The design is 2-stage cluster
#' sampling with clusters of unequal sizes. An SRS of 40 districts is selected
#' (psus) from the 757 districts in the population and then up to 5 schools (min
#' 1) were selected from each district (ssus). The target variable is enroll - note that it contains 4 NA values.
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table. Contains 1 DE row with DEstratumName == "Pckg_SDAResources_apiclus2_v2_H1"}
#'   \item{SD}{the Sampling Details data table. Contains 1 child SD row}
#'   \item{VS}{the Vessel Selection data table. Contains 126 child rows (the 126 schools finally observed), each associated to its cluster (dname), VSnumberTotalClusters is 757, VSnumberTotal is 1...5 schools sampled}
#'   \item{FT}{the Fishing Trip data table. Just 1:1 links to the final data (in SA)}
#'   \item{FO}{the Fishing Operation data table. Just 1:1 links to the final data (in SA)}
#'   \item{SS}{the Species Selection data table. Just 1:1 links to the final data (in SA)}
#'   \item{SA}{the Sample data table. SAsampleWeightMeasured is enroll (note the 4 NAs)}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table}
#' }
#' @source \url{https://CRAN.R-project.org/package=survey}
"Pckg_survey_apiclus2_v2_H1_WGRDBES_EST_TEST_1"

#' A RDBESDataObject converted from package survey dataset apistrat
#'
#' This data set is a stratified version of the previous "apiclus2" data. It is derived from the Academic Performance Index computed for all California schools based on standardized testing of students. The original data sets contain information for all schools with at least 100 students and for various probability samples of the data. The design is 1-stage cluster sampling with clusters of unequal sizes. An SRS of 200 districts is selected (psus) from the 755 districts in the population. All schools within district are selected (ssus).
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table. Contains 1 DE row}
#'   \item{SD}{the Sampling Details data table. Contains 1 child SD row}
#'   \item{VS}{the Vessel Selection data table. Contains 200 child rows
#'   (the 200 schools finally observed), each associated to its cluster
#'   (dname), VSnumberTotalClusters is 755, VSnumberTotal is 50-100 schools
#'   sampled}
#'   \item{FT}{the Fishing Trip data table. Contains 200 child rows
#'   (the 200 schools finally observed), each associated to its cluster
#'   (dname), FTnumberTotal is the number of schools in the cluster (census)}
#'   \item{FO}{the Fishing Operation data table. Just 1:1 links to the
#'   final data (in SA)}
#'   \item{SS}{the Species Selection data table. Just 1:1 links to the
#'   final data (in SA)}
#'   \item{SA}{the Sample data table. SAsampleWeightMeasured is enroll}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table. Contains 311 child rows}
#'   \item{SL}{the Species List data table. Contains 1 child row}
#' }
#' @source \url{https://CRAN.R-project.org/package=survey}
"Pckg_survey_apistrat_H1_WGRDBES_EST_TEST_1"




##------H1 Data from SDAResources package-------------------

#' A RDBESDataObject converted from package SDAResources dataset agsrs
#'
#' This data set is derived from the data(agsrs) used in Lohr examples 2.6, 2.7
#' and 2.11 of SDA book. Information required for example 4.8
#' (domain estimation) is also added to SA (farmcat <=> SAarea).
#' VSnumberSampled and VSnumberTotal set according to agsrs and book pop
#' values. VSunitName is set to a combination of original agsrs$county,
#' agsrs$state, agsrs$region and row numbers. Table SA contains the variable
#' measured agsrs$acres92 in SAtotalWeightMeasured, SAsampleWeightMeasured and
#' SAconversionFactorMeasLive set to 1. Table SA also contains the domain
#' information, coded in SAarea. Table DE, SD, FT and FO are for the most
#' dummy tables inserted to meet RDBES model requirements to be aggregated
#' during estimation tests. Values of mandatory fields have dummy values with
#' exception of Design-Variables in VS that match the book. BV, FM, CL, and CE
#' are not provided. SL and VD are subset to the essential rows.
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table. Contains dummy values with exception of
#'   Design-Variables in VS that match the book}
#'   \item{SD}{the Sampling Details data table. Contains dummy values}
#'   \item{VS}{the Vessel Selection data table. Contains core information of
#'   data(agsrs), VSnumberSampled and VSnumberTotal set according to agsrs
#'   and book pop values, VSunitName is set to a combination of original
#'   agsrs$county, agsrs$state, agsrs$region and row numbers}
#'   \item{FT}{the Fishing Trip data table. Contains dummy values}
#'   \item{FO}{the Fishing Operation data table. Contains dummy values}
#'   \item{SS}{the Species Selection data table. Contains dummy values}
#'   \item{SA}{the Sample data table. Contains the variable measured
#'   agsrs$acres92 in SAtotalWeightMeasured, SAsampleWeightMeasured and
#'   SAconversionFactorMeasLive set to 1, and the domain information,
#'   coded in SAarea}
#'   \item{FM}{the Frequency Measure data table. Not provided}
#'   \item{BV}{the Biological Variable data table. Not provided}
#'   \item{VD}{the Vessel Details data table. Subset to the essential rows}
#'   \item{SL}{the Species List data table. Subset to the essential rows}
#' }
#' @source \url{https://CRAN.R-project.org/package=SDAResources}
"Pckg_SDAResources_agsrs_H1_WGRDBES_EST_TEST_1"

#' A RDBESDataObject converted from package SDAResources dataset agstrat
#'
#' This data set is derived from the data(agstrat) used in Lohr examples 3.2
#' and 3.6. Table VS is stratified with VSstratumName set to agstrat$region,
#' and VSnumberSampled and VSnumberTotal set according to agstrat.
#' VSunitName is set to a combination of original agstrat$county,
#' agstrat$state, agstrat$region and agstrat$agstrat row numbers.
#' Table SA contains the variable measured agstrat$acres92 in
#' SAtotalWeightMeasured, SAsampleWeightMeasured and SAconversionFactorMeasLive
#' set to 1. Table DE, SD, FT and FO are for the most dummy tables inserted to
#' meet RDBES model requirements to be aggregated during estimation tests.
#' Values of mandatory fields have dummy values taken from an onboard programme,
#' with exception of selectionMethod that is set to CENSUS. BV, FM, CL, and CE
#' are not provided. SL and VD are subset to the essential rows.
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table. Contains dummy values with exception of
#'   selectionMethod that is set to CENSUS}
#'   \item{SD}{the Sampling Details data table. Contains dummy values}
#'   \item{VS}{the Vessel Selection data table. Contains core information of
#'   data(agstrat), VSstratumName set to agstrat$region, and VSnumberSampled
#'   and VSnumberTotal set according to agstrat, VSunitName is set to a
#'   combination of original agstrat$county, agstrat$state, agstrat$region and
#'   agstrat$agstrat row numbers}
#'   \item{FT}{the Fishing Trip data table. Contains dummy values}
#'   \item{FO}{the Fishing Operation data table. Contains dummy values}
#'   \item{SS}{the Species Selection data table. Contains dummy values}
#'   \item{SA}{the Sample data table. Contains the variable measured
#'   agstrat$acres92 in SAtotalWeightMeasured, SAsampleWeightMeasured and
#'   SAconversionFactorMeasLive set to 1}
#'   \item{FM}{the Frequency Measure data table. Not provided}
#'   \item{BV}{the Biological Variable data table. Not provided}
#'   \item{VD}{the Vessel Details data table. Subset to the essential rows}
#'   \item{SL}{the Species List data table. Subset to the essential rows}
#' }
#' @source \url{https://CRAN.R-project.org/package=SDAResources}
"Pckg_SDAResources_agstrat_H1_WGRDBES_EST_TEST_1"

#' A RDBESDataObject converted from package SDAResources dataset algebra
#'
#' This data set is derived from a fictional data for an SRS of 12 algebra
#' classes in a city, from a population of 187 classes. The design is 1-stage
#' cluster sampling with clusters of unequal sizes. Clusters are classes (class).
#' Clusters (psu) are unequal sized (Mi). In each cluster, all students are
#' selected (ssus, nrows). The total number of psus is known (187). The target
#' variable is score.
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table. Contains 1 DE row with
#'   DEstratumName == "Pckg_SDAResources_algebra_H1"}
#'   \item{SD}{the Sampling Details data table. Contains 1 child SD row}
#'   \item{VS}{the Vessel Selection data table. Contains 299 child rows
#'   (the 299 students observed), each associated to its cluster (class),
#'   VSnumberTotalClusters is 187, VSnumberSampledClusters is 12,
#'   VSnumberTotal is Missing}
#'   \item{FT}{the Fishing Trip data table. Just 1:1 links to the final
#'   data (in SA)}
#'   \item{FO}{the Fishing Operation data table. Just 1:1 links to the final
#'    data (in SA)}
#'   \item{SS}{the Species Selection data table. Just 1:1 links to the final
#'    data (in SA)}
#'   \item{SA}{the Sample data table. Each score is a SAsampleWeightMeasured}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table}
#' }
#' @source \url{https://CRAN.R-project.org/package=SDAResources}
"Pckg_SDAResources_algebra_H1_WGRDBES_EST_TEST_1"

#' A RDBESDataObject converted from package SDAResources dataset coots
#'
#' This data set is derived from the data(coots). The design is 2-stage cluster
#' sampling with clusters of unequal sizes and Npsu not known. Clusters are
#' clutches of eggs (nests) with at least 2 eggs. In each cluster, the volume
#' of two eggs is measured. Clusters (psu) are unequal sized. In each cluster,
#' 2 eggs are selected (ssus) and measured. The total number of psus is not
#' known (a drawback in this example). It is assumed very large (fpc negligible).
#' The target variable is volume (others are available).
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table. Contains 1 DE row with
#'   DEstratumName == "Pckg_SDAResources_coots_H1"}
#'   \item{SD}{the Sampling Details data table. Contains 1 child SD row}
#'   \item{VS}{the Vessel Selection data table. Contains 368 child rows
#'   (the 368 eggs/psus observed), each associated to its cluster (clutch),
#'   VSnumberTotalClusters is not known, VSnumberTotal is csize}
#'   \item{FT}{the Fishing Trip data table. Just 1:1 links to the final data
#'   (in SA)}
#'   \item{FO}{the Fishing Operation data table. Just 1:1 links to the final
#'   data (in SA)}
#'   \item{SS}{the Species Selection data table. Just 1:1 links to the
#'   final data (in SA)}
#'   \item{SA}{the Sample data table. Each volume is a SAsampleWeightMeasured.
#'   ATT volumes are *100000000 to meet type requirement (integer)}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table}
#' }
#' @source \url{https://CRAN.R-project.org/package=SDAResources}
"Pckg_SDAResources_coots_H1_WGRDBES_EST_TEST_1"

#' A Multi-stage RDBESDataObject from package SDAResources dataset coots
#'
#' This data set is derived from the data(coots). The design is 2-stage cluster
#' sampling with clusters of unequal sizes and Npsu not known. Clusters are
#' clutches of eggs (nests) with at least 2 eggs. In each cluster, the volume
#' of two eggs is measured. Clusters (psu) are unequal sized. In each cluster,
#' 2 eggs are selected (ssus) and measured. The total number of psus is not
#' known (a drawback in this example). It is assumed very large (fpc negligible).
#' The target variable is volume.
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table. Contains 1 DE row with
#'   DEstratumName == "Pckg_SDAResources_coots_H1"}
#'   \item{SD}{the Sampling Details data table. Contains 1 child SD row}
#'   \item{VS}{the Vessel Selection data table. Contains 184 child rows
#'   (the 184 clutches/psus observed), each associated to its cluster (clutch),
#'   VSnumberTotal is not known, VSnumberSampled is 184}
#'   \item{FT}{the Fishing Trip data table. Contains 368 child rows
#'   (the 368 eggs/ssus measured), each associated to its vessel (clutch),
#'   FTnumbersampled is 2, FTnumberTotal is csize}
#'   \item{FO}{the Fishing Operation data table. Just 1:1 links to the final
#'   data (in SA)}
#'   \item{SS}{the Species Selection data table. Just 1:1 links to the final
#'   data (in SA)}
#'   \item{SA}{the Sample data table. Each volume is a SAsampleWeightMeasured.
#'   ATT volumes are *100000000 to meet type requirement (integer)}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table}
#' }
#' @source \url{https://CRAN.R-project.org/package=SDAResources}
"Pckg_SDAResources_coots_multistage_H1_WGRDBES_EST_TEST_1"

#' A RDBESDataObject converted from package SDAResources dataset gpa
#'
#' This data set is derived from the data(gpa). The design is 1-stage cluster
#' sampling with clusters of equal sizes. Each cluster (suite) has 4 elements
#' with the same weight. The target variable is gpa.
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table. Contains 1 DE row with
#'   DEstratumName == "Pckg_SDAResources_gpa_H1"}
#'   \item{SD}{the Sampling Details data table. Contains 1 child SD row}
#'   \item{VS}{the Vessel Selection data table. Contains 20 child rows
#'   (the 20 observations), each associated to its cluster (suite),
#'   VSnumberTotalClusters is 100, VSnumberTotal is 4 because all elements in
#'   cluster are sampled}
#'   \item{FT}{the Fishing Trip data table. Just 1:1 links to the final data
#'   (in SA)}
#'   \item{FO}{the Fishing Operation data table. Just 1:1 links to the final
#'   data (in SA)}
#'   \item{SS}{the Species Selection data table. Just 1:1 links to the final
#'   data (in SA)}
#'   \item{SA}{the Sample data table. Each gpa score is a
#'   SAsampleWeightMeasured. ATT gpa scores are *100 to meet type requirement
#'   (integer)}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table}
#' }
#' @source \url{https://CRAN.R-project.org/package=SDAResources}
"Pckg_SDAResources_gpa_H1_WGRDBES_EST_TEST_1"

#' A RDBESDataObject converted from package SDAResources dataset schools
#'
#' This data set is derived from the data(schools). The design is 2-stage cluster
#' sampling with clusters of unequal sizes and Npsu not known. Clusters are
#' schools (schoolid). Clusters (psu) are unequal sized (Mi). In each cluster,
#' 20 students are selected (ssus) and measured (nrows). The total number of
#' psus is known (75). The target variable is mathlevel.
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table. Contains 1 DE row with
#'   DEstratumName == "Pckg_SDAResources_schools_H1"}
#'   \item{SD}{the Sampling Details data table. Contains 1 child SD row}
#'   \item{VS}{the Vessel Selection data table. Contains 200 child rows
#'   (the 200 students observed), each associated to its cluster (schoolid),
#'   VSnumberTotalClusters is 100, VSnumberTotal is Mi}
#'   \item{FT}{the Fishing Trip data table. Just 1:1 links to the final data
#'   (in SA)}
#'   \item{FO}{the Fishing Operation data table. Just 1:1 links to the
#'   final data (in SA)}
#'   \item{SS}{the Species Selection data table. Just 1:1 links to the
#'   final data (in SA)}
#'   \item{SA}{the Sample data table. Each volume is a SAsampleWeightMeasured}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table}
#' }
#' @source \url{https://CRAN.R-project.org/package=SDAResources}
"Pckg_SDAResources_schools_H1_WGRDBES_EST_TEST_1"

##---------------Made Up data---------------

#' A made-up dataset for testing manipulations of SA based on SL
#'
#' This data set is created for testing the idea of manipulating Sample data (SA)
#' based on Species List (SL). It represents the simplest case for testing this
#' idea. The data set contains two species in SL for the same SLcountry,
#' SLinstitute, SLspeciesListName, SLyear, SLcatchFraction, SLcommercialTaxon,
#' SLspeciesCode & SLcommercialTaxon == SLspeciesCode. There is one species in
#' SA - one row in SS with keys equal to the SL keys.
#'
#' @format A list containing entries required for H1 RDBES data:
#' \describe{
#'   \item{DE}{the Design data table}
#'   \item{SD}{the Sampling Details data table}
#'   \item{VS}{the Vessel Selection data table}
#'   \item{FT}{the Fishing Trip data table}
#'   \item{FO}{the Fishing Operation data table}
#'   \item{SS}{the Species Selection data table.
#'   Contains one row with keys equal to the SL keys}
#'   \item{SA}{the Sample data table. Contains one species}
#'   \item{FM}{the Frequency Measure data table}
#'   \item{BV}{the Biological Variable data table}
#'   \item{VD}{the Vessel Details data table}
#'   \item{SL}{the Species List data table.
#'   Contains two species for the same SLcountry, SLinstitute,
#'   SLspeciesListName, SLyear, SLcatchFraction, SLcommercialTaxon,
#'   SLspeciesCode & SLcommercialTaxon == SLspeciesCode}
#' }
"MadeUpData_for_SL_SA_tests_v1_WGRDBES_EST_TEST_3"


#-------Other example data----------

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

