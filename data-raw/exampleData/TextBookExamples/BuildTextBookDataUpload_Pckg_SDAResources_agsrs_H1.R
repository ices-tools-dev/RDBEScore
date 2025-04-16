#======Prepares textbook SDAResources::agsrs as H1 upload file===========

# Info:
# Table VS and SA contain the core information of \code{data(agsrs)} used in Lohr examples 2.6, 2.7 and 2.11 of SDA book
# information required for example 4.8 (domain estimation) is also added to SA [farmcat <=> SAarea]
# VSnumberSampled and VSnumberTotal set according to \code{agsrs} and book pop values
# VSunitName is set to a combination of original \code{agsrs$county}, \code{agsrs$state}, \code{agsrs$region} and row numbers
# Table SA contains the variable measured agsrs$acres92 in \code{SAtotalWeightMeasured}, \code{SAsampleWeightMeasured} and \code{SAconversionFactorMeasLive} set to 1.
# Table SA also contains the domain information, coded in SAarea
# Table DE, SD, FT and FO are for the most dummy tables inserted to meet RDBES model requirements to be aggregated
# during estimation tests.
# Values of mandatory fields have dummy values with exception of Design-Variables in VS that match the book
# BV, FM, CL, and CE are not provided.
# SL and VD are subset to the essential rows

	rm(list=ls())
	library(data.table)

	# load textbook data
		library(SDAResources)
		data(agsrs)
		dataset<-agsrs
			# adds domain info - pg 46 example 4.8
				dataset$farmcat<-"large"
				dataset$farmcat[dataset$farms92 < 600] <- "small"
		target_var <- "acres92"

	# name your project (will be used in filenames for CS, SL and VD)
		project_name_outputs <- "WGRDBES-EST_TEST_1_Pckg_SDAResources_agsrs_H1"

	# select a year for upload
		DEyear<-1965
		SDinstitution <- 4484
		DEsamplingScheme<-"WGRDBES-EST TEST 1"
		DEstratumName <- "Pckg_SDAResources_agsrs_H1"
    project_name_outputs <- gsub(" ","_", paste0(DEsamplingScheme,"_", DEstratumName))
		baseDir <- "./data-raw/exampleData/TextBookExamples/"
		baseDir <- ""
		VD_base <- readRDS(paste0(baseDir,"aux_TextBookExamples/VD_base.rds"))
		SL_base <- readRDS(paste0(baseDir,"aux_TextBookExamples/SL_base.rds"))

		#nameof the directory where the outputs are saved currently

		base_dir_outputs <- paste0(baseDir,"BuiltUploads")
		dir.create(base_dir_outputs, recursive=T, showWarnings=FALSE)


#========Outline of Hierarchy 1================
	# Design
	# Sampling details
	# Vessel Selection
	# Fishing Trip
	# Fishing Operation
	# Species Selection
	# Sample
	# Length
		# Biological variables



#===DE============



# 1                                     DEid [] - int
# 2                         DErecordType [M] - string
# 3             DEsamplingScheme [M] - SamplingScheme
# 4     DEsamplingSchemeType [M] - SamplingSchemeType
# 5                                 DEyear [M] - Year
# 6               DEstratumName [M] - StringLength100
# 7              DEhierarchyCorrect [M] - YesNoFields
# 8             DEhierarchy [M] - RDBESUpperHierarchy
# 9                    DEsampled [DV,M] - YesNoFields
# 10 DEreasonNotSampled [DV,O] - ReasonForNotSampling
# 11	  DEnonResponseCollected [DV,O] - YesNoFields
# 12    DEauxiliaryVariableTotal [DV,O] - DecimalPrec3
# 13    DEauxiliaryVariableValue [DV,O] - DecimalPrec3
# 14 DEauxiliaryVariableName [DV,O] - AuxiliaryVariableName
# 15 DEauxiliaryVariableUnit[DV,O]-MUNIT

DE_df_base<-expand.grid(DEyear=DEyear,
						DEstratumName="U",stringsAsFactors=F)

DE_df<-data.frame(
		  DEid = 1:nrow(DE_df_base),
		  DErecordType = "DE",
		  DEsamplingScheme = DEsamplingScheme,
		  DEsamplingSchemeType = "NatRouCF",
		  DEyear = as.integer(DEyear),
		  DEstratumName = DEstratumName,
		  DEhierarchyCorrect = "Y",
		  DEhierarchy = 1,
		  DEsampled = "Y",
		  DEreasonNotSampled = "",
		  DEnonResponseCollected = "Y",
		  DEauxiliaryVariableTotal = "",
		  DEauxiliaryVariableValue = "",
		  DEauxiliaryVariableName = "",
		  DEauxiliaryVariableUnit = "",
		  stringsAsFactors=FALSE
			)

#===SD============


                          # x
# 1            SDid [M] - int
# 2            DEid [M] - int
# 3 SDrecordType [M] - string
# 4  SDcountry [M] - ISO_3166
# 5  SDinstitution [M] - EDMO

SD_df<-data.frame(
  SDid=DE_df$DEid,
  DEid=DE_df$DEid,
  SDrecordType="SD",
  SDcountry="ZW",
  SDinstitution=as.integer(SDinstitution),
  stringsAsFactors=FALSE
)

#===VS============


                                                             # x
# 1                                               VSid [M] - int
# 2                                               SDid [M] - int
# 3                                               VDid [M] - int
# 4                                             TEid [M/O] - int
# 5                                    VSrecordType [M] - string
# 6                                   VSsequenceNumber [M] - int
# 7                  VSencryptedVesselCode [M] - StringLength100
# 8                        VSstratification [DV,M] - YesNoFields
# 9                       VSstratumName [DV,M] - StringLength100
# 10                            VSclustering [DV,M] - Clustering
# 11                      VSclusterName [DV,M] - StringLength100
# 12                                     VSsampler [O] - Sampler
# 13                                  VSnumberTotal [DV,O] - int
# 14                                VSnumberSampled [DV,O] - int
# 15        VSselectionProb [DV,O] - Decimal0.0000000000000001-1
# 16        VSinclusionProb [DV,O] - Decimal0.0000000000000001-1
# 17                  VSselectionMethod [DV,M] - SelectionMethod
# 18                         VSunitName [DV,M] - StringLength100
# 19           VSselectionMethodCluster [DV,O] - SelectionMethod
# 20                          VSnumberTotalClusters [DV,O] - int
# 21                        VSnumberSampledClusters [DV,O] - int
# 22 VSselectionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 23 VSinclusionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 24                              VSsampled [DV,M] - YesNoFields
# 25            VSreasonNotSampled [DV,O] - ReasonForNotSampling
# 26          	     VSnonResponseCollected [DV,O] - YesNoFields
# 27              VSauxiliaryVariableTotal [DV,O] - DecimalPrec3
# 28              VSauxiliaryVariableValue [DV,O] - DecimalPrec3
# 29      VSauxiliaryVariableName [DV,O] - AuxiliaryVariableName
# 30                      VSauxiliaryVariableUnit [DV,O] - MUNIT

#check_All_fields("VS")

# adds VSid to dataset
	dataset$VSid <- 1:nrow(dataset)

# creates a dummyVD and adds dataset
	# restricts VD_base to what is needed
	VD_base <- VD_base[1:nrow(dataset),]

	dataset$VSencryptedVesselCode<-VD_base$VDencryptedVesselCode
	# should be 0
	test<-sum(duplicated(dataset$VSencryptedVesselCode))==0
	if(!test) stop( "duplicated VSencryptedVesselCode")


VS_df <- data.frame(
  VSid = dataset$VSid,
  SDid = as.integer(1),
  VDid = "",
  TEid = "",
  VSrecordType = 'VS',
  VSsequenceNumber = 1:nrow(dataset),# M
  VSencryptedVesselCode = dataset$VSencryptedVesselCode, #M
  VSstratification = "N",
  VSstratumName = "U", #M
  VSclustering = "N", #M
  VSclusterName = "U", #M
  VSsampler = "Observer", #M
  VSnumberTotal = "",
  VSnumberSampled = "",
  VSselectionProb = "",
  VSinclusionProb = "",
  VSselectionMethod = "SRSWOR", #M
  VSunitName = paste(dataset$county, dataset$state, dataset$region, dataset$VSid),#M
  VSselectionMethodCluster = "",
  VSnumberTotalClusters = "",
  VSnumberSampledClusters = "",
  VSselectionProbCluster = "",
  VSinclusionProbCluster = "",
  VSsampled = "Y",#M
  VSreasonNotSampled = "",
  VSnonResponseCollected = "N",
  VSauxiliaryVariableTotal = "",
  VSauxiliaryVariableValue = "",
  VSauxiliaryVariableName = "",
  VSauxiliaryVariableUnit = "",
  stringsAsFactors=FALSE
  )

sampSize<-table(VS_df$VSstratumName)
VS_df$VSnumberSampled<-sampSize[VS_df$VSstratumName]
#strataSize<-c('NC' = 1054, 'NE' = 220, 'S' = 1382, 'W' = 422)
VS_df$VSnumberTotal<-3078

#====FT===========


# 1                                               FTid [M] - int
# 2                                             OSid [M/O] - int
# 3                                             VSid [M/O] - int
# 4                                               VDid [M] - int
# 5                                             SDid [M/O] - int
# 6                                             FOid [M/O] - int
# 7                                             TEid [M/O] - int
# 8                                    FTrecordType [M] - string
# 9                  FTencryptedVesselCode [M] - StringLength100
# 10                             	  FTsequenceNumber [M] - int
# 11                       FTstratification [DV,M] - YesNoFields
# 12                      FTstratumName [DV,M] - StringLength100
# 13                            FTclustering [DV,M] - Clustering
# 14                      FTclusterName [DV,M] - StringLength100
# 15                                     FTsampler [O] - Sampler
# 16                        FTsamplingType [M] - SamplingContext
# 17                  FTnumberOfHaulsOrSets [M/O] - IntZeroToMax
# 18                  FTdepartureLocation [M/O] - Harbour_LOCODE
# 19                                FTdepartureDate [M/O] - Date
# 20                 FTdepartureTime [M/O] - StringLength60
# 21                      FTarrivalLocation [M] - Harbour_LOCODE
# 22                                    FTarrivalDate [M] - Date
# 23                   FTarrivalTime [M/O] - StringLength60
# 24                                  FTnumberTotal [DV,O] - int
# 25                                FTnumberSampled [DV,O] - int
# 26                    FTselectionProb [DV,O] - Decimal0-1
# 27                    FTinclusionProb [DV,O] - Decimal0-1
# 28                  FTselectionMethod [DV,M] - SelectionMethod
# 29                         FTunitName [DV,M] - StringLength100
# 30           FTselectionMethodCluster [DV,O] - SelectionMethod
# 31                          FTnumberTotalClusters [DV,O] - int
# 32                        FTnumberSampledClusters [DV,O] - int
# 33             FTselectionProbCluster [DV,O] - Decimal0-1
# 34             FTinclusionProbCluster [DV,O] - Decimal0-1
# 35                              FTsampled [DV,M] - YesNoFields
# 36            FTreasonNotSampled [DV,O] - ReasonForNotSampling
# 37            FTnonResponseCollected [DV,O] - YesNoFields
# 38         FTauxiliaryVariableTotal [DV,O] - DecimalPrec3
# 39         FTauxiliaryVariableValue [DV,O] - DecimalPrec3
# 40 FTauxiliaryVariableName [DV,O] - AuxiliaryVariableName
# 41                 FTauxiliaryVariableUnit [DV,O] - MUNIT

FT_df <- data.frame(
  FTid = dataset$VSid,#[M] - int
  OSid = "",#  [M/O] - int
  VSid = dataset$VSid, #[M/O]
  VDid = "", #[M] - int
  SDid = "", #[M/O] - int
  FOid = "", #[M/O] - int
  TEid = "", #[M/O] - int
  FTrecordType='FT', #[M] - string
  FTencryptedVesselCode = dataset$VSencryptedVesselCode, #[M]
  FTsequenceNumber = as.integer(1:nrow(dataset)), #[M] - string
  FTstratification = "N", #[DV,M] - RS_Stratification
  FTstratumName = "U", #[DV,M] - string
  FTclustering = "N", #[DV,M] - RS_Clustering
  FTclusterName = "U", #[DV,M] - string
  FTsampler = "Observer", #[M] - RS_Sampler
  FTsamplingType = "AtSea" , #[M] - RS_SamplingType
  FTnumberOfHaulsOrSets = 1, #[O] - int
  FTdepartureLocation="ZWHWN", #[O] - Harbour_LOCODE
  FTdepartureDate=seq(from = as.Date("1968-01-01", format='%Y-%m-%d'), by=1, length.out=nrow(dataset)), #[M/O] - date
  FTdepartureTime="", #[O] - time
  FTarrivalLocation = "ZWHWN", #[M] - Harbour_LOCODE
  FTarrivalDate=seq(from = as.Date("1968-01-01", format='%Y-%m-%d'), by=1, length.out=nrow(dataset)), #[M] - date
  FTarrivalTime="", #[O] - time
  FTnumberTotal= 1, #[DV,O] - int
  FTnumberSampled=1, #[DV,O] - int
  FTselectionProb=1, #[DV,O] - DecimalPrec10
  FTinclusionProb=1, #[DV,O] - DecimalPrec10
  FTselectionMethod="CENSUS", #[DV,M] - RS_SelectionMethod
  FTunitName = dataset$VSid, #[DV,M] - string
  FTselectionMethodCluster="", #[DV,O] - RS_SelectionMethod
  FTnumberTotalClusters="", #[DV,O] - int
  FTnumberSampledClusters="", #[DV,O] - int
  FTselectionProbCluster="", #[DV,O] - DecimalPrec10
  FTinclusionProbCluster="", #[DV,O] - DecimalPrec10
  FTsampled="Y", #[DV,M] - YesNoFields
  FTreasonNotSampled= "", #[DV,O] - RS_ReasonForNotSampling
  FTnonResponseCollected = "N",
  FTauxiliaryVariableTotal = "",
  FTauxiliaryVariableValue = "",
  FTauxiliaryVariableName = "",
  FTauxiliaryVariableUnit = "",
  stringsAsFactors=FALSE
)


#====FO===========


# 1                                                                       FOid [M] - int
# 2                                                                     FTid [M/O] - int
# 3                                                                     SDid [M/O] - int
# 4                                                            FOrecordType [M] - string
# 5                                                FOstratification [DV,M] - YesNoFields
# 6                                                           FOsequenceNumber [M] - int
# 7                                               FOstratumName [DV,M] - StringLength100
# 8                                                     FOclustering [DV,M] - Clustering
# 9                                               FOclusterName [DV,M] - StringLength100
# 10                                                             FOsampler [O] - Sampler
# 11                                           FOaggregationLevel [M] - AggregationLevel
# 12                                                       FOvalidity [M] - ValidityFlag
# 13                                                  FOcatchReg [M] - CatchRegistration
# 14                                                              FOstartDate [O] - Date
# 15                                                              FOstartTime [O] - Time
# 16                                                                FOendDate [M] - Date
# 17                                                              FOendTime [M/O] - Time
# 18                                                              FOduration [M/O] - int
# 19                                               FOdurationSource [M] - DurationSource
# 20                                                            FOhandlingTime [O] - int
# 21                                        FOstartLat [O] - Decimal-90.000000-90.000000
# 22                                      FOstartLon [O] - Decimal-180.000000-180.000000
# 23                                         FOstopLat [O] - Decimal-90.000000-90.000000
# 24                                       FOstopLon [O] - Decimal-180.000000-180.000000
# 25                                     FOexclusiveEconomicZoneIndicator [O] - ISO_3166
# 26                                                              FOarea [M] - ICES_Area
# 27                                                           FOrectangle [O] - StatRec
# 28                                          FOfisheriesManagementUnit [O] - AreaNonFAO
# 29                                                   FOgsaSubarea [M] - Areas_GFCM_GSA
# 30                                           FOjurisdictionArea [O] - JurisdictionArea
# 31                                                            FOfishingDepth [O] - int
# 32                                                              FOwaterDepth [O] - int
# 33                             FOnationalFishingActivity [O] - NationalFishingActivity
# 34                                             FOmetier5 [O] - Metier5_FishingActivity
# 35                                             FOmetier6 [M] - Metier6_FishingActivity
# 36                                                               FOgear [M] - GearType
# 37                                                                FOmeshSize [O] - int
# 38                                             FOselectionDevice [O] - SelectionDevice
# 39                                                 FOselectionDeviceMeshSize [O] - int
# 40                                                 FOtargetSpecies [O] - TargetSpecies
# 41              FOincidentalByCatchMitigationDeviceFirst [M] - BycatchMitigationDevice
# 42  FOincidentalByCatchMitigationDeviceTargetFirst [M] - BycatchMitigationDeviceTarget
# 43             FOincidentalByCatchMitigationDeviceSecond [M] - BycatchMitigationDevice
# 44 FOincidentalByCatchMitigationDeviceTargetSecond [M] - BycatchMitigationDeviceTarget
# 45                                                          FOgearDimensions [O] - int
# 46                                             FOobservationCode [M] - ObservationCode
# 47                                                          FOnumberTotal [DV,O] - int
# 48                                                        FOnumberSampled [DV,O] - int
# 49                                FOselectionProb [DV,O] - Decimal0.0000000000000001-1
# 50                                FOinclusionProb [DV,O] - Decimal0.0000000000000001-1
# 51                                          FOselectionMethod [DV,M] - SelectionMethod
# 52                                                 FOunitName [DV,M] - StringLength100
# 53                                   FOselectionMethodCluster [DV,O] - SelectionMethod
# 54                                                  FOnumberTotalClusters [DV,O] - int
# 55                                                FOnumberSampledClusters [DV,O] - int
# 56                         FOselectionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 57                         FOinclusionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 58                                                      FOsampled [DV,M] - YesNoFields
# 59                                    FOreasonNotSampled [DV,O] - ReasonForNotSampling
# 60                                         FOnonResponseCollected [DV,O] - YesNoFields
# 61                                      FOauxiliaryVariableTotal [DV,O] - DecimalPrec3
# 62                                      FOauxiliaryVariableValue [DV,O] - DecimalPrec3
# 63                              FOauxiliaryVariableName [DV,O] - AuxiliaryVariableName
# 64                                              FOauxiliaryVariableUnit [DV,O] - MUNIT


FO_df <- data.frame(
	FOid = dataset$VSid,
	FTid = dataset$VSid,
	SDid = "",
	FOrecordType = "FO",#M
	FOstratification = "N",#M
	FOsequenceNumber = as.integer(1:nrow(dataset)),#M
	FOstratumName = "U",#M
	FOclustering = "N",#M
	FOclusterName = "U",#M
	FOsampler = "Observer",
	FOaggregationLevel = "H",#M
	FOvalidity = "V",#M
	FOcatchReg = "Lan",#M
	FOstartDate = as.Date(FT_df$FTdepartureDate, format='%Y-%m-%d'),
	FOstartTime = "",
	FOendDate = as.Date(FT_df$FTdepartureDate, format='%Y-%m-%d'),#M
	FOendTime ="",#M
	FOduration = 60, #M ATTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT!!!!
	FOdurationSource = "Crew",
	FOhandlingTime = "",
	FOstartLat="", # ATT!
	FOstartLon="", # ATT!
	FOstopLat="",
	FOstopLon="",
	FOexclusiveEconomicZoneIndicator = "", # might differ!!
	FOarea = "27.3.a.21", #M
	FOrectangle = "",
	FOfisheriesManagementUnit = "",
	FOgsaSubarea = "NotApplicable", #M
	FOjurisdictionArea = "",
	FOfishingDepth = "",
	FOwaterDepth = "",
	FOnationalFishingActivity = "",
	FOmetier5 = "",
	FOmetier6 = "OTT_CRU_70-89_2_35",#M
	FOgear = "OTT",#M
	FOmeshSize = "",
	FOselectionDevice = "",
	FOselectionDeviceMeshSize = "",
	FOtargetSpecies = "",
	FOincidentalByCatchMitigationDeviceFirst = "NotRecorded",#M
	FOincidentalByCatchMitigationDeviceTargetFirst = "NotApplicable",# [M]
	FOincidentalByCatchMitigationDeviceSecond = "NotRecorded",#M
	FOincidentalByCatchMitigationDeviceTargetSecond = "NotApplicable",#M
	FOgearDimensions = "",
	FOobservationCode = 'So', #M
	FOnumberTotal = 10,
	FOnumberSampled = 10,
	FOselectionProb = 1,
	FOinclusionProb = 1,
	FOselectionMethod = "CENSUS", #M
	FOunitName = VS_df$VSid, #M
	FOselectionMethodCluster = "",
	FOnumberTotalClusters = "",
	FOnumberSampledClusters = "",
	FOselectionProbCluster = "",
	FOinclusionProbCluster = "",
	FOsampled = "Y", #M
	FOreasonNotSampled = "",
	FOnonResponseCollected = "N",
	FOauxiliaryVariableTotal = "",
	FOauxiliaryVariableValue = "",
	FOauxiliaryVariableName = "",
	FOauxiliaryVariableUnit = "",
stringsAsFactors=FALSE
)


#===SS============

# 1                                                SSid [] - int
# 2                                             LEid [M/O] - int
# 3                                             FOid [M/O] - int
# 4                                             FTid [M/O] - int
# 5                                             OSid [M/O] - int
# 6                                             TEid [M/O] - int
# 7                                               SLid [M] - int
# 8                                    SSrecordType [M] - string
# 9                                   SSsequenceNumber [M] - int
# 10                       SSstratification [DV,M] - YesNoFields
# 11                      SSstratumName [DV,M] - StringLength100
# 12                            SSclustering [DV,M] - Clustering
# 13                      SSclusterName [DV,M] - StringLength100
# 14  SSobservationActivityType [DV,M] - ObservationActivityType
# 15                         SScatchFraction [M] - CatchFraction
# 16                SSobservationType [DV,M] - ObservationMethod
# 17                                     SSsampler [O] - Sampler
# 18                     SSspeciesListName [M] - StringLength100
# 19                     SSuseForCalculateZero [M] - YesNoFields
# 20                                       SStimeTotal [O] - int
# 21                                     SStimeSampled [O] - int
# 22                                  SSnumberTotal [DV,O] - int
# 23                                SSnumberSampled [DV,O] - int
# 24        SSselectionProb [DV,O] - Decimal0.0000000000000001-1
# 25        SSinclusionProb [DV,O] - Decimal0.0000000000000001-1
# 26                  SSselectionMethod [DV,M] - SelectionMethod
# 27                         SSunitName [DV,M] - StringLength100
# 28           SSselectionMethodCluster [DV,O] - SelectionMethod
# 29                          SSnumberTotalClusters [DV,O] - int
# 30                        SSnumberSampledClusters [DV,O] - int
# 31 SSselectionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 32 SSinclusionProbCluster [DV,O] - Decimal0.0000000000000001-1
# 33                              SSsampled [DV,M] - YesNoFields
# 34            SSreasonNotSampled [DV,O] - ReasonForNotSampling
# 35                SSnonResponseCollected [DV,O] - YesNoFields
# 36             SSauxiliaryVariableTotal [DV,O] - DecimalPrec3
# 37             SSauxiliaryVariableValue [DV,O] - DecimalPrec3
# 38     SSauxiliaryVariableName [DV,O] - AuxiliaryVariableName
# 39                     SSauxiliaryVariableUnit [DV,O] - MUNIT

SS_df<-data.frame(
	SSid = dataset$VSid,
	LEid = "",
	FOid = dataset$VSid,
	FTid = "",
	OSid = "",
	TEid = "",
	SLid = "",
	SSrecordType = "SS", #M
	SSsequenceNumber = as.integer(dataset$VSid), #M
	SSstratification = "N", #M
	SSstratumName = "U", #M
	SSclustering = "N", #M
	SSclusterName = "U", #M
	SSobservationActivityType = "Sort", #M
	SScatchFraction = "Lan", #M
	SSobservationType = "Volume", #M
	SSsampler = "Observer", #M
	SSspeciesListName = project_name_outputs, #M
	SSuseForCalculateZero = "N", #M
	SStimeTotal = "",
	SStimeSampled = "",
	SSnumberTotal = 1,
	SSnumberSampled = 1,
	SSselectionProb = 1,
	SSinclusionProb = 1,
	SSselectionMethod = "CENSUS",
	SSunitName = dataset$VSid,
	SSselectionMethodCluster = "",
	SSnumberTotalClusters = "",
	SSnumberSampledClusters = "",
	SSselectionProbCluster = "",
	SSinclusionProbCluster = "",
	SSsampled = "Y", #M,
	SSreasonNotSampled = "",
	SSnonResponseCollected = "N",
	SSauxiliaryVariableTotal = "",
	SSauxiliaryVariableValue = "",
	SSauxiliaryVariableName = "",
	SSauxiliaryVariableUnit = "",
	stringsAsFactors=FALSE
)

#====SA===========


# 1                                            SAid [] - int
# 2                                            SSid [] - int
# 3                                SArecordType [M] - string
# 4                               SAsequenceNumber [M] - int
# 5                         SAparentSequenceNumber [O] - int
# 6                    SAstratification [DV,M] - YesNoFields
# 7                   SAstratumName [DV,M] - StringLength100
# 8                            SAspeciesCode [M] - SpecWoRMS
# 9                         SAspeciesCodeFAO [O] - SpecASFIS
# 10             SAstateOfProcessing [M] - StateOfProcessing
# 11                SApresentation [M] - ProductPresentation
# 12                   SAspecimensState [M] - SpecimensState
# 13                     SAcatchCategory [M] - CatchCategory
# 14                 SAlandingCategory [O] - LandingCategory
# 15    SAcommSizeCatScale [O] - CommercialSizeCategoryScale
# 16              SAcommSizeCat [O] - CommercialSizeCategory
# 17                                       SAsex [M] - SEXCO
# 18         SAexclusiveEconomicZoneIndicator [O] - ISO_3166
# 19                                  SAarea [O] - ICES_Area
# 20                               SArectangle [O] - StatRec
# 21              SAfisheriesManagementUnit [O] - AreaNonFAO
# 22                       SAgsaSubarea [M] - Areas_GFCM_GSA
# 23               SAjurisdictionArea [O] - JurisdictionArea
# 24 SAnationalFishingActivity [O] - NationalFishingActivity
# 25                 SAmetier5 [O] - Metier5_FishingActivity
# 26                 SAmetier6 [O] - Metier6_FishingActivity
# 27                                   SAgear [O] - GearType
# 28                                    SAmeshSize [O] - int
# 29                 SAselectionDevice [O] - SelectionDevice
# 30                     SAselectionDeviceMeshSize [O] - int
# 31                           SAunitType [M] - SamplingUnit
# 32                             SAtotalWeightLive [O] - int
# 33                            SAsampleWeightLive [O] - int
# 34             SAnumberTotal [DV,O] - Decimal0.1-999999999
# 35           SAnumberSampled [DV,O] - Decimal0.1-999999999
# 36                     SAselectionProb [DV,O] - Decimal0-1
# 37                     SAinclusionProb [DV,O] - Decimal0-1
# 38              SAselectionMethod [DV,M] - SelectionMethod
# 39                     SAunitName [DV,M] - StringLength100
# 40                 SAlowerHierarchy [M/O] - LowerHierarchy
# 41                                 SAsampler [O] - Sampler
# 42                          SAsampled [DV,M] - YesNoFields
# 43        SAreasonNotSampled [DV,O] - ReasonForNotSampling
# 44             SAnonResponseCollected [DV,O] - YesNoFields
# 45      SAreasonNotSampledFM [DV,O] - ReasonForNotSampling
# 46      SAreasonNotSampledBV [DV,O] - ReasonForNotSampling
# 47                         SAtotalWeightMeasured [O] - int
# 48                        SAsampleWeightMeasured [O] - int
# 49        SAconversionFactorMeasLive [O] - Decimal0.900-10
# 50          SAauxiliaryVariableTotal [DV,O] - DecimalPrec3
# 51          SAauxiliaryVariableValue [DV,O] - DecimalPrec3
# 52  SAauxiliaryVariableName [DV,O] - AuxiliaryVariableName
# 53                  SAauxiliaryVariableUnit [DV,O] - MUNIT


SA_df<-data.frame(
		SAid = dataset$VSid,
		SSid = dataset$VSid,
		SArecordType = "SA", #M
		SAsequenceNumber = dataset$VSid, #M
		SAparentSequenceNumber = "",
		SAstratification = "N", #M
		SAstratumName = "U", #M
		SAspeciesCode = 107254,  #M
		SAspeciesCodeFAO = "NEP",
		SAstateOfProcessing = "FRE", #M
		SApresentation = "WHL", #M
		SAspecimensState = "DeadOrZeroProbSurvival", #M
		SAcatchCategory = "Lan", #M
		SAlandingCategory = "",
		SAcommSizeCatScale = "",
		SAcommSizeCat = "",
		SAsex = "U", #M
		SAexclusiveEconomicZoneIndicator = "",
		SAarea = ifelse(dataset$farmcat=='large','27.4.a','27.3.a.20'), # this is the domain
		SArectangle = "",
		SAfisheriesManagementUnit = "",
		SAgsaSubarea = "NotApplicable", #M
		SAjurisdictionArea = "",
		SAnationalFishingActivity = "",
		SAmetier5 = "",
		SAmetier6 = "",
		SAgear = "",
		SAmeshSize = "",
		SAselectionDevice = "",
		SAselectionDeviceMeshSize = "",
		SAunitType = "Box", #M
		SAtotalWeightLive = "",
		SAsampleWeightLive = "",
		SAnumberTotal = 1,
		SAnumberSampled = 1,
		SAselectionProb = 1,
		SAinclusionProb = 1,
		SAselectionMethod = "CENSUS", #M
		SAunitName = dataset$VSid, #M
		SAlowerHierarchy = "D",
		SAsampler = "Observer",
		SAsampled = "N", #M
		SAreasonNotSampled = "",
		SAnonResponseCollected = "N",
		SAreasonNotSampledFM = "",
		SAreasonNotSampledBV = "",
		SAtotalWeightMeasured = dataset[[target_var]],
		SAsampleWeightMeasured = dataset[[target_var]],
		SAconversionFactorMeasLive = 1,
		SAauxiliaryVariableTotal = "",
		SAauxiliaryVariableValue = "",
		SAauxiliaryVariableName = "",
		SAauxiliaryVariableUnit = "",
		stringsAsFactors=FALSE
)



#====Builds final format===========


RDBESlist = list(DE = DE_df,SD = SD_df, VS = VS_df, FT = FT_df, FO = FO_df, SS = SS_df, SA = SA_df)

#id table
a<-merge(DE_df["DEid"],SD_df[c("DEid","SDid")])
a<-merge(a, VS_df[c("SDid","VSid")], all.x=T)
a<-merge(a, FT_df[c("VSid","FTid")], all.x=T)
a<-merge(a, FO_df[c("FTid","FOid")], all.x=T)
a<-merge(a, SS_df[c("FOid","SSid")], all.x=T)
a<-merge(a, SA_df[c("SSid","SAid")], all.x=T)

# reorder columns
a<-a[c("DEid","SDid","VSid","FTid","FOid","SSid","SAid")]
# reorder rows
a<-data.table(a)
a<-a[order(DEid,SDid,VSid,FTid,FOid,SSid,SAid),]

a$DEindex=apply(a[,1:which(colnames(a)=="DEid")],1,paste, collapse="_")
a$SDindex=apply(a[,1:which(colnames(a)=="SDid")],1,paste, collapse="_")
a$VSindex=apply(a[,1:which(colnames(a)=="VSid")],1,paste, collapse="_")
a$FTindex=apply(a[,1:which(colnames(a)=="FTid")],1,paste, collapse="_")
a$FOindex=apply(a[,1:which(colnames(a)=="FOid")],1,paste, collapse="_")
a$SSindex=apply(a[,1:which(colnames(a)=="SSid")],1,paste, collapse="_")
a$SAindex=apply(a[,1:which(colnames(a)=="SAid")],1,paste, collapse="_")

key<-c(a$DEindex[match(DE_df$DEid,a$DEid)],
a$SDindex[match(SD_df$SDid,a$SDid)],
a$VSindex[match(VS_df$VSid,a$VSid)],
a$FTindex[match(FT_df$FTid,a$FTid)],
a$FOindex[match(FO_df$FOid,a$FOid)],
a$SSindex[match(SS_df$SSid,a$SSid)],
a$SAindex[match(SA_df$SAid,a$SAid)]
)

# file production
Oldscipen<-.Options$scipen
options(scipen=500)

#remove all id
for (i in names(RDBESlist))
{
RDBESlist[[i]][which(grepl(colnames(RDBESlist[[i]]),pat="[A-Z]id"))]<-NULL
}

#===Save============

	dir_outputs<-paste0(base_dir_outputs,"/",
	                    project_name_outputs,"/")
  dir.create(dir_outputs, recursive=T, showWarnings=FALSE)
	filename_output_CS <- paste0("H1.csv")
	filename_output_SL <- paste0("HSL.csv")
	filename_output_VD <- paste0("HVD.csv")


lapply(RDBESlist, function(x, filename1 = paste0(dir_outputs,filename_output_CS)){
if("DErecordType" %in% colnames(x)){
	write.table(x, file = filename1, append = FALSE, quote = FALSE, sep = ",",
				eol = "\n", na = "NA", dec = ".", row.names = FALSE,
				col.names = FALSE, qmethod = c("escape", "double"))
	} else {
write.table(x, file = filename1, append = TRUE, quote = FALSE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = FALSE, qmethod = c("escape", "double"))
			}
})

b<-read.table(file=paste0(dir_outputs,filename_output_CS), header=F, sep=";")
b<-cbind(key,b)
b<-b[order(as.character(b$key), decreasing=FALSE),]
b<-b[!is.na(key),]
b$key<-NULL
b$V1<-as.character(b$V1)

# saves CS output
write.table(b$V1, file=paste0(dir_outputs,filename_output_CS), col.names=FALSE, row.names = FALSE, quote=FALSE,sep=",")

# -----Builds and saves dummySL-----------------


	SL_base$SLspeciesListName<-project_name_outputs
	SL_base$SLyear<-DEyear
	SL_base$SLinstitute<-SDinstitution
# saves SL output
	write.table(SL_base,  file=paste0(dir_outputs,filename_output_SL), col.names=FALSE, row.names = FALSE, quote=FALSE,sep=",")


# -----Builds and saves dummyVD-----------------


# saves VD output
	VD_base$VDyear<-DEyear
	write.table(VD_base, file=paste0(dir_outputs,filename_output_VD), col.names=FALSE, row.names = FALSE, quote=FALSE,sep=",")



# -----Clean SL after dowload-----------------

# cleans excess of SL rows frequently present in download
# note:
# since it is an example - nicer to do here, fixing directly data input to scripts than patching it over multiple scripts later
# since example data are very simple, a simple select is done on specieaListName - this may not be sufficient in some minor cases

filename_download <- "2022_10_14_062441.zip"
unzip(paste0(dir_outputs, filename_download), exdir = paste0(dir_outputs,"tmp"))
# reads file to be fixed, fixes it and saves it back overwriting
tmp<-fread(paste0(dir_outputs,"tmp/SpeciesList.csv"))
tmp<-tmp[SLspeciesListName==project_name_outputs,]
write.csv(tmp, file=paste0(dir_outputs,"tmp/SpeciesList.csv"), quote=F, row.names=F)
# re-zips the file, puts it in final dir and deletes tmp dir

zip::zip(zipfile = paste0(project_name_outputs,".zip"), files =paste0(dir_outputs,"tmp/",dir(paste0(dir_outputs,"tmp"))), mode="cherry-pick")
unlink(paste0(dir_outputs,"tmp"), force=T, recursive = T)


