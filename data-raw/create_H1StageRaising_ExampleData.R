## Simulated hierarchy 1 data for the stage-wise raising vignette
##
## Built with the test simulator (tests/testthat/helper-simulateH1.R), which
## generates every fish of every haul, so the true numbers at age are known.
## Run from the RDBEScore repository root.
##
## Design:
## - 4 vessels (the whole population), 2 trips each, 6 hauls per trip
## - quarters 1 and 2, areas 27.4.a and 27.4.b, one metier
## - unequal haul sampling linked to the age composition: the vessels with
##   young fish (1, 2) have 1 haul sampled per trip, the vessels with old fish
##   (3, 4) have all 6 hauls sampled
## - one box of at most 40 fish per sampled haul, all fish aged (lower
##   hierarchy C)
## - selection methods: all vessels and trips (CENSUS), hauls by SRSWOR
## - FOsamp = "N" for hauls not sampled, XXincProb, XXauxVarValue (landed
##   live weight of the unit, kg) and XXauxVarTot (its parent total) at every
##   stage, DEauxVarTot = landed live weight of the population (kg)

library(data.table)
devtools::load_all(".")
source("tests/testthat/helper-simulateH1.R")

sim <- simulateH1RatioData(seed = 1, nVessels = 4, tripsPerVessel = 2,
                           haulsPerTrip = 6,
                           sampledHaulsPerTrip = c(1, 1, 1, 1, 6, 6, 6, 6),
                           vesselsInPopulation = 4,
                           vesselEffect = c(-0.5, -0.5, 0.5, 0.5),
                           boxFish = 40, lowHierarchy = "C",
                           selectMeth = c(VS = "CENSUS", FT = "CENSUS",
                                          FO = "SRSWOR"))

H1StageRaisingExample <- sim$obj
validateRDBESDataObject(H1StageRaisingExample)

# True numbers and live weight (g) at age per domain
H1StageRaisingTruth <- sim$truthAge[, .(year, quarter, area, metier6, age,
                                        N, W)]

usethis::use_data(H1StageRaisingExample, overwrite = TRUE, compress = "xz")
usethis::use_data(H1StageRaisingTruth, overwrite = TRUE, compress = "xz")
