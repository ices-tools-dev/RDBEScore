# RDBEScore (development version)

- New stage-wise estimation for upper hierarchy 1 (landings): `doRatioEstimation(obj, species, raising = raisingScheme(...))` estimates numbers, mean weight and mean length at age or length per domain. The samples are raised within the sample, then stage by stage (vessels, trips, hauls) with a method chosen per stage: `"none"`, `"count"` (N / n), `"aux"` (RDBES auxiliary fields), `"ratio"` (any columns or a function), `"external"` (user tables via `externalAux()`) or `"prob"` (reported inclusion probabilities), within design strata. The top is raised to the DE totals (`DEauxVarTot` per DE stratum) or not at all. Selection methods are logged, not used. Species selection (SS) is always 1.
- Each step is exported and returns an inspectable table: `getSampleUnits()`, `expandWithinSample()`, `getUnitTable()`, `computeStageFactors()`, `getStageWeights()`, `raiseToDE()`, `summariseRatioEstimates()`. Schemes are built with `raisingScheme()`, `stageRaising()`, `topRaising()` and `externalAux()`. Assumptions and exclusions are collected in a log, read with `getRatioLog()`.
- `summariseRatioEstimates()` columns carry units: `meanWeight_g`, `meanLength_mm`, `totalWeight_kg`, `X_kg`, `sumX_kg`, `lengthClass_mm`.
- `getSampleUnits()` excludes, and logs, lowest-level SA records of lower hierarchy A or B without FM, or C without BV.
- New vignette "03c Estimating numbers at age with stage-wise raising", with the simulated example data `H1StageRaisingExample` and its true numbers at age `H1StageRaisingTruth` (built in `data-raw/create_H1StageRaising_ExampleData.R`).
- New `getRDBESCodeList()` and `checkRDBESCodes()` check coded fields against the live ICES code lists (`icesVocab`, now in Imports). `classifySelectionMethod()` classifies selection methods by design quality.

# RDBEScore 0.3.5

- Bug fix: addressed [#251](https://github.com/ices-tools-dev/RDBEScore/issues/251).
- Docs/params: expanded docs for `combineRDBESDataObjects()` and `createRDBESDataObject()`; clarified hierarchy behavior and `...` options (strict, verbose, hierarchy).
- Mixed hierarchies: `combineRDBESDataObjects()` now warns/errors when objects use different hierarchies (`strict=TRUE` for error).
- ID tables: `createTableOfRDBESIds()` merging more robust by hierarchy; clearer BV handling and console output.

# RDBEScore 0.3.4

- Defaults: `createRDBESDataObject()`  now runs validation by default
- Estimation object: added `incDesignVariables` to `createRDBESEstObject()` to optionally drop design variables; convert character columns to factors to reduce size.
- SA sub-sampling: replaced recursive logic with a self-join + lookup (`prepareSubSampleLevelLookup`), with warnings for missing or non‑unique matches.
- Memory: added frequent `gc()` calls across estimation/join steps for large data.
- Joins: improved field selection for hierarchy 7 in `procRDBESEstObjUppHier()`; clarified logic for selecting `VDid` fields.
- Docs/CI: added pkgdown GitHub Actions workflow; improved function docs (params/returns); updated `.Rbuildignore` and package URLs.
- Vignettes: updated estimation workflow and sub-sampling sections; added memory tips, minimal examples, and brief benchmarking notes.
- Performance: `filterRDBESDataObject()` now uses `data.table` for faster filtering.

# RDBEScore 0.3.3 

* Update to work with with the 2025 RDBES data call format

# RDBEScore 0.3.2 

* Can import zip files of the updated download format where each hierarchy is in a separate directory
* added optional install method to readme

# RDBEScore 0.3.1 

* update to the latest RDBES data format (version 1.19.20)

# RDBEScore 0.3.0 - 20/11/2023

This version introduces a bunch of changes to the package. The updated behaviour is best explained in the vignettes. The main changes are:

* The package has been updated to use the newest RDBES data format (version 1.19.18)
* S3 methods print, summary and sort have been added for the `RDBESDataObject` class
* All vignettes are updated to reflect the changes in the package
* A lot of example data is added to the package from packages survey and SDAResources
* Example data for RDBES hierarchies 1, 5 and 8 is added to the package data
* function `createRDBESDataObject` is now the only data import function in the package
*  `createRDBESDataObject` accepts .zip files, al list of data frames and folder of. csv files as input
* a `strict` parameter is added to `validateRDBESDataObject` to control the strictness of the validation
* A new set of tests have been added to the package

Also some minor fixes to functions have been added.

# RDBEScore 0.2.0

* `generateZerosUsingSL`: fixed behaviour and added tests for function

# RDBEScore 0.1.0

* `RDBESRawObjects` (and associated functions) have been renamed to `RDBESDataObjects`. Code from previous versions of the package will need to be updated. 


