# RDBEScore (development version)

- New stage-wise estimation for upper hierarchy 1 (landings): `doRatioEstimation(obj, species, raising = raisingScheme(...))` estimates numbers, mean weight and mean length at age or length per domain. The samples are raised within the sample, then stage by stage (vessels, trips, hauls) with a method chosen per stage: `"none"`, `"count"` (N / n), `"aux"` (RDBES auxiliary fields), `"ratio"` (any columns or a function), `"external"` (user tables via `externalAux()`) or `"prob"` (reported inclusion probabilities), within design strata. The top is raised to the DE totals (`DEauxVarTot` per DE stratum) or not at all. Selection methods are logged, not used. Species selection (SS) is always 1.
- Each step is exported and returns an inspectable table: `getSampleUnits()`, `expandWithinSample()`, `getUnitTable()`, `computeStageFactors()`, `getStageWeights()`, `raiseToDE()`, `summariseRatioEstimates()`, `summariseRatioDomains()`. Schemes are built with `raisingScheme()`, `stageRaising()`, `sampleRaising()`, `deRaising()`, `totalsRaising()` and `externalAux()`. Assumptions and exclusions are collected in a log, read with `getRatioLog()`.
- `getSampleUnits()` and `doRatioEstimation()` take species as AphiaIDs only (`SAspeCode`), so that the zero and NA rows of `generateZerosUsingSL()` and `generateNAsUsingSL()` are found.
- `summariseRatioEstimates()` columns carry units: `meanWeight_g`, `meanLength_mm`, `totalWeight_kg`, `lengthClass_mm`.
- The result of `doRatioEstimation()` is a list of tables, each named by what one row of it is: `estimates` (domain × class), `domains` (domain), `ratios$DE` (DE stratum), `ratios$totals` (totals stratum), `samples` (lowest-level SA, key columns first), `stageUnits` (one table per stage), `fishBySample` (sample × class) and `byStratum` (domain × DE stratum × class). The per-domain columns moved from `estimates` to `domains`, made by the new `summariseRatioDomains()`. Old element names: `sampleUnits` and `stageWeights` are now `samples`, `expanded` is `fishBySample`, `raised` is `byStratum`, and the attributes `deRatios`, `totalsRatios` and `unitWeights` are `ratios$DE`, `ratios$totals` and `stageUnits`.
- New `ratioOutputGuide()` explains every column of the `doRatioEstimation()` result: element, what one row is, unit and meaning. A test checks that every output column is in it.
- Columns renamed so that multipliers start with `f` (one step), `w` (product of steps) or `r` (ratio), and fixed units are in the name:

  | Old | New |
  |---|---|
  | `raiseFactor` | `fSample` |
  | `x` (per sample) | `xSample` |
  | `factor` (`computeStageFactors()`, unit weights) | `fStage` |
  | `wUnit` | `wCumulative` |
  | `w` | `wStages` |
  | `ratio` (DE ratios) | `rDE` |
  | `ratio` (totals ratios) | `rTotals` |
  | `ratio` (per domain) | `rApplied` |
  | `T` | `total` |
  | `sumWX` | `xWeightedStratum` |
  | `sumX`, `sumX_kg` | `liveWeightSampled_kg` |
  | `X`, `X_kg` | `liveWeightEst_kg` |
  | `yW`, `yL` | `yW_g`, `yL_mm` |
  | `W`, `L` | `W_g`, `L_mm` |
  | `stratum` | `DEstratumName` |
- `getSampleUnits()` excludes, and logs, lowest-level SA records of lower hierarchy A or B without FM, or C without BV. An SA with total weight 0 (e.g. from `generateZerosUsingSL()`) is a recorded zero. `getStageWeights()` keeps sampled hauls without the species, or with a recorded zero, as zero catch, and treats a sampled haul whose samples of the species are all excluded as non-response, which cascades to trips and vessels.
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


