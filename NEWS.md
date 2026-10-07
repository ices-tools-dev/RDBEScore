# RDBEScore (development version)

- Estimation is done directly on the `RDBESDataObject` ([#15](https://github.com/ices-tools-dev/RDBEScore/issues/15)). The `RDBESEstObject` is deprecated: `createRDBESEstObject()` warns, and `createRDBESEstObject()`, `validateRDBESEstObject()` and `filterRDBESEstObject()` will be removed in a future version. `doEstimationForAllStrata()` no longer takes an `RDBESEstObject`.
- New `createDesignTree()`: one row per sampling unit of all tables with its parent unit and design variables. Handles stratification, clustering ("1C", "2C"), SA sub-sampling and lower hierarchies A, B and C. Stops on incomplete or inconsistent designs (selection methods without estimator, non-response, sample sizes different from `numSamp`, missing auxiliary values); `methodMap`, `nonResponse`, `strictSampleSize` and `varianceAsWR` make the alternatives explicit.
- New `doEstimationOnDesignTree()`: multistage generalised Horvitz-Thompson (multiple count) estimation of totals with variances that include the lower stages (Sarndal et al. 1992, Results 4.3.1 and 4.5.1), and ratio estimators at any stage (`ratio = c(SA = "weight")` or `c(<table> = "aux")`).
- New `doRatioEstimationToTotal()`: ratio estimation to known totals such as CL landings, with linearisation variance; with `X = 1` it estimates ratios and means.
- New `getLeafValuesSA()`, `getLeafValuesBV()`, `getLeafValuesFM()` and `getAncestorValue()`.
- `doEstimationForAllStrata()` now takes an `RDBESDataObject` and returns `est.total`, `var.between`, `var.within`, `var.total` and `se.total` for every stratum. The variances now include the within-unit variance of the lower stages; `est.mean` and `var.mean` are no longer returned.
- Vignette 03b compares the simple (`doBVestimCANUM()`) and the design-based estimates of numbers at age and at length side by side and shows why they differ; the package overview presentation is reorganised around the workflow and this comparison, and shows downloads with icesRDBES.
- Tests reproduce text book examples of Lohr (Sampling: Design and Analysis) and the survey package: simple random, stratified, one- and two-stage cluster sampling, ratio estimation and means.

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


