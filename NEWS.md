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


