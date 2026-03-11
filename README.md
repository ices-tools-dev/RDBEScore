
[![R-CMD-check](https://github.com/ices-tools-dev/RDBEScore/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ices-tools-dev/RDBEScore/actions/workflows/R-CMD-check.yaml)
[![License](https://img.shields.io/badge/license-GPL%20(%3E%3D%202)-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!--
[![codecov](https://codecov.io/gh/ices-tools-prod/RDBEScore/branch/master/graph/badge.svg)](https://codecov.io/gh/ices-tools-prod/RDBEScore)
[![GitHub release](https://img.shields.io/github/release/ices-tools-prod/RDBEScore.svg?maxAge=2592001)]()
[![CRAN Status](http://www.r-pkg.org/badges/version/RDBEScore)](https://cran.r-project.org/package=RDBEScore)
[![CRAN Monthly](http://cranlogs.r-pkg.org/badges/RDBEScore)](https://cran.r-project.org/package=RDBEScore)
[![CRAN Total](http://cranlogs.r-pkg.org/badges/grand-total/RDBEScore)](https://cran.r-project.org/package=RDBEScore)
-->

[<img align="right" alt="ICES Logo" width="17%" height="17%" src="http://ices.dk/_layouts/15/1033/images/icesimg/iceslogo.png">](http://ices.dk)

`RDBEScore`
=========

`RDBEScore` provides functions to work with the [**Regional DataBase and Estimation System (RDBES)**](https://sboxrdbes.ices.dk/#/).

It is implemented as an [**R**](https://www.r-project.org) package and
available on <!-- [CRAN](https://cran.r-project.org/package=RDBEScore) --> 
[**GitHub**](https://github.com/ices-tools-dev/RDBEScore)

Installation
------------

<!--
RDBEScore can be installed from CRAN using the `install.packages` command:

```R
install.packages("RDBEScore")

```
-->

The most recent production version of the package can be installed from ICES tools (production). Theese are 
tools that are operational and maintained by the ICES Secretariat.

```R
install.packages("RDBEScore", repos = c("https://ices-tools-prod.r-universe.dev", "https://cloud.r-project.org"))
```

A more updated version of RDBEScore can be installed from GitHub using the `install_github`
command from the [remotes](https://remotes.r-lib.org/) package:

```R
library(remotes)

install_github("ices-tools-dev/RDBEScore", build_vignettes = TRUE)
```


Usage
-----

For a summary of the package see the following [Vignettes]():

```R
browseVignettes(package = "RDBEScore")
```

References
----------

* Regional Database & Estimation System:
https://sboxrdbes.ices.dk/

* Working Group on Governance of the Regional Database & Estimation System:
https://www.ices.dk/community/groups/Pages/WGRDBESGOV.aspx

* Working Group on Estimation with the RDBES data model (WGRDBES-EST):
https://github.com/ices-tools-dev/RDBEScore/blob/main/WGRDBES-EST/references/WGRDBES-EST%20Resolutions.pdf

* see also: https://github.com/ices-tools-dev/RDBEScore/tree/main/WGRDBES-EST/references

`RDBEScore (Development)`
=========

RDBEScore is developed openly on
[GitHub](https://github.com/ices-tools-dev/RDBEScore).

Feel free to open an
[issue](https://github.com/ices-tools-dev/RDBEScore/issues) there if you
encounter problems or have suggestions for future versions.

The current development version can be installed using:

```R
library(remotes)
install_github("ices-tools-dev/RDBEScore@dev")
```
If the installation fails due to R CMD, this alternative option can be used

```R
library(remotes)
install_github("ices-tools-dev/RDBEScore@dev", build = FALSE)
```

## How to Start developing

A good starting setup is a recent version of R, RStudio, and the packages commonly used for package development. `RDBEScore` currently requires R 4.1 or newer. In practice, most contributors will also want `remotes`, `devtools`, `roxygen2` and `testthat` installed locally. 

Use **camelCase** for new function names so they match the existing package style. A good function for a new contributor to take as an example is [createRDBESDataObject](https://github.com/ices-tools-dev/RDBEScore/blob/dev/R/createRDBESDataObject.R) but if you prefer something even more compact [getTablesInRDBESHierarchy](https://github.com/ices-tools-dev/RDBEScore/blob/dev/R/getTablesInRDBESHierarchy.R) is another good candidate. 

Development work should start from the `dev` branch. Before making changes, pull the latest `dev`, create or claim an issue, and make the changes locally. New package code should be added to the core package directories so that it is available to other contributors and can be built and checked in the normal package workflow. In most cases this means placing function code in `R/`, packaged data in `data/`, data-preparation scripts in `data-raw/`, tests in `tests/`, and user-facing examples or longer documentation in `vignettes/`. The repository also contains `WGRDBES-EST` directory for extra documentation.

When adding a new function, **aim to commit a complete unit of work rather than an unfinished fragment**. A typical contribution includes the function itself, roxygen2 documentation, any supporting example data, and tests for the new behaviour. If a change affects user workflows, add or update a vignette example when possible. This is more helpful to new users than adding code alone, and it makes review easier for maintainers.

Before pushing changes, run local checks. At a minimum, regenerate documentation if needed, run tests, and **run a package check**. A common local workflow is:

```r
devtools::document()
devtools::test()
#optionally do a full check as well
devtools::check()
```

## On `data.table` usage

Objects of type `data.table` passed as parameters should be copied before modification. As an example:
```r
function zeroIds(sl) {
  sl <- data.table::copy(sl)
  sl[,SLid:=0]
  sl
}
```
Now invocations of this function on SL tables will not alter the original copy.

## Precommit-hook framework

For adhering to package styling guides it is advisable to use precommit checks while developing.
In order to run lintr and styler (etc) before committing code, follow the instructions at the following address: https://github.com/lorenzwalthert/precommit#installation

1. Check that python3 is installed and install if necessary
2. Run pip install
   ```bash
   pip3 install pre-commit --user
   ```
3. Install R precommit package
   ```r
   install.packages("precommit")
   ```
4. Run at the root of the git repository.
   ```r
   library(precommit)
   precommit::use_precommit()
   ```
5. Running git commit should run the various checks automatically. See [the config file](.pre-commit-config.yaml) for all the checks.

## Building binary packages

Building binary packages can be achieved by running the following commands.
```bash
Rscript.exe -e "roxygen2::roxygenize('.', roclets = c('rd', 'collate', 'namespace'))"
Rcmd.exe INSTALL --build --preclean .
```
Note to replace `.` with the relative path of the project if necessary. Also on some operating systems, `Rcmd.exe` is called `Rcmd` and `Rscript.exe` is `Rcmd` respectively.

In R-studio there is also a button on the `build` - tab: `More -> Build Binary Package` that builds the binary package.
