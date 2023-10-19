
<!-- badges: start -->
[![R-CMD-check](https://github.com/ices-tools-dev/RDBEScore/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ices-tools-dev/RDBEScore/actions/workflows/R-CMD-check.yaml)
[![License](https://img.shields.io/badge/license-GPL%20(%3E%3D%202)-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!--
[![GitHub release](https://img.shields.io/github/release/ices-tools-prod/RDBEScore.svg?maxAge=2592001)]()
[![CRAN Monthly](http://cranlogs.r-pkg.org/badges/RDBEScore)](https://cran.r-project.org/package=RDBEScore)
[![CRAN Total](http://cranlogs.r-pkg.org/badges/grand-total/RDBEScore)](https://cran.r-project.org/package=RDBEScore)
-->

[<img align="right" alt="ICES Logo" width="17%" height="17%" src="http://ices.dk/_layouts/15/1033/images/icesimg/iceslogo.png">](http://ices.dk)

`RDBEScore`
=========

`RDBEScore` provides functions to work with the [**Regional DataBase and Estimation System (RDBES)**](https://rdbes.ices.dk/#/).

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

RDBEScore can be installed from GitHub using the `install_github`
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


