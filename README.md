
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
https://rdbes.ices.dk/

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

Contributions and the use of AI coding tools are tolerated, but all code must be tested and checked by humans to maintain the reliability of the package.

### 1. Getting started

Install a recent version of R (4.1 or newer), preferably with RStudio, and the packages `remotes`, `devtools`, `roxygen2` and `testthat`.

Use **camelCase** for new function names to match the existing package style. Two useful examples are [createRDBESDataObject](https://github.com/ices-tools-dev/RDBEScore/blob/dev/R/createRDBESDataObject.R) and the simpler [getTablesInRDBESHierarchy](https://github.com/ices-tools-dev/RDBEScore/blob/dev/R/getTablesInRDBESHierarchy.R).

The main package directories are:

- `R/` — R functions
- `tests/testthat/` — function tests
- `data/` — packaged datasets
- `data-raw/` — scripts preparing package data
- `vignettes/` — examples and user guides
- `WGRDBES-EST/` — additional documentation

### 2. Development workflow

1. **Create or claim a GitHub issue.** Describe what needs to be added or changed, including the expected behaviour. Assign yourself to the issue so others know you are working on it.

2. **Start from the latest `dev` branch.** Work directly on `dev` or create your own development branch based on it. The `main` branch is protected and only accepts changes through pull requests.

3. **Write or modify the code and tests.** You can do this manually or with AI coding tools. New functions should include roxygen2 documentation and appropriate tests. Add or update examples and vignettes when relevant. Aim to keep changes related to one issue together.

4. **Document AI involvement and human review.** Add the information described in Section 3 to every new or substantially modified function and test(s).

5. **Check the complete package** before integrating changes into `dev`. Regenerate documentation, run the tests and check the package:

   ```r
   devtools::document()
   devtools::test()
   devtools::check()
   ```

   Fix any problems introduced by your changes before submitting them.

6. **Commit your changes**, including the issue number in the commit message, for example: `Improve data validation #123` GitHub automatically links the commit to the issue, preserving the development history. Keep unrelated changes in separate commits whenever possible.

7. **Request additional review if needed.** Ask another contributor in the issue discussion or add the `human review required` label. Once the changes are ready, they can be incorporated into `dev`. Only changes considered ready for release should be transferred to `main`.

### 3. AI-assisted development and human review

Starting with version 0.3.5, contributors are allowed to use AI coding tools to accelerate development. However, all contributors remain responsible for understanding and verifying their code, regardless of whether it was written manually or generated with AI.

Every **new or substantially modified** function and test must document **AI involvement** and **human review**.

**Function documentation**

Include a `Development review` section in the function's roxygen2 documentation so that the information appears in the installed package's R help pages (`?functionName`).

```r
#' @section Development review:
#' - AI-assisted: Yes
#' - Human review: janesmith
#' - Notes/scope: Tested with hierarchy H5 using
#'   2025 RDBES data (downloaded 2026-10-14).
```

**Test documentation**

Add the same information as comments immediately before **each `test_that()` block**, rather than only at the beginning of the test file. Different tests may have different authors, reviewers or levels of verification.

```r
# AI-assisted: Yes
# Human review: janesmith
# Notes/scope: Expected results checked manually.
test_that("function returns correct values", {
  expect_equal(myFunction(2), 4)
})

# AI-assisted: No
# Human review: janesmith, johnbrown
test_that("function handles missing values", {
  expect_true(is.na(myFunction(NA)))
})
```

**Meaning of the fields**

- **AI-assisted:** `Yes` if AI tools were used to generate or substantially modify the current code or test, otherwise `No`. Routine autocomplete or minor wording corrections do not need to be reported.
- **Human review:** Github usernames of people who have checked and accepted the current implementation or test. The original contributor records one approval after checking their own work. Each additional reviewer who approves the current version is added.
- **Notes/scope (optional):** Briefly describe what has been verified, for example specific RDBES sampling hierarchies, datasets, reference calculations, or known limitations. Include data versions or dates when relevant.

**Updating review**

When another person reviews the code or test and approves it without changes, add their username:

```r
# Human review: janesmith, johnbrown
```

If a reviewer substantially modifies the code, previous review are no longer assumed to apply. The person making the changes checks the modified version and updates the approval record:

```r
# Human review: johnbrown
```

review for functions and tests are maintained separately. A change to one does not automatically invalidate approval of the other, unless its correctness is affected.

**Testing and scientific reliability**

All new or modified functionality should have appropriate tests. Expected test results must be independently established or checked by a human, rather than relying solely on AI-generated expectations. Where possible, statistical estimation functions should be tested against manually calculated results, published examples or independently verified reference implementations.

Additional independent human review is particularly encouraged for statistical estimation functions and scientifically important data transformations. The number of human review indicates the extent of documented human review, but does not by itself guarantee scientific correctness. The optional `Notes/scope` field can clarify what has actually been verified.

Existing functions and tests do not need to be retrospectively classified unless they are substantially modified.

### 4. Releasing reviewed changes to `main`

The `dev` branch may contain functions that are still being developed, tested or reviewed. **Only changes considered ready for release should be transferred to `main`.**

We aim at 2 releases a year (autumn and spring) and an online meeting will be scheduled to a approve the pull request to the main

When preparing a release a subset of the package contributors will:

1. Identify the functions and changes that are ready, considering their tests, human review and any review notes.
2. If only some changes from `dev` are ready, create a release branch from `main` and selectively transfer the required commits using Git cherry-pick. Include any dependent changes, tests and documentation.
3. Run `devtools::document()`, `devtools::test()` and `devtools::check()` on the complete release candidate.
4. Submit a pull request to `main` for final review and merging.


Changes that are not yet sufficiently reviewed remain in `dev`. The complete `dev` branch should only be merged into `main` when all included changes are ready for release.

Keeping commits focused on individual issues or functions makes selective releases easier.

### 5. On `data.table` usage

Objects of type `data.table` passed as parameters should be copied before modification to avoid unintentionally changing the original object by reference.

For example:

```r
zeroIds <- function(sl) {
  sl <- data.table::copy(sl)
  sl[, SLid := 0]
  sl
}
```

This ensures that calling `zeroIds()` does not modify the original input table.

### 6. Building binary packages (optional)

Binary packages can be built in RStudio using **Build → More → Build Binary Package**.

Alternatively, use the command line:

```bash
Rscript -e "roxygen2::roxygenize('.', roclets = c('rd', 'collate', 'namespace'))"
R CMD INSTALL --build .
```

Commands and executable names may vary slightly between operating systems. Building binary packages is not required for normal development.
