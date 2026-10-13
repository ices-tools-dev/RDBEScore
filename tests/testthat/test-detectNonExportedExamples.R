#' - AI-assisted: Yes
#' - Review:
#' - Notes/scope: not really reviewed, just worked when needed
#'

test_that("detectNonExportedExamples finds non-exported functions in examples", {
  pkg_path <- tempfile("testpkg-")
  dir.create(pkg_path)
  dir.create(file.path(pkg_path, "R"))
  dir.create(file.path(pkg_path, "man"))

  # Minimal package metadata
  writeLines(
    c(
      "Package: testpkg",
      "Title: Test Package",
      "Version: 0.0.1",
      "Description: A package for testing.",
      "License: MIT"
    ),
    file.path(pkg_path, "DESCRIPTION")
  )

  # Define one exported and one internal function
  writeLines(
    c(
      "export(public_fun)"
    ),
    file.path(pkg_path, "NAMESPACE")
  )

  writeLines(
    c(
      "public_fun <- function(x) x",
      "internal_fun <- function(x) x"
    ),
    file.path(pkg_path, "R", "functions.R")
  )

  # Help page example calls the internal function
  writeLines(
    c(
      "\\\\name{public_fun}",
      "\\\\alias{public_fun}",
      "\\\\title{Public function}",
      "\\\\description{A test help page.}",
      "\\\\examples{",
      "public_fun(1)",
      "internal_fun(1)",
      "}"
    ),
    file.path(pkg_path, "man", "public_fun.Rd")
  )

  result <- detectNonExportedExamples(pkg_path)

  expect_true(is.data.frame(result))
  expect_true(all(c(
    "topic", "function_name", "call_type", "defined_in_package"
  ) %in% names(result)))

  expect_true(any(
    result$function_name == "internal_fun" &
      result$defined_in_package
  ))
})

test_that("detectNonExportedExamples ignores exported functions", {
  pkg_path <- tempfile("testpkg-")
  dir.create(pkg_path)
  dir.create(file.path(pkg_path, "R"))
  dir.create(file.path(pkg_path, "man"))

  writeLines(
    c(
      "Package: testpkg",
      "Title: Test Package",
      "Version: 0.0.1",
      "Description: A package for testing.",
      "License: MIT"
    ),
    file.path(pkg_path, "DESCRIPTION")
  )

  # Both functions are defined, but only public_fun is exported
  writeLines(
    "export(public_fun)",
    file.path(pkg_path, "NAMESPACE")
  )

  writeLines(
    c(
      "public_fun <- function(x) x",
      "internal_fun <- function(x) x"
    ),
    file.path(pkg_path, "R", "functions.R")
  )

  # The example calls only the exported function
  writeLines(
    c(
      "\\name{public_fun}",
      "\\alias{public_fun}",
      "\\title{Public function}",
      "\\description{A test help page.}",
      "\\examples{",
      "public_fun(1)",
      "}"
    ),
    file.path(pkg_path, "man", "public_fun.Rd")
  )

  result <- detectNonExportedExamples(pkg_path)

  expect_false(any(result$function_name == "public_fun"))
})

