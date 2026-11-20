#' Detect Non-Exported Functions Used in Package Examples
#'
#' Scans the \file{man/} directory of an R package and identifies calls to
#' non-exported functions in the examples sections of its Rd documentation.
#' Function definitions are collected from the package's \file{R/} directory,
#' and exported function names are read from the package's
#' \file{NAMESPACE} file.
#'
#' The function does not execute examples. It parses the documentation and
#' source files to identify candidate calls to non-exported functions.
#'
#' @param pkgPath Character string specifying the path to the root directory
#'   of an R package source tree. The directory should contain \file{man/},
#'   \file{R/}, and \file{NAMESPACE}.
#'
#' @return A data frame with one row per detected call and the following
#'   columns:
#'   \describe{
#'     \item{topic}{Name of the Rd documentation topic in which the call
#'       was detected, usually the filename without the \file{.Rd} extension.}
#'     \item{function_name}{Name of the non-exported function detected in
#'       the example.}
#'     \item{call_type}{Type of call: \code{unqualified} for a direct
#'       function call, or \code{explicit :::} for an explicitly
#'       namespace-qualified call using the triple-colon operator.}
#'     \item{defined_in_package}{Logical value indicating whether the
#'       function was found among the function definitions detected in the
#'       package's \file{R/} source files.}
#'   }
#'
#'   If no matching calls are found, an empty data frame with these columns
#'   is returned.
#'
#' @details
#' The function identifies two types of calls:
#' \itemize{
#'   \item Unqualified calls to functions defined in the package source
#'     that are not listed as exports in \file{NAMESPACE}.
#'   \item Calls using \code{pkg:::fun()}, where \code{pkg} matches the
#'     package directory name and \code{fun} is not exported.
#' }
#'
#' The \code{defined_in_package} column is based on static inspection of
#' source files. A value of \code{FALSE} for an explicitly qualified call
#' means that a matching direct function definition was not found. This
#' does not necessarily mean that the function is absent; it may be generated
#' dynamically or defined in a form not recognized by the detector.
#'
#' The analysis is static and may not identify calls created dynamically
#' through mechanisms such as \code{get()}, \code{assign()}, or
#' metaprogramming. It also relies on the package's source files and
#' \file{NAMESPACE} being consistent with the documentation being inspected.
#'
#' @seealso [tools::parse_Rd()], [tools::Rd2ex()]
#'
#' @section Development review:
#' - AI-assisted: Yes
#' - Review:
#' - Notes/scope: not really reviewed, just worked when needed and could be useful
#' in the future
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Scan a local package source directory
#' issues <- detectNonExportedFunctionsInExamples("path/to/package")
#'
#' # Display all detected calls
#' print(issues)
#'
#' # Inspect calls whose definitions were not found in the source
#' subset(issues, defined_in_package == FALSE)
#' }
#'

detectNonExportedFunctionsInExamples <- function(pkgPath) {
  stopifnot(dir.exists(pkgPath))

  man_dir <- file.path(pkgPath, "man")
  r_dir <- file.path(pkgPath, "R")
  ns_file <- file.path(pkgPath, "NAMESPACE")

  if (!dir.exists(man_dir)) {
    stop("Cannot find package man/ directory: ", man_dir)
  }

  # --------------------------------------------------
  # 1. Read exported function names from NAMESPACE
  # --------------------------------------------------
  exports <- character()

  if (file.exists(ns_file)) {
    ns_lines <- readLines(ns_file, warn = FALSE)

    export_lines <- grep(
      "^\\s*export\\s*\\(",
      ns_lines,
      value = TRUE
    )

    exports <- unlist(
      lapply(export_lines, function(line) {
        matches <- regmatches(
          line,
          gregexpr(
            'export\\s*\\(\\s*["\u0060]?[^)"\u0060]+\\)',
            line,
            perl = TRUE
          )
        )
        matches
      })
    )

    # Parse NAMESPACE directives properly where possible
    ns_exprs <- tryCatch(
      parse(ns_file),
      error = function(e) expression()
    )

    exports <- unique(unlist(lapply(ns_exprs, function(expr) {
      if (is.call(expr) &&
          identical(expr[[1L]], as.name("export"))) {
        as.character(expr[-1L])
      } else {
        character()
      }
    })))
  }

  # --------------------------------------------------
  # 2. Find functions defined in the package's R files
  # --------------------------------------------------
  internal_functions <- character()

  if (dir.exists(r_dir)) {
    r_files <- list.files(
      r_dir,
      pattern = "\\.[Rr]$",
      full.names = TRUE,
      recursive = TRUE
    )

    for (file in r_files) {
      exprs <- tryCatch(
        parse(file),
        error = function(e) expression()
      )

      find_definitions <- function(x) {
        out <- character()

        if (
          is.call(x) &&
          length(x) >= 3L &&
          (
            identical(x[[1L]], as.name("<-")) ||
            identical(x[[1L]], as.name("=")) ||
            identical(x[[1L]], as.name("<<-"))
          ) &&
          is.symbol(x[[2L]]) &&
          is.call(x[[3L]]) &&
          identical(x[[3L]][[1L]], as.name("function"))
        ) {
          out <- c(out, as.character(x[[2L]]))
        }

        if (is.call(x) || is.expression(x) || is.pairlist(x)) {
          for (i in seq_along(x)) {
            # Skip missing arguments
            if (identical(x[[i]], quote(expr = ))) {
              next
            }

            out <- c(out, find_definitions(x[[i]]))
          }
        }

        unique(out)
      }

      internal_functions <- c(
        internal_functions,
        unlist(lapply(exprs, find_definitions))
      )
    }
  }

  internal_functions <- unique(internal_functions)

  # --------------------------------------------------
  # 3. Extract calls from the original \examples{} tags
  # --------------------------------------------------
  rd_files <- list.files(
    man_dir,
    pattern = "\\.Rd$",
    full.names = TRUE,
    recursive = TRUE
  )

  results <- list()

  get_calls <- function(x) {
    calls <- character()

    if (is.call(x)) {
      head <- x[[1L]]

      # Ordinary function call: foo(...)
      if (is.symbol(head)) {
        calls <- c(calls, as.character(head))
      }

      # Namespace-qualified call: pkg::foo(...) or pkg:::foo(...)
      if (
        is.call(head) &&
        length(head) == 3L &&
        (
          identical(head[[1L]], as.name("::")) ||
          identical(head[[1L]], as.name(":::"))
        ) &&
        is.symbol(head[[2L]]) &&
        is.symbol(head[[3L]])
      ) {
        operator <- as.character(head[[1L]])

        calls <- c(
          calls,
          paste0(
            as.character(head[[2L]]),
            operator,
            as.character(head[[3L]])
          )
        )
      }

      # Traverse arguments, skipping missing arguments
      for (i in seq_along(x)[-1L]) {
        arg <- tryCatch(x[[i]], error = function(e) NULL)

        if (!is.null(arg)) {
          calls <- c(calls, get_calls(arg))
        }
      }

    } else if (is.expression(x) || is.pairlist(x) || is.list(x)) {
      for (i in seq_along(x)) {
        el <- tryCatch(x[[i]], error = function(e) NULL)

        if (!is.null(el)) {
          calls <- c(calls, get_calls(el))
        }
      }
    }

    unique(calls)
  }

  # Recursively extract text under \examples{}
  extract_examples <- function(rd) {
    found <- list()

    walk <- function(node) {
      if (!is.list(node)) return()

      if (identical(attr(node, "Rd_tag"), "\\examples")) {
        found[[length(found) + 1L]] <<- node
        return()
      }

      for (child in node) {
        walk(child)
      }
    }

    walk(rd)
    found
  }

  for (file in rd_files) {
    rd <- tryCatch(
      tools::parse_Rd(file),
      error = function(e) NULL
    )

    if (is.null(rd)) next

    examples <- extract_examples(rd)
    if (!length(examples)) next

    topic <- sub("\\.Rd$", "", basename(file))

    for (example in examples) {
      # Convert Rd markup to example R code. This does not
      # execute the examples.
      code <- paste(
        unlist(lapply(example, function(part) {
          paste(as.character(part), collapse = "")
        })),
        collapse = "\n"
      )

      # Fallback to Rd2ex if direct extraction cannot be parsed
      exprs <- tryCatch(
        parse(text = code),
        error = function(e) NULL
      )

      if (is.null(exprs)) {
        tmp <- tempfile(fileext = ".R")
        tryCatch({
          tools::Rd2ex(rd, out = tmp)
          exprs <- parse(file = tmp)
        }, error = function(e) {
          exprs <<- NULL
        }, finally = {
          unlink(tmp)
        })
      }

      if (is.null(exprs)) next

      calls <- unique(unlist(lapply(exprs, get_calls)))

      for (call_name in calls) {
        # Namespace-qualified calls
        if (grepl("::", call_name, fixed = TRUE)) {
          parts <- strsplit(call_name, "::", fixed = TRUE)[[1L]]
          fn <- parts[length(parts)]
          operator <- if (grepl(":::", call_name, fixed = TRUE)) {
            ":::"
          } else {
            "::"
          }

          if (operator == ":::" &&
              parts[1L] == basename(pkgPath) &&
              !fn %in% exports) {
            results[[length(results) + 1L]] <- data.frame(
              topic = topic,
              function_name = fn,
              call_type = "explicit :::",
              defined_in_package = fn %in% internal_functions,
              stringsAsFactors = FALSE
            )
          }

          next
        }

        # Unqualified calls to package-defined functions
        if (call_name %in% internal_functions &&
            !call_name %in% exports) {
          results[[length(results) + 1L]] <- data.frame(
            topic = topic,
            function_name = call_name,
            call_type = "unqualified",
            defined_in_package = TRUE,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (!length(results)) {
    return(data.frame(
      topic = character(),
      function_name = character(),
      call_type = character(),
      defined_in_package = logical()
    ))
  }

  unique(do.call(rbind, results))
}
