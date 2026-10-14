# Constructors for stage-wise raising schemes:
# stageRaising(), topRaising(), externalAux() and raisingScheme().
# A scheme states, for every sampling stage of the hierarchy, how the sampled
# units are raised to their parent unit, and how the top of the hierarchy is
# raised to the totals of the DE stratum.

#' Raising method for one sampling stage
#'
#' Says how the sampled units of one stage (for example hauls, FO) are raised
#' to their parent unit (for example the trip, FT). Used in
#' [raisingScheme()].
#'
#' Almost every method gives each sampled unit the factor
#' \eqn{T_h / \sum_{i \in s_h} v_i}, computed within parent x stratum
#' \eqn{h}: a total \eqn{T} divided by the sum of a value \eqn{v} over the
#' sampled units. The methods are presets of that one formula:
#'
#' | method | total | value | factor |
#' |---|---|---|---|
#' | `"none"` | | | 1 (units pooled) |
#' | `"count"` | `XXnumTotal` | 1 | N / n |
#' | `"aux"` | `XXauxVarTot` | `XXauxVarValue` | aux total / sum of aux |
#' | `"ratio"` | any column | any column or function | total / sum of value |
#' | `"external"` | user table or column | user table or column | total / sum of value |
#' | `"prob"` | | `XXincProb` | 1 / inclusion probability |
#'
#' `XX` is the stage prefix, filled in by [raisingScheme()]. The selection
#' method (`XXselectMeth`) is never used to compute a factor.
#'
#' @param method One of `"none"`, `"count"`, `"aux"`, `"ratio"`,
#'   `"external"` or `"prob"`.
#' @param total Total \eqn{T} per parent x stratum: a column name of the
#'   stage table, or an [externalAux()] object. Defaults depend on `method`.
#' @param value Value \eqn{v} per sampled unit: a column name of the stage
#'   table, `1`, a function that takes the unit table and returns a numeric
#'   vector, or an [externalAux()] object. Defaults depend on `method`.
#' @param strata Column that defines the strata within each parent.
#'   `"design"` (default) uses the design stratum of the stage
#'   (`XXstratumName`). `NULL` means one group per parent.
#'
#' @return An object of class `RDBESStageRaising`.
#' @seealso [raisingScheme()], [topRaising()], [externalAux()]
#' @export
#'
#' @examples
#' stageRaising("count")
#' stageRaising("ratio", total = "FTauxVarTot", value = "FTauxVarValue")
stageRaising <- function(method = c("none", "count", "aux", "ratio",
                                    "external", "prob"),
                         total = NULL, value = NULL, strata = "design") {
  method <- match.arg(method)
  checkRaisingArg(total, "total", allowFunction = FALSE)
  checkRaisingArg(value, "value", allowFunction = TRUE)
  if (!is.null(strata) && !(is.character(strata) && length(strata) == 1)) {
    stop("strata must be \"design\", a column name or NULL.", call. = FALSE)
  }
  if (method == "ratio" && (is.null(total) || is.null(value))) {
    stop("method \"ratio\" needs both total and value.", call. = FALSE)
  }
  if (method == "external") {
    if (is.null(total) || is.null(value)) {
      stop("method \"external\" needs both total and value.", call. = FALSE)
    }
    if (!inherits(total, "RDBESExternalAux") &&
        !inherits(value, "RDBESExternalAux")) {
      stop("method \"external\" needs an externalAux() object for total, ",
           "value or both.", call. = FALSE)
    }
  }
  if (method %in% c("none", "count", "aux", "prob") &&
      (!is.null(total) || !is.null(value))) {
    stop("method \"", method, "\" sets total and value itself. Use method ",
         "\"ratio\" or \"external\" to choose them.", call. = FALSE)
  }
  structure(list(method = method, total = total, value = value,
                 strata = strata),
            class = "RDBESStageRaising")
}

#' Raising method for the top of the hierarchy (DE stratum)
#'
#' Says how the weighted sample is raised to the whole DE stratum.
#' `"none"` takes the weighted sum as the estimate, so every stage must be
#' raised to its population. `"ratio"` multiplies by
#' \eqn{T / \sum w x} per DE stratum, where \eqn{T} is `DEauxVarTot` (or an
#' external total) and \eqn{x} the same quantity measured on the sampled
#' units.
#'
#' @param method `"none"` or `"ratio"`.
#' @param total Total per DE stratum: a DE column (default `"DEauxVarTot"`)
#'   or an [externalAux()] object keyed by `DEstratumName` or `DEid`.
#' @param value The quantity x measured on the sampled units:
#'   `"speciesLiveWeight"` (the live weight of the species in each lowest-level
#'   SA, with `at = "SA"`), a column name of the stage table given by `at`, a
#'   function of that table, or an [externalAux()] object.
#' @param at Stage on which `value` is measured: `"SA"` or a stage of the
#'   hierarchy (`"VS"`, `"FT"` or `"FO"` in hierarchy 1, `"OS"`, `"LE"` or
#'   `"FT"` in hierarchy 5). [raisingScheme()] checks it against the
#'   hierarchy. When it is not `"SA"`, sampled units without the species stay
#'   in the sum of x.
#' @param strata DE column that defines the top strata. Default
#'   `"DEstratumName"`.
#'
#' @return An object of class `RDBESTopRaising`.
#' @seealso [raisingScheme()]
#' @export
#'
#' @examples
#' topRaising("ratio", value = "speciesLiveWeight", at = "SA")
topRaising <- function(method = c("none", "ratio"), total = "DEauxVarTot",
                       value = "speciesLiveWeight", at = "SA",
                       strata = "DEstratumName") {
  method <- match.arg(method)
  checkRaisingArg(total, "total", allowFunction = FALSE)
  checkRaisingArg(value, "value", allowFunction = TRUE)
  if (!(is.character(at) && length(at) == 1 && grepl("^[A-Z]{2}$", at))) {
    stop("at must be \"SA\" or a stage table name such as \"FT\" or \"LE\".",
         call. = FALSE)
  }
  if (identical(value, "speciesLiveWeight") && at != "SA") {
    stop("value = \"speciesLiveWeight\" is measured on SA: use at = \"SA\".",
         call. = FALSE)
  }
  if (!(is.character(strata) && length(strata) == 1)) {
    stop("strata must be a DE column name.", call. = FALSE)
  }
  structure(list(method = method, total = total, value = value, at = at,
                 strata = strata),
            class = "RDBESTopRaising")
}

#' Externally provided auxiliary information
#'
#' Wraps a user table with auxiliary values or totals that are not in the
#' RDBES upload, for example logbook landings per trip, VMS fishing hours per
#' haul or national landings per DE stratum. Use it as `total` or `value` in
#' [stageRaising()] or [topRaising()].
#'
#' @param data A `data.frame` with the key columns and the value column.
#' @param by Key columns. They must also exist in the unit table they are
#'   matched to: RDBES ids such as `"FTid"`, or natural keys such as
#'   `"FTunitName"`, `"VSencrVessCode"` or `"DEstratumName"`. For a total,
#'   the keys identify the parent x stratum.
#' @param col Name of the numeric column in `data` that holds the values.
#'
#' @return An object of class `RDBESExternalAux`.
#' @seealso [stageRaising()], [topRaising()]
#' @export
#'
#' @examples
#' logbook <- data.frame(FTunitName = c("T1", "T2"), landKg = c(5200, 3100))
#' externalAux(logbook, by = "FTunitName", col = "landKg")
externalAux <- function(data, by, col) {
  if (!is.data.frame(data)) stop("data must be a data.frame.", call. = FALSE)
  if (!is.character(by) || length(by) == 0) {
    stop("by must name the key columns.", call. = FALSE)
  }
  if (!(is.character(col) && length(col) == 1)) {
    stop("col must name one column.", call. = FALSE)
  }
  missingCols <- setdiff(c(by, col), names(data))
  if (length(missingCols) > 0) {
    stop("Columns not in data: ", paste(missingCols, collapse = ", "),
         call. = FALSE)
  }
  if (!is.numeric(data[[col]])) {
    stop("Column '", col, "' must be numeric.", call. = FALSE)
  }
  dt <- data.table::as.data.table(data)[, c(by, col), with = FALSE]
  if (anyDuplicated(dt[, by, with = FALSE]) > 0) {
    stop("Duplicate keys in the external table (by = ",
         paste(by, collapse = ", "), ").", call. = FALSE)
  }
  structure(list(data = dt, by = by, col = col), class = "RDBESExternalAux")
}

#' Stage-wise raising scheme
#'
#' Combines the raising method of every sampling stage and of the top into
#' one scheme for [doRatioEstimation()]. Every stage gives each sampled unit
#' a factor. The factors are multiplied along the path, so each lowest-level
#' SA gets one weight `w`. Estimates are \eqn{\sum w y}, optionally followed
#' by the top ratio.
#'
#' Stages not given are `"none"` (pooled). The species selection (SS) is
#' always 1: a species list with more species gets no extra weight.
#'
#' Hierarchies 1 and 5 are implemented. In hierarchy 5 the stages are OS
#' (onshore event) and LE (landing event). FT is an optional stage under LE:
#' name it (`FT = stageRaising(...)`) when the SS records of the data link to
#' FT. If the data use FT and the scheme does not name it, FT is `"none"`.
#'
#' @param ... Stage raising methods, named by stage (`VS`, `FT`, `FO` in
#'   hierarchy 1, `OS`, `LE`, `FT` in hierarchy 5), each made with
#'   [stageRaising()].
#' @param top The top raising, made with [topRaising()].
#' @param hierarchy The RDBES upper hierarchy: `1` (default) or `5`. It must
#'   match `DEhierarchy` in the data.
#'
#' @return An object of class `RDBESRaisingScheme`: a list with `stages` (one
#'   resolved entry per stage, with the actual column names) and `top`.
#' @seealso [stageRaising()], [topRaising()], [externalAux()]
#' @export
#'
#' @examples
#' raisingScheme(FO = stageRaising("count"),
#'               FT = stageRaising("count"),
#'               top = topRaising("ratio", value = "speciesLiveWeight"))
#' # Hierarchy 5: auction days counted, landings raised by landed weight
#' raisingScheme(OS = stageRaising("count"), LE = stageRaising("aux"),
#'               top = topRaising("ratio"), hierarchy = 5)
raisingScheme <- function(..., top = topRaising("none"), hierarchy = 1) {
  spec <- hierarchySpec(hierarchy)
  hierarchy <- spec$hierarchy
  given <- list(...)
  if (length(given) > 0 &&
      (is.null(names(given)) || any(names(given) == ""))) {
    stop("Name every stage, e.g. FO = stageRaising(\"count\").",
         call. = FALSE)
  }
  if ("SS" %in% names(given)) {
    stop("SS is always 1 and cannot be raised.", call. = FALSE)
  }
  stages <- spec$stages
  allowed <- c(stages, names(spec$optional))
  bad <- setdiff(names(given), allowed)
  if (length(bad) > 0) {
    stop("Unknown stage(s): ", paste(bad, collapse = ", "),
         ". Stages in hierarchy ", hierarchy, ": ",
         paste(allowed, collapse = ", "), call. = FALSE)
  }
  # Optional stages are used only when named, placed under their parent
  for (opt in intersect(names(spec$optional), names(given))) {
    stages <- append(stages, opt,
                     after = match(spec$optional[[opt]], stages))
  }
  if (inherits(top, "RDBESTopRaising") && !top$at %in% c("SA", allowed)) {
    stop("top at = \"", top$at, "\" is not a stage of hierarchy ", hierarchy,
         ". Use \"SA\" or one of: ", paste(allowed, collapse = ", "), ".",
         call. = FALSE)
  }
  for (nm in names(given)) {
    if (!inherits(given[[nm]], "RDBESStageRaising")) {
      stop("Stage ", nm, " must be made with stageRaising().", call. = FALSE)
    }
  }
  if (!inherits(top, "RDBESTopRaising")) {
    stop("top must be made with topRaising().", call. = FALSE)
  }

  resolved <- lapply(stages, function(tb) {
    s <- if (tb %in% names(given)) given[[tb]] else stageRaising("none")
    resolveStageRaising(s, tb)
  })
  names(resolved) <- stages

  notRaised <- stages[vapply(resolved, function(s) s$method == "none",
                             logical(1))]
  if (top$method == "none" && length(notRaised) > 0) {
    stop("With top = \"none\" every stage must be raised to its population, ",
         "but these stages are \"none\": ", paste(notRaised, collapse = ", "),
         ". Use \"count\" (it gives 1 when all units were sampled) or a top ",
         "ratio.", call. = FALSE)
  }
  structure(list(stages = resolved, top = top, hierarchy = hierarchy),
            class = "RDBESRaisingScheme")
}

#' Fill in the stage-specific defaults of a stage raising
#' @keywords internal
#' @noRd
resolveStageRaising <- function(s, tb) {
  s$stage <- tb
  if (s$method == "count") {
    s$total <- paste0(tb, "numTotal")
    s$value <- 1
  }
  if (s$method == "aux") {
    s$total <- paste0(tb, "auxVarTot")
    s$value <- paste0(tb, "auxVarValue")
  }
  if (s$method == "prob") s$value <- paste0(tb, "incProb")
  if (identical(s$strata, "design")) s$strata <- paste0(tb, "stratumName")
  s
}

#' Check a total or value argument of a raising constructor
#' @keywords internal
#' @noRd
checkRaisingArg <- function(x, name, allowFunction) {
  if (is.null(x) || inherits(x, "RDBESExternalAux")) return(invisible(TRUE))
  if (is.character(x) && length(x) == 1) return(invisible(TRUE))
  if (identical(name, "value") && is.numeric(x) && length(x) == 1 &&
      x == 1) return(invisible(TRUE))
  if (allowFunction && is.function(x)) return(invisible(TRUE))
  stop(name, " must be a column name",
       if (identical(name, "value")) ", 1" else "",
       if (allowFunction) ", a function" else "",
       " or an externalAux() object.", call. = FALSE)
}

#' Describe a total or value for printing
#' @keywords internal
#' @noRd
describeRaisingArg <- function(x) {
  if (is.null(x)) return("-")
  if (inherits(x, "RDBESExternalAux")) {
    return(paste0("external '", x$col, "' by ",
                  paste(x$by, collapse = " + ")))
  }
  if (is.function(x)) return("function")
  as.character(x)
}

#' Print a stage-wise raising scheme
#'
#' @param x An `RDBESRaisingScheme` from [raisingScheme()].
#' @param ... Not used.
#' @return `x`, invisibly.
#' @export
print.RDBESRaisingScheme <- function(x, ...) {
  cat("RDBES raising scheme (hierarchy ", x$hierarchy, ")\n", sep = "")
  cat("  SS: always 1\n")
  for (s in x$stages) {
    cat("  ", s$stage, ": ", s$method, sep = "")
    if (s$method != "none") {
      if (s$method != "prob") {
        cat(" | total:", describeRaisingArg(s$total))
      }
      cat(" | value:", describeRaisingArg(s$value))
      cat(" | strata:", if (is.null(s$strata)) "none" else s$strata)
    }
    cat("\n")
  }
  tp <- x$top
  cat("  top: ", tp$method, sep = "")
  if (tp$method == "ratio") {
    cat(" | total:", describeRaisingArg(tp$total),
        "| value:", describeRaisingArg(tp$value), "at", tp$at,
        "| strata:", tp$strata)
  }
  cat("\n")
  invisible(x)
}
