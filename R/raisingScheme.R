# Constructors for stage-wise raising schemes:
# stageRaising(), deRaising(), externalAux() and raisingScheme().
# A scheme states, for every sampling stage of the hierarchy, how the sampled
# units are raised to their parent unit, and how the highest stage is raised
# to the totals of the DE stratum.

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
#' @seealso [raisingScheme()], [deRaising()], [externalAux()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
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

#' Raising within the sample
#'
#' Says how the measured fish of a sample are scaled to the catch fraction of
#' the SA record. Given as `SA = ` in [raisingScheme()]. The factor is per SA
#' record. With sub-samples it is the product along the SA chain.
#'
#' | method | factor per SA record |
#' |---|---|
#' | `"weight"` (default) | `SAtotalWtMes / SAsampWtMes` |
#' | `"count"` | `SAnumTotal / SAnumSamp` |
#' | `"none"` | 1: the measured fish are the sample, not scaled |
#'
#' The auxiliary x of the DE or totals ratio is scaled with the same factor,
#' so x and the numbers at age stay consistent.
#'
#' @param method `"weight"`, `"count"` or `"none"`.
#'
#' @return An object of class `RDBESSampleRaising`.
#' @seealso [raisingScheme()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
#' @export
#'
#' @examples
#' sampleRaising("none")
sampleRaising <- function(method = c("weight", "count", "none")) {
  method <- match.arg(method)
  structure(list(method = method), class = "RDBESSampleRaising")
}

#' Values of x that are measured on the samples themselves
#' @keywords internal
#' @noRd
samplePresets <- function() c("speciesLiveWeight", "fishWeight", "fishCount")

#' Raising to the DE stratum
#'
#' Says how the weighted sample is raised to the whole DE stratum, the
#' sampling frame of the design. Given as `DE = ` in [raisingScheme()].
#' `"none"` takes the weighted sum as the estimate, so every stage must be
#' raised to its population. `"ratio"` multiplies by
#' \eqn{T / \sum w x} per DE stratum, where \eqn{T} is `DEauxVarTot` (or an
#' external total per DE stratum) and \eqn{x} the same quantity measured on
#' the sampled units.
#'
#' @param method `"none"` or `"ratio"`.
#' @param total Total per DE stratum: a DE column (default `"DEauxVarTot"`)
#'   or an [externalAux()] object keyed by `DEstratumName` or `DEid`.
#' @param value The quantity x measured on the sampled units. With
#'   `at = "SA"`:
#'   * `"speciesLiveWeight"` (default): the live weight of the species in the
#'     catch fraction of each lowest-level SA, i.e. the sampled live weight
#'     times the factor of [sampleRaising()],
#'   * `"fishWeight"`: the summed live weight (kg) of the fish counted in the
#'     numbers at age or length, from [expandWithinSample()]. Fish without a
#'     weight get the mean weight of the weighed fish of their sample
#'     (logged),
#'   * `"fishCount"`: the number of fish counted, from [expandWithinSample()],
#'   * an SA column or a function of the sample table.
#'
#'   Otherwise a column name of the stage table given by `at`, a function of
#'   that table, or an [externalAux()] object.
#' @param at Stage on which `value` is measured: `"SA"` or a stage of the
#'   hierarchy (`"VS"`, `"FT"` or `"FO"` in hierarchy 1, `"FT"` or `"FO"` in
#'   hierarchy 2, `"OS"`, `"LE"` or
#'   `"FT"` in hierarchy 5). [raisingScheme()] checks it against the
#'   hierarchy. When it is not `"SA"`, sampled units without the species stay
#'   in the sum of x.
#' @param strata DE column that defines the DE strata. Default
#'   `"DEstratumName"`.
#'
#' @return An object of class `RDBESDERaising`.
#' @seealso [raisingScheme()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
#' @export
#'
#' @examples
#' deRaising("ratio", value = "speciesLiveWeight", at = "SA")
deRaising <- function(method = c("none", "ratio"), total = "DEauxVarTot",
                      value = "speciesLiveWeight", at = "SA",
                      strata = "DEstratumName") {
  method <- match.arg(method)
  checkRaisingArg(total, "total", allowFunction = FALSE)
  checkRaisingArg(value, "value", allowFunction = TRUE)
  if (!(is.character(at) && length(at) == 1 && grepl("^[A-Z]{2}$", at))) {
    stop("at must be \"SA\" or a stage table name such as \"FT\" or \"LE\".",
         call. = FALSE)
  }
  checkSamplePresetAt(value, at)
  if (!(is.character(strata) && length(strata) == 1)) {
    stop("strata must be a DE column name.", call. = FALSE)
  }
  structure(list(method = method, total = total, value = value, at = at,
                 strata = strata),
            class = "RDBESDERaising")
}

#' Raising to totals you supply
#'
#' An optional last step, after the DE step: the estimate is raised to totals
#' from outside the sampling design, for example landings from CL, effort
#' from CE or national statistics. Given as `totals = ` in [raisingScheme()].
#' Per totals stratum \eqn{g} the factor is
#' \deqn{r_g = T_g / \sum_{i \in g} w_i r_{DE,i} x_i}
#' where \eqn{w_i r_{DE,i}} is the weight of unit \eqn{i} after the stages
#' and the DE step, and \eqn{x_i} the same quantity as \eqn{T_g} measured on
#' it. The strata are chosen freely: any column the samples carry.
#'
#' With the same x in the DE step and here, and every totals stratum inside
#' one DE stratum, the DE ratio cancels: the result equals raising to the
#' totals alone. It matters when a totals stratum spans several DE strata.
#'
#' @param total An [externalAux()] table with one row per totals stratum.
#'   Its key columns (`by`) are the strata.
#' @param strata Columns that define the totals strata. Default: the key
#'   columns of `total`. Allowed: the domain variables (`"year"`,
#'   `"quarter"`, `"month"`, `"area"`, `"statRect"`, `"metier6"`, derived as
#'   in [getSampleUnits()]), any SA column (for example `"SAcommCat"`, only
#'   with `at = "SA"`), or any column of a stage table or DE.
#' @param value The quantity x measured on the sampled units, as in
#'   [deRaising()]: `"speciesLiveWeight"` (default), `"fishWeight"`,
#'   `"fishCount"`, or a column or function of the table given by `at`.
#' @param at `"SA"` or a stage of the hierarchy. With a stage, sampled units
#'   without the species stay in the sum of x (needed when x is effort).
#'
#' @return An object of class `RDBESTotalsRaising`.
#' @seealso [raisingScheme()], [deRaising()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
#' @export
#'
#' @examples
#' landings <- data.frame(quarter = 1:4, metier6 = "OTB_DEF_70-99_0_0",
#'                        kg = c(1200, 900, 1500, 1100))
#' totalsRaising(externalAux(landings, by = c("quarter", "metier6"),
#'                           col = "kg"))
totalsRaising <- function(total, strata = NULL, value = "speciesLiveWeight",
                          at = "SA") {
  if (!inherits(total, "RDBESExternalAux")) {
    stop("total must be an externalAux() table with your totals, one row ",
         "per totals stratum.", call. = FALSE)
  }
  if (is.null(strata)) strata <- total$by
  if (!is.character(strata) || length(strata) == 0) {
    stop("strata must name one or more columns.", call. = FALSE)
  }
  if (!setequal(strata, total$by)) {
    stop("strata must be the key columns of the totals table (by = ",
         paste(total$by, collapse = ", "), ").", call. = FALSE)
  }
  checkRaisingArg(value, "value", allowFunction = TRUE)
  if (!(is.character(at) && length(at) == 1 && grepl("^[A-Z]{2}$", at))) {
    stop("at must be \"SA\" or a stage table name such as \"FT\" or \"LE\".",
         call. = FALSE)
  }
  checkSamplePresetAt(value, at)
  saCols <- strata[startsWith(strata, "SA")]
  if (length(saCols) > 0 && at != "SA") {
    stop("SA column(s) ", paste(saCols, collapse = ", "), " as strata need ",
         "at = \"SA\".", call. = FALSE)
  }
  structure(list(method = "ratio", total = total, strata = strata,
                 value = value, at = at),
            class = "RDBESTotalsRaising")
}

#' Externally provided auxiliary information
#'
#' Wraps a user table with auxiliary values or totals that are not in the
#' RDBES upload, for example logbook landings per trip, VMS fishing hours per
#' haul or national landings per DE stratum. Use it as `total` or `value` in
#' [stageRaising()] or [deRaising()].
#'
#' @param data A `data.frame` with the key columns and the value column.
#' @param by Key columns. They must also exist in the unit table they are
#'   matched to: RDBES ids such as `"FTid"`, or natural keys such as
#'   `"FTunitName"`, `"VSencrVessCode"` or `"DEstratumName"`. For a total,
#'   the keys identify the parent x stratum.
#' @param col Name of the numeric column in `data` that holds the values.
#'
#' @return An object of class `RDBESExternalAux`.
#' @seealso [stageRaising()], [deRaising()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
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
#' Combines the raising method of every sampling stage and of the DE step into
#' one scheme for [doRatioEstimation()]. Every stage gives each sampled unit
#' a factor. The factors are multiplied along the path, so each lowest-level
#' SA gets one weight `wStages`. Estimates are \eqn{\sum w y}, optionally followed
#' by the DE ratio.
#'
#' Stages not given are `"none"` (pooled). The species selection (SS) is
#' always 1: a species list with more species gets no extra weight.
#'
#' All hierarchies except 6 and 11 are implemented. The stages, from the top:
#' * hierarchy 1: VS, FT, FO
#' * hierarchy 2: FT, FO (trips sampled directly)
#' * hierarchy 3: TE (temporal event), VS, FT, FO
#' * hierarchy 4: OS (onshore event), FT, LE (landing event)
#' * hierarchy 5: OS, LE, and FT under LE as an optional stage: name it
#'   (`FT = stageRaising(...)`) when the SS records of the data link to FT.
#'   If the data use FT and the scheme does not name it, FT is `"none"`.
#' * hierarchy 7: OS
#' * hierarchy 8: TE, VS, LE
#' * hierarchy 9: LO (location), TE
#' * hierarchy 10: VS, TE, FT, FO
#' * hierarchy 12: LO, TE, LE
#' * hierarchy 13: FO (hauls sampled directly)
#'
#' @param ... Stage raising methods, named by stage (see Details), each made
#'   with
#'   [stageRaising()].
#' @param SA The raising within the sample, made with [sampleRaising()].
#'   Default `sampleRaising("weight")`.
#' @param DE The raising to the DE stratum, made with [deRaising()].
#' @param totals Optional raising to totals you supply, made with
#'   [totalsRaising()]. Applied after the DE step. Default `NULL` (none).
#' @param hierarchy The RDBES upper hierarchy: `1` (default), `2` or `5`. It must
#'   match `DEhierarchy` in the data.
#'
#' @details
#' When the DE ratio or the totals ratio measures x on a stage (`at`), every
#' stage below it must be raised. Otherwise x covers whole units while the
#' numbers cover only their sampled part.
#'
#' @return An object of class `RDBESRaisingScheme`: a list with `SA`,
#'   `stages` (one resolved entry per stage, with the actual column names),
#'   `DE` and `totals`.
#' @seealso [stageRaising()], [deRaising()], [externalAux()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
#' @export
#'
#' @examples
#' raisingScheme(FO = stageRaising("count"),
#'               FT = stageRaising("count"),
#'               DE = deRaising("ratio", value = "speciesLiveWeight"))
#' # Hierarchy 5: auction days counted, landings raised by landed weight
#' raisingScheme(OS = stageRaising("count"), LE = stageRaising("aux"),
#'               DE = deRaising("ratio"), hierarchy = 5)
raisingScheme <- function(..., SA = sampleRaising("weight"),
                          DE = deRaising("none"), totals = NULL,
                          hierarchy = 1) {
  if (!inherits(SA, "RDBESSampleRaising")) {
    stop("SA must be made with sampleRaising().", call. = FALSE)
  }
  if (!is.null(totals) && !inherits(totals, "RDBESTotalsRaising")) {
    stop("totals must be made with totalsRaising().", call. = FALSE)
  }
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
  if (inherits(DE, "RDBESDERaising") && !DE$at %in% c("SA", allowed)) {
    stop("DE at = \"", DE$at, "\" is not a stage of hierarchy ", hierarchy,
         ". Use \"SA\" or one of: ", paste(allowed, collapse = ", "), ".",
         call. = FALSE)
  }
  if (!is.null(totals) && !totals$at %in% c("SA", allowed)) {
    stop("totals at = \"", totals$at, "\" is not a stage of hierarchy ",
         hierarchy, ". Use \"SA\" or one of: ",
         paste(allowed, collapse = ", "), ".", call. = FALSE)
  }
  for (nm in names(given)) {
    if (!inherits(given[[nm]], "RDBESStageRaising")) {
      stop("Stage ", nm, " must be made with stageRaising().", call. = FALSE)
    }
  }
  if (!inherits(DE, "RDBESDERaising")) {
    stop("DE must be made with deRaising().", call. = FALSE)
  }

  resolved <- lapply(stages, function(tb) {
    s <- if (tb %in% names(given)) given[[tb]] else stageRaising("none")
    resolveStageRaising(s, tb)
  })
  names(resolved) <- stages

  notRaised <- stages[vapply(resolved, function(s) s$method == "none",
                             logical(1))]
  if (DE$method == "none" && is.null(totals) && length(notRaised) > 0) {
    stop("With DE = \"none\" and no totals every stage must be raised to its ",
         "population, but these stages are \"none\": ",
         paste(notRaised, collapse = ", "), ". Use \"count\" (it gives 1 ",
         "when all units were sampled), a DE ratio or totals.", call. = FALSE)
  }
  for (step in list(list(name = "DE", s = DE), list(name = "totals",
                                                     s = totals))) {
    if (is.null(step$s) || step$s$method == "none") next
    checkStagesBelowRaised(step$s$at, stages, notRaised, step$name)
  }
  structure(list(SA = SA, stages = resolved, DE = DE, totals = totals,
                 hierarchy = hierarchy),
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

#' Check that every stage below a stage-level x is raised
#'
#' A ratio with x measured on stage `at` compares whole units (x) with the
#' sampled part of them (y) when a stage below `at` is "none".
#' @keywords internal
#' @noRd
checkStagesBelowRaised <- function(at, stages, notRaised, name) {
  if (identical(at, "SA") || !at %in% stages) return(invisible(TRUE))
  below <- stages[seq_along(stages) > match(at, stages)]
  bad <- intersect(below, notRaised)
  if (length(bad) > 0) {
    stop(name, " ratio with x measured on ", at, ": the stage(s) below it ",
         "must be raised, but ", paste(bad, collapse = ", "), " is \"none\". ",
         "Otherwise x covers whole ", at, " units while the numbers cover ",
         "only their sampled part. Use \"count\" (1 when all units were ",
         "sampled) or measure x on SA.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Check that a value measured on the samples is used with at = "SA"
#' @keywords internal
#' @noRd
checkSamplePresetAt <- function(value, at) {
  if (is.character(value) && length(value) == 1 &&
      value %in% samplePresets() && at != "SA") {
    stop("value = \"", value, "\" is measured on the samples: use at = \"SA\".",
         call. = FALSE)
  }
  invisible(TRUE)
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
  saMethod <- if (is.null(x$SA)) "weight" else x$SA$method
  cat("  SA (within sample): ", saMethod, "\n", sep = "")
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
  tp <- x$DE
  cat("  DE: ", tp$method, sep = "")
  if (tp$method == "ratio") {
    cat(" | total:", describeRaisingArg(tp$total),
        "| value:", describeRaisingArg(tp$value), "at", tp$at,
        "| strata:", tp$strata)
  }
  cat("\n")
  tt <- x$totals
  if (!is.null(tt)) {
    cat("  totals: ratio | total:", describeRaisingArg(tt$total),
        "| value:", describeRaisingArg(tt$value), "at", tt$at,
        "| strata:", paste(tt$strata, collapse = " x "), "\n")
  }
  invisible(x)
}
