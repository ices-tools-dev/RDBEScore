#' Stage-wise estimation of numbers at age or length (landings)
#'
#' Estimates numbers, mean weight and mean length at age or at length per
#' domain (for example year x quarter x area x métier). The samples are
#' raised within the sample, then stage by stage (vessels, trips, hauls in
#' hierarchy 1, trips and hauls in hierarchy 2, onshore events, landing events
#' and optionally trips in
#' hierarchy 5) with the method chosen for each stage in a
#' [raisingScheme()], and finally to the totals of the DE stratum.
#'
#' This wrapper runs the steps below in order. Every step is exported and
#' can be run on its own to inspect the intermediate tables, which are all
#' returned in the result:
#'
#' 1. [checkRDBESCodes()]: coded fields against the live ICES code lists.
#' 2. [getSampleUnits()]: one row per lowest-level SA with its path, design
#'    classes, domain, within-sample factor `fSample` and live weight `xSample`.
#' 3. [expandWithinSample()]: FM/BV composition expanded to the sampled catch
#'    fraction (lower hierarchies A, B and C).
#' 4. [getStageWeights()]: the factor of every stage and the weight `wStages` of
#'    each lowest-level SA.
#' 5. [raiseToDE()]: weighted sums per DE stratum and domain, with the DE
#'    ratio (optional).
#' 6. [raiseToTotals()]: only when the scheme has `totals`. The ratio to
#'    your own totals, and the sums per domain again.
#' 7. [summariseRatioEstimates()] and [summariseRatioDomains()]: the
#'    estimates per domain and class, and per domain (SOP, sample counts).
#'
#' Selection methods are not used to compute weights. Non-probabilistic
#' designs are logged as assumptions.
#'
#' Implemented for upper hierarchies 1, 2, 3, 5, 7, 8, 10 and 13. The scheme
#' must be made for the hierarchy of the data
#' (`raisingScheme(..., hierarchy = 5)`).
#'
#' Not in this version: variance, hierarchies 4, 6, 9, 11 and 12,
#' discards.
#'
#' @inheritParams getSampleUnits
#' @inheritParams expandWithinSample
#' @param raising A stage-wise raising scheme from [raisingScheme()].
#' @param domainVars Domain variables: any of `"year"`, `"quarter"`,
#'   `"month"`, `"area"`, `"statRect"` and `"metier6"`. Default
#'   `c("year", "quarter", "area", "metier6")`.
#' @param plusGroup Optional plus group age.
#' @param checkCodes Logical. Run [checkRDBESCodes()] (needs internet access
#'   to vocab.ices.dk). Default `TRUE`.
#'
#' @return An object of class `RDBESRatioEstimate`, a list. Each element is
#'   named by what one row of it is:
#'   * `estimates`: one row per domain and age (or length class), from
#'     [summariseRatioEstimates()]. The result.
#'   * `domains`: one row per domain, from [summariseRatioDomains()]:
#'     estimated live weight, the factor applied, SOP and sample counts.
#'   * `ratios`: a list with `DE` (one row per DE stratum) and `totals` (one
#'     row per totals stratum, `NULL` without totals), from [raiseToDE()] and
#'     [raiseToTotals()].
#'   * `samples`: one row per lowest-level SA, from [getStageWeights()]:
#'     ids, domain, inclusion, the factors and `wStages` first, the design
#'     columns copied from the data last.
#'   * `stageUnits`: a list with one table per stage, one row per sampled
#'     unit with its factor, from [getStageWeights()].
#'   * `fishBySample`: one row per sample and class, the fish of each sample
#'     scaled to its catch fraction, from [expandWithinSample()].
#'   * `byStratum`: one row per domain, DE stratum and class, the raised
#'     fish before the strata are summed, from [raiseToDE()]. With totals one
#'     row per domain and class, from [raiseToTotals()].
#'   * `codeCheck`, `log` and `settings`.
#'
#'   [ratioOutputGuide()] explains every column.
#' @seealso [raisingScheme()], [getSampleUnits()], [expandWithinSample()],
#'   [getStageWeights()], [raiseToDE()], [raiseToTotals()],
#'   [summariseRatioEstimates()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
#' @export
#'
#' @examples
#' \dontrun{
#' scheme <- raisingScheme(FO = stageRaising("count"),
#'                         DE = deRaising("ratio"))
#' res <- doRatioEstimation(myH1Object, species = 127143, raising = scheme,
#'                          target = "Age", plusGroup = 10)
#' res
#' res$estimates
#' res$log
#' }
doRatioEstimation <- function(RDBESDataObj,
                              species,
                              raising,
                              target = c("Age", "Length"),
                              domainVars = c("year", "quarter", "area",
                                             "metier6"),
                              catchCat = "Lan",
                              areaLookup = NULL,
                              lengthType = "LengthTotal",
                              classWidth = NULL,
                              ageRoute = c("sample", "sampleFillPooled"),
                              externalALK = NULL,
                              gutToLive = NULL,
                              useWeights = TRUE,
                              plusGroup = NULL,
                              checkCodes = TRUE,
                              strict = TRUE,
                              verbose = FALSE) {
  if (missing(raising) || !inherits(raising, "RDBESRaisingScheme")) {
    stop("raising must be a scheme made with raisingScheme().",
         call. = FALSE)
  }
  target <- match.arg(target)
  ageRoute <- match.arg(ageRoute)
  checkDomainVars(domainVars)
  log <- newRatioLog()

  codeCheck <- NULL
  if (checkCodes) {
    codeCheck <- checkRDBESCodes(RDBESDataObj, strict = strict)
    nBad <- codeCheck[valid == FALSE, sum(n)]
    if (nBad > 0) {
      bad <- codeCheck[valid == FALSE,
                       paste0(field, "='", value, "'", collapse = ", ")]
      warning("Values not in the ICES code lists: ", bad,
              ". See $codeCheck.", call. = FALSE)
      log <- addToRatioLog(log, "checkRDBESCodes", "warning",
        paste0("Values not in the ICES code lists: ", bad), nBad, verbose)
    }
  }

  sampleUnits <- getSampleUnits(RDBESDataObj, species = species,
                                catchCat = catchCat,
                                areaLookup = areaLookup, strict = strict,
                                withinSample = if (is.null(raising$SA)) {
                                  sampleRaising("weight")
                                } else {
                                  raising$SA
                                },
                                verbose = verbose)
  expanded <- expandWithinSample(RDBESDataObj, sampleUnits, target = target,
                                 lengthType = lengthType,
                                 classWidth = classWidth, ageRoute = ageRoute,
                                 externalALK = externalALK,
                                 gutToLive = gutToLive,
                                 useWeights = useWeights, verbose = verbose)
  stageWeights <- getStageWeights(RDBESDataObj, sampleUnits, raising,
                                  verbose = verbose)
  raised <- raiseToDE(expanded, stageWeights, RDBESDataObj, scheme = raising,
                      domainVars = domainVars, verbose = verbose)
  if (!is.null(raising$totals)) {
    raised <- raiseToTotals(raised, expanded, stageWeights, RDBESDataObj,
                            scheme = raising, verbose = verbose)
  }
  estimates <- summariseRatioEstimates(raised, plusGroup = plusGroup)
  domains <- summariseRatioDomains(raised)

  log <- data.table::rbindlist(list(log, getRatioLog(sampleUnits),
                                    getRatioLog(expanded),
                                    getRatioLog(stageWeights),
                                    getRatioLog(raised)))
  settings <- list(species = species, target = target, raising = raising,
                   domainVars = domainVars, catchCat = catchCat,
                   lengthType = lengthType,
                   classWidth = attr(expanded, "classWidth"),
                   ageRoute = ageRoute, gutToLive = gutToLive,
                   plusGroup = plusGroup)
  # Every table as a list element, named by what one row is. The step
  # outputs keep their attributes, the result does not need them.
  structure(list(estimates = estimates,
                 domains = domains,
                 ratios = list(DE = attr(raised, "deRatios"),
                               totals = attr(raised, "totalsRatios")),
                 samples = sampleColumnsFirst(stripAttributes(stageWeights)),
                 stageUnits = attr(stageWeights, "unitWeights"),
                 fishBySample = stripAttributes(expanded),
                 byStratum = stripAttributes(raised),
                 codeCheck = codeCheck, log = log, settings = settings),
            class = "RDBESRatioEstimate")
}

#' Drop the attributes a step adds (log, settings) from a data.table
#' @keywords internal
#' @noRd
stripAttributes <- function(d) {
  d <- data.table::copy(d)
  keep <- c("names", "row.names", "class", ".internal.selfref")
  for (a in setdiff(names(attributes(d)), keep)) data.table::setattr(d, a, NULL)
  d
}

#' Put the key columns of the sample table first
#'
#' Ids, DE, domain, inclusion, factors and x first, the design columns
#' copied from the data (selection method, stratum, numTotal, ...) last.
#' @keywords internal
#' @noRd
sampleColumnsFirst <- function(d) {
  ids <- c("SAid", "topSAid", "SAlevel",
           grep("^[A-Z]{2}id$", names(d), value = TRUE))
  lead <- c(ids, "DEid", "DEstratumName",
            "year", "quarter", "month", "area", "statRect", "metier6",
            "include", "excludeReason", "fSample", "xSample",
            grep("^f_[A-Z]{2}$", names(d), value = TRUE), "wStages")
  lead <- unique(intersect(lead, names(d)))
  data.table::setcolorder(d, c(lead, setdiff(names(d), lead)))
  d
}

#' Print an RDBESRatioEstimate
#'
#' @param x An `RDBESRatioEstimate` from [doRatioEstimation()].
#' @param ... Not used.
#' @return `x`, invisibly.
#' @export
print.RDBESRatioEstimate <- function(x, ...) {
  s <- x$settings
  dv <- s$domainVars
  classLabel <- if (identical(s$target, "Age")) "age" else "length class"
  cat("RDBES stage-wise estimate (hierarchy ", s$raising$hierarchy, ")\n",
      sep = "")
  cat("  species:", paste(s$species, collapse = ", "),
      "| target:", s$target, "\n")
  print(s$raising)
  nDom <- nrow(x$domains)
  cat("  domains estimated:", nDom, "| by", paste(dv, collapse = " x "), "\n")
  if (nDom > 0) {
    cat("  SOP range:", paste(format(range(x$domains$SOP, na.rm = TRUE),
                                     digits = 3), collapse = " - "), "\n")
  }
  lv <- table(x$log$level)
  cat("  log:", paste(names(lv), lv, sep = " = ", collapse = ", "),
      "(see $log)\n")
  if (!is.null(x$codeCheck)) {
    cat("  invalid ICES codes:", x$codeCheck[valid == FALSE, .N], "\n")
  }
  # What each element holds, one row of it, and how many rows
  nr <- function(d) if (is.null(d)) "-" else nrow(d)
  cat("\nElements (one row per ...):\n")
  rows <- list(
    c("estimates", paste("domain x", classLabel), nr(x$estimates)),
    c("domains", "domain", nr(x$domains)),
    c("ratios$DE", "DE stratum", nr(x$ratios$DE)),
    c("ratios$totals", "totals stratum", nr(x$ratios$totals)),
    c("samples", "lowest-level sample (SA)", nr(x$samples)),
    c("stageUnits", paste0("sampled unit, one table per stage (",
                           paste(names(x$stageUnits), collapse = ", "), ")"),
      ""),
    c("fishBySample", paste("sample x", classLabel), nr(x$fishBySample)),
    c("byStratum", paste("domain x DE stratum x", classLabel),
      nr(x$byStratum)),
    c("log", "log entry", nr(x$log)))
  for (r in rows) {
    cat(sprintf("  $%-14s %-48s %s\n", r[1], r[2],
                if (r[3] == "") "" else paste(r[3], "rows")))
  }
  cat("  What each column means: ratioOutputGuide()\n")
  cat("\nFirst rows of $estimates:\n")
  print(utils::head(x$estimates, 10))
  invisible(x)
}
