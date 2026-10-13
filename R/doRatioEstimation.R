#' Stage-wise estimation of numbers at age or length (hierarchy 1, landings)
#'
#' Estimates numbers, mean weight and mean length at age or at length per
#' domain (for example year x quarter x area x métier). The samples are
#' raised within the sample, then stage by stage (vessels, trips, hauls) with
#' the method chosen for each stage in a [raisingScheme()], and finally to the
#' totals of the DE stratum.
#'
#' This wrapper runs the steps below in order. Every step is exported and
#' can be run on its own to inspect the intermediate tables, which are all
#' returned in the result:
#'
#' 1. [checkRDBESCodes()]: coded fields against the live ICES code lists.
#' 2. [getSampleUnits()]: one row per lowest-level SA with its path, design
#'    classes, domain, raising factor and live weight `x`.
#' 3. [expandWithinSample()]: FM/BV composition expanded to the sampled catch
#'    fraction (lower hierarchies A, B and C).
#' 4. [getStageWeights()]: the factor of every stage and the weight `w` of
#'    each lowest-level SA.
#' 5. [raiseToDE()]: weighted sums per DE stratum and domain, with the top
#'    ratio to the DE total.
#' 6. [summariseRatioEstimates()]: final table with SOP and sample counts.
#'
#' Selection methods are not used to compute weights. Non-probabilistic
#' designs are logged as assumptions.
#'
#' Not in this version: variance, hierarchies other than 1, discards.
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
#' @return An object of class `RDBESRatioEstimate`, a list with
#'   `estimates`, `sampleUnits`, `expanded`, `stageWeights`, `raised`,
#'   `codeCheck`, `log` and `settings`.
#' @seealso [raisingScheme()], [getSampleUnits()], [expandWithinSample()],
#'   [getStageWeights()], [raiseToDE()], [summariseRatioEstimates()]
#' @export
#'
#' @examples
#' \dontrun{
#' scheme <- raisingScheme(FO = stageRaising("count"),
#'                         top = topRaising("ratio"))
#' res <- doRatioEstimation(myH1Object, species = "PLE", raising = scheme,
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
  estimates <- summariseRatioEstimates(raised, plusGroup = plusGroup)

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
  structure(list(estimates = estimates, sampleUnits = sampleUnits,
                 expanded = expanded, stageWeights = stageWeights,
                 raised = raised, codeCheck = codeCheck, log = log,
                 settings = settings),
            class = "RDBESRatioEstimate")
}

#' Print an RDBESRatioEstimate
#'
#' @param x An `RDBESRatioEstimate` from [doRatioEstimation()].
#' @param ... Not used.
#' @return `x`, invisibly.
#' @export
print.RDBESRatioEstimate <- function(x, ...) {
  s <- x$settings
  est <- x$estimates
  dv <- s$domainVars
  nDom <- nrow(unique(est[, dv, with = FALSE]))
  cat("RDBES stage-wise estimate (hierarchy 1)\n")
  cat("  species:", paste(s$species, collapse = ", "),
      "| target:", s$target, "\n")
  print(s$raising)
  cat("  domains estimated:", nDom, "| by", paste(dv, collapse = " x "), "\n")
  if (nDom > 0) {
    sop <- unique(est[, c(dv, "SOP"), with = FALSE])$SOP
    cat("  SOP range:", paste(format(range(sop, na.rm = TRUE), digits = 3),
                              collapse = " - "), "\n")
  }
  lv <- table(x$log$level)
  cat("  log:", paste(names(lv), lv, sep = " = ", collapse = ", "),
      "(see $log)\n")
  if (!is.null(x$codeCheck)) {
    cat("  invalid ICES codes:", x$codeCheck[valid == FALSE, .N], "\n")
  }
  cat("\nFirst rows of $estimates:\n")
  print(utils::head(est, 10))
  invisible(x)
}
