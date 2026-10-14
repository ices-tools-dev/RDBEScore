#' Raise the weighted samples to the DE stratum
#'
#' Last raising step of stage-wise raising (see [raisingScheme()]). Each
#' expanded sample from [expandWithinSample()] is multiplied by its stage
#' weight `w` from [getStageWeights()], and summed per DE stratum, domain and
#' class:
#' \deqn{\hat N_{d,a} = r_h \sum_{i \in d} w_i y_{i,a}}
#' With DE `"none"`, \eqn{r_h = 1}. With DE `"ratio"`,
#' \eqn{r_h = T_h / \sum_{j \in h} w_j x_j} per DE stratum \eqn{h}, where
#' \eqn{T_h} is `DEauxVarTot` (or an external total) and \eqn{x_j} the
#' auxiliary value of the sampled units of the stage given by `at`, weighted
#' by their cumulative stage weight. When `at` is not `"SA"`, sampled units
#' without the species stay in the sum.
#'
#' The ratio is formed per DE stratum and shared by all domains (quarter,
#' area, métier, ...) of that stratum.
#'
#' `X_kg` in the result is the **estimated** live weight of the species per
#' domain, \eqn{r_h \sum w x} with x the sampled live weight. The SOP check in
#' [summariseRatioEstimates()] then compares the raised fish weights with the
#' raised sample weights.
#'
#' @param expanded Output of [expandWithinSample()].
#' @param stageWeights Output of [getStageWeights()].
#' @param RDBESDataObj The `RDBESDataObject` (for DE totals and the unit
#'   tables of `at`).
#' @param scheme The scheme from [raisingScheme()]. Default: the one stored in
#'   `stageWeights`.
#' @param domainVars Domain variables. Default
#'   `c("year", "quarter", "area", "metier6")`.
#' @param verbose Logical. Print log entries as messages. Default `FALSE`.
#'
#' @return A `data.table` with the domain
#'   variables, `stratum` (the DE stratum), `class`, raised `N`, `NW`, `W`,
#'   `NL`, `L`, `nFish`, and `sumX` (weighted sample live weight, kg), `X`
#'   (estimated live weight, kg) and `ratio` (DE ratio, 1 for `"none"`).
#'   Attributes: `"deRatios"`, `"domainSummary"`, `"target"`,
#'   `"domainVars"` and `"ratioLog"`.
#' @seealso [getStageWeights()], [summariseRatioEstimates()]
#' @export
raiseToDE <- function(expanded, stageWeights, RDBESDataObj,
                      scheme = attr(stageWeights, "scheme"),
                      domainVars = c("year", "quarter", "area", "metier6"),
                      verbose = FALSE) {
  if (!inherits(scheme, "RDBESRaisingScheme")) {
    stop("scheme must be made with raisingScheme().", call. = FALSE)
  }
  checkDomainVars(domainVars)
  step <- "raiseToDE"
  log <- newRatioLog()
  deStep <- scheme$DE
  stratCol <- deStep$strata

  su <- data.table::copy(stageWeights[include == TRUE &
                                        SAid %in% unique(expanded$SAid)])
  if (nrow(su) == 0) stop("No included samples with expanded data.")
  for (v in domainVars) {
    if (v %in% c("year", "quarter", "month")) su[, (v) := as.integer(get(v))]
    else su[, (v) := as.character(get(v))]
  }
  de <- data.table::copy(RDBESDataObj$DE)
  if (!stratCol %in% names(de)) {
    stop("DE strata column '", stratCol, "' not in DE.", call. = FALSE)
  }
  # One stratum per DE record. The label is the strata column, with the
  # DEid added when a label is used by more than one DE record (for example
  # several years or sampling schemes in one object).
  labs <- as.character(de[[stratCol]])
  dupLab <- labs %in% labs[duplicated(labs)]
  labs[dupLab] <- paste0(labs[dupLab], " [DEid ", de$DEid[dupLab], "]")
  deLab <- data.table::data.table(DEid = de$DEid, .stratum = labs)
  su <- merge(su, deLab, by = "DEid", all.x = TRUE)

  # ---------------------------------------------------------------------------
  # DE ratio per DE stratum
  # ---------------------------------------------------------------------------
  if (deStep$method == "none") {
    ratios <- data.table::data.table(.stratum = unique(su$.stratum),
                                     ratio = 1)
    log <- addToRatioLog(log, step, "info",
      "DE 'none': estimates are the weighted sums, no DE ratio", NA,
      verbose)
  } else {
    if (deStep$at == "SA") {
      xs <- sampleX(su, expanded, deStep$value, step, verbose)
      log <- rbind(log, xs$log)
      xu <- su[, .(.id = SAid, DEid, wUnit = w)]
      xu[, x := xs$x[match(xu$.id, xs$SAid)]]
    } else {
      uw <- attr(stageWeights, "unitWeights")[[deStep$at]]
      if (is.null(uw)) {
        stop("No unit weights for stage ", deStep$at, ".", call. = FALSE)
      }
      idCol <- paste0(deStep$at, "id")
      units <- getUnitTable(RDBESDataObj, deStep$at,
                            fields = stageFieldsNeeded(
                              RDBESDataObj, deStep$at,
                              list(value = deStep$value, total = NULL,
                                   strata = NULL)),
                            verbose = verbose)
      r <- resolveRaisingArg(units, deStep$value, ".x", step, verbose)
      log <- rbind(log, r$log)
      xu <- merge(uw[, c(idCol, "DEid", "wUnit"), with = FALSE],
                  r$units[, c(idCol, ".x"), with = FALSE], by = idCol)
      data.table::setnames(xu, c(idCol, ".x"), c(".id", "x"))
    }
    xu[, .v := wUnit * x]

    r <- resolveRaisingArg(de, deStep$total, ".total", step, verbose)
    log <- rbind(log, r$log)
    deT <- merge(deLab, r$units[, .(DEid, .total)], by = "DEid")
    noSample <- deT[!is.na(.total) & .total > 0 & !DEid %in% xu$DEid]
    if (nrow(noSample) > 0) {
      stop("DE strata with a total but no sampled units: ",
           paste(noSample$.stratum, collapse = ", "), call. = FALSE)
    }
    xu <- merge(xu, deT, by = "DEid", all.x = TRUE)
    f <- computeStageFactors(xu, id = ".id", by = ".stratum",
                             total = ".total", value = ".v",
                             step = paste0(step, " (DE)"), verbose = verbose)
    log <- rbind(log, getRatioLog(f))
    ratios <- unique(f[, .(.stratum, ratio = factor, T = total,
                           sumWX = sumValue)])
    log <- addToRatioLog(log, step, "info",
      paste0("DE ratio per ", stratCol, " with x measured on ", deStep$at),
      nrow(ratios), verbose)
  }

  # ---------------------------------------------------------------------------
  # Weighted sums per DE stratum x domain x class
  # ---------------------------------------------------------------------------
  byS <- c(domainVars, ".stratum")
  ys <- merge(expanded, su[, c("SAid", "w", byS), with = FALSE],
              by = "SAid")[
    , .(yN = sum(w * yN), yNW = sum(w * yNW), yW = sum(w * yW),
        yNL = sum(w * yNL), yL = sum(w * yL), nFish = sum(nFish)),
    by = c(byS, "class")]
  xs <- su[, .(sumX = sum(w * x)), by = byS]
  out <- merge(merge(ys, xs, by = byS), ratios[, .(.stratum, ratio)],
               by = ".stratum")
  out[, `:=`(N = yN * ratio, NW = yNW * ratio, W = yW * ratio,
             NL = yNL * ratio, L = yL * ratio, X = sumX * ratio)]
  data.table::setnames(out, ".stratum", "stratum")
  out <- out[, c(domainVars, "stratum", "class", "N", "NW", "W", "NL", "L",
                 "nFish", "sumX", "X", "ratio"), with = FALSE]
  data.table::setorderv(out, c(domainVars, "stratum", "class"))

  # Domain summary
  # Number of sampled units per stage, e.g. nVessels or nLandingEvents
  sc <- ratioStageChain(RDBESDataObj)
  labels <- sc$spec$unitLabels[sc$stages]
  idCols <- paste0(sc$stages, "id")
  keepIds <- idCols %in% names(su)
  idCols <- idCols[keepIds]
  labels <- labels[keepIds]
  suR <- merge(su, ratios[, .(.stratum, ratio)], by = ".stratum")
  domSum <- suR[, c(list(nSamples = .N, sumX = sum(w * x),
                         X = sum(w * x * ratio)),
                    lapply(.SD, data.table::uniqueN)),
                by = domainVars, .SDcols = idCols]
  data.table::setnames(domSum, idCols, unname(labels))

  log <- addToRatioLog(log, step, "info", "Domains estimated",
                       nrow(domSum), verbose)
  deRatios <- data.table::copy(ratios)
  data.table::setnames(deRatios, ".stratum", "stratum")
  data.table::setkey(deRatios, NULL)
  attr(out, "deRatios") <- deRatios[]
  attr(out, "domainSummary") <- domSum
  attr(out, "target") <- attr(expanded, "target")
  attr(out, "domainVars") <- domainVars
  attr(out, "ratioLog") <- log
  out[]
}

#' x per sample for a ratio measured on the samples (at = "SA")
#'
#' `"speciesLiveWeight"` is `x` from [getSampleUnits()]. `"fishWeight"` and
#' `"fishCount"` come from the expanded fish, so they carry the same
#' within-sample factor as the numbers at age. Any other value is an SA
#' column or a function of the sample table.
#' @return A list with `SAid`, `x` and `log`.
#' @keywords internal
#' @noRd
sampleX <- function(su, expanded, value, step, verbose) {
  log <- newRatioLog()
  if (identical(value, "speciesLiveWeight")) {
    out <- su[, .(SAid, x)]
    noX <- out[is.na(x), SAid]
    if (length(noX) > 0) {
      stop(step, ": the live weight of the species is missing for ",
           length(noX), " sample(s) (SAid ", paste(utils::head(noX, 10),
                                                   collapse = ", "),
           "). With sampleRaising(\"count\") or (\"none\") it needs ",
           "SAsampWtMes. Fill it or use value = \"fishWeight\".",
           call. = FALSE)
    }
    return(list(SAid = out$SAid, x = out$x, log = log))
  }
  if (identical(value, "fishWeight") || identical(value, "fishCount")) {
    e <- expanded[SAid %in% su$SAid,
                  .(n = sum(yN), nW = sum(yNW), sW = sum(yW)), by = SAid]
    if (identical(value, "fishCount")) {
      return(list(SAid = e$SAid, x = e$n, log = log))
    }
    noW <- e[nW <= 0, SAid]
    if (length(noW) > 0) {
      stop(step, ": value = \"fishWeight\" but ", length(noW),
           " sample(s) have no weighed fish (SAid ",
           paste(utils::head(noW, 10), collapse = ", "), ").",
           call. = FALSE)
    }
    part <- e[nW < n - 1e-9, .N]
    if (part > 0) {
      log <- addToRatioLog(log, step, "assumption",
        paste0("fishWeight: fish without a weight get the mean weight of the ",
               "weighed fish of their sample"), part, verbose)
    }
    # g to kg, scaled up for the fish without a weight
    return(list(SAid = e$SAid, x = e$sW * e$n / e$nW / 1000, log = log))
  }
  r <- resolveRaisingArg(su, value, ".x", step, verbose)
  list(SAid = r$units$SAid, x = r$units$.x, log = r$log)
}
