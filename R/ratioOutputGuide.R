#' What every column of a stage-wise ratio estimate means
#'
#' A table that explains the output of [doRatioEstimation()]: for every
#' element of the result and every column in it, what one row of the element
#' is, the unit and the meaning.
#'
#' Naming rules of the columns:
#' * multipliers start with `f` (one step, for example `fSample`, `fStage`,
#'   `f_FO`), `w` (a product of steps, `wStages`, `wCumulative`) or `r` (a
#'   ratio to a known total, `rDE`, `rTotals`, `rApplied`),
#' * the auxiliary variable is `x` (`xSample`, `xWeightedStratum`), the live
#'   weight of the species is `liveWeight...`,
#' * `y` in front means one sample scaled to its catch fraction (`yN`), no
#'   `y` means raised to the population (`N`),
#' * `n` in front means a count of sampled things, never raised (`nFish`,
#'   `nHauls`),
#' * fixed units are in the name (`_g`, `_kg`, `_mm`).
#'
#' In the column names, `XX` stands for a stage table prefix (for example
#' `FO` in `FOselectMeth`). Domain variables are the ones chosen in
#' `domainVars`.
#'
#' @param element Optional element name(s) to keep, for example
#'   `"samples"` or `"ratios$DE"`. Default `NULL` returns all.
#'
#' @return A `data.table` with columns `element`, `column`, `rowUnit` (what
#'   one row of the element is), `unit` and `meaning`.
#' @seealso [doRatioEstimation()]
#' @export
#'
#' @examples
#' ratioOutputGuide("domains")
#' ratioOutputGuide()[column == "wStages"]
ratioOutputGuide <- function(element = NULL) {
  g <- ratioGuideTable()
  if (!is.null(element)) {
    wanted <- element
    bad <- setdiff(wanted, unique(g$element))
    if (length(bad) > 0) {
      stop("Unknown element(s): ", paste(bad, collapse = ", "),
           ". Elements: ", paste(unique(g$element), collapse = ", "),
           call. = FALSE)
    }
    g <- g[g$element %in% wanted]
  }
  g[]
}

#' The rows of ratioOutputGuide()
#' @keywords internal
#' @noRd
ratioGuideTable <- function() {
  row <- function(column, unit, meaning) {
    data.table::data.table(column = column, unit = unit, meaning = meaning)
  }
  domain <- data.table::rbindlist(list(
    row("year", "", "Domain: year of the sample date"),
    row("quarter", "", "Domain: quarter of the sample date"),
    row("month", "", "Domain: month of the sample date"),
    row("area", "", "Domain: area (SA, else FO in H1 and H2, or LE in H5)"),
    row("statRect", "", "Domain: ICES rectangle (SA, else FO or LE)"),
    row("metier6", "", "Domain: metier level 6 (SA, else FO or LE)")))
  fish <- data.table::rbindlist(list(
    row("nFish", "fish", "Fish measured or aged in the class. Never raised")))
  counts <- data.table::rbindlist(list(
    row("nSamples", "samples", "Lowest-level samples used in the domain"),
    row("nVessels", "units", "Sampled vessels in the domain (H1)"),
    row("nTrips", "units", "Sampled trips in the domain (H1, H2, or H5 with FT)"),
    row("nHauls", "units", "Sampled hauls in the domain (H1, H2)"),
    row("nOnshoreEvents", "units", "Sampled onshore events in the domain (H5)"),
    row("nLandingEvents", "units",
        "Sampled landing events in the domain (H5)")))
  liveWeight <- data.table::rbindlist(list(
    row("liveWeightSampled_kg", "kg",
        paste("Live weight of the species in the samples, times wStages.",
              "Before the DE and totals ratios")),
    row("liveWeightEst_kg", "kg",
        paste("Estimated live weight of the species: liveWeightSampled_kg",
              "times the ratios")),
    row("rApplied", "factor",
        paste("Factor applied by the DE and totals ratios:",
              "liveWeightEst_kg / liveWeightSampled_kg"))))

  parts <- list(
    estimates = list("domain x age (or length class)", data.table::rbindlist(
      list(domain,
           row("age", "years", "Age class"),
           row("lengthClass_mm", "mm", "Length class, lower bound"),
           row("N", "fish", "Estimated number of fish in the class"),
           row("meanWeight_g", "g", "Mean live weight, W_g / NW"),
           row("meanLength_mm", "mm", "Mean length, L_mm / NL"),
           row("totalWeight_kg", "kg",
               "Estimated weight of the fish in the class, N x meanWeight_g"),
           fish))),
    domains = list("domain", data.table::rbindlist(list(
      domain, liveWeight,
      row("SOP", "ratio",
          paste("Sum of products: sum of totalWeight_kg / liveWeightEst_kg.",
                "Fish weights against sample weights. Should be close to 1")),
      row("weightCoverage", "share",
          "Share of N that had an individual weight, sum NW / sum N"),
      counts))),
    "ratios$DE" = list("DE stratum", data.table::rbindlist(list(
      row("DEstratumName", "", "The DE stratum (with the DEid when a name is used twice)"),
      row("rDE", "factor", "DE ratio: total / xWeightedStratum. 1 for DE none"),
      row("total", "as DEauxVarUnit", "The known DE total, by default DEauxVarTot"),
      row("xWeightedStratum", "as x",
          "Sum of wStages x xSample over the samples of the stratum")))),
    "ratios$totals" = list("totals stratum", data.table::rbindlist(list(
      row("<strata>", "", "Your totals strata columns, the keys of your table"),
      row("rTotals", "factor", "Totals ratio: total / xWeightedStratum"),
      row("total", "as your table", "Your total for the stratum"),
      row("xWeightedStratum", "as x",
          "Sum of wStages x rDE x xSample over the samples of the stratum"),
      row("nUnits", "units", "Samples (or stage units) in the stratum")))),
    samples = list("lowest-level sample (SA)", data.table::rbindlist(list(
      row("SAid", "", "The lowest-level SA, where the fish were measured"),
      row("topSAid", "", "The top SA of its sub-sample tree"),
      row("SAlevel", "", "Sub-sample level, 1 without sub-sampling"),
      row("XXid", "", "Ids of the path up the hierarchy (SS, FO, FT, VS in H1, SS, FO, FT in H2, SS, FT, LE, OS in H5)"),
      row("DEid", "", "The DE record of the sample"),
      row("DEstratumName", "", "The DE stratum of the sample"),
      domain,
      row("include", "", "TRUE if the sample is used"),
      row("excludeReason", "", "Why the sample is not used"),
      row("fSample", "factor",
          "Within-sample factor: measured fish to catch fraction (sampleRaising)"),
      row("xSample", "kg",
          "Live weight of the species in the catch fraction of the sample"),
      row("f_XX", "factor", "Factor of stage XX on the sample's path (fStage)"),
      row("wStages", "factor", "Product of the stage factors f_XX"),
      row("XXselectMeth", "", "Selection method of stage XX, from the data"),
      row("XXselectionClass", "",
          "Design class of the selection method (classifySelectionMethod)"),
      row("XXstratumName", "", "Design stratum of stage XX, from the data"),
      row("XXnumTotal", "units", "Reported number of units of stage XX"),
      row("XXnumSamp", "units", "Reported number of sampled units of stage XX"),
      row("XXclustering", "", "Clustering of stage XX, from the data"),
      row("SAlowHierarchy", "", "Lower hierarchy (A, B, C or D)"),
      row("SAspeCodeFAO", "", "FAO species code"),
      row("SAspeCode", "", "AphiaID of the species"),
      row("SAcatchCat", "", "Catch category"),
      row("SApres", "", "Presentation of the weighed sample"),
      row("SAconFacMeasLive", "factor",
          "Conversion factor from measured to live weight")))),
    stageUnits = list("sampled unit of one stage (one table per stage)",
      data.table::rbindlist(list(
        row("XXid", "", "The unit and the ids of the stages above it"),
        row("SDid", "", "Sampling details id"),
        row("DEid", "", "The DE record"),
        row("DEstratumName", "", "The DE stratum"),
        row("fStage", "factor", "Factor of this stage only: total / sum of value"),
        row("wCumulative", "factor",
            "Product of the factors from the top stage down to this unit")))),
    fishBySample = list("sample x class", data.table::rbindlist(list(
      row("SAid", "", "The lowest-level SA"),
      row("class", "years or mm", "Age, or length class lower bound"),
      row("yN", "fish", "Fish in the class, scaled to the catch fraction (x fSample)"),
      row("yNW", "fish", "Of yN, the fish that had a weight"),
      row("yW_g", "g", "Summed live weight of those fish"),
      row("yNL", "fish", "Of yN, the fish that had a length"),
      row("yL_mm", "mm", "Summed length of those fish"),
      fish))),
    byStratum = list("domain x DE stratum x class", data.table::rbindlist(list(
      domain,
      row("DEstratumName", "", "The DE stratum. Not present with totals"),
      row("class", "years or mm", "Age, or length class lower bound"),
      row("N", "fish", "Raised number of fish: sum of yN x wStages x ratios"),
      row("NW", "fish", "Raised number of fish that had a weight"),
      row("W_g", "g", "Raised live weight of those fish"),
      row("NL", "fish", "Raised number of fish that had a length"),
      row("L_mm", "mm", "Raised summed length of those fish"),
      fish, liveWeight))),
    log = list("log entry", data.table::rbindlist(list(
      row("step", "", "The function (and stage) that wrote the entry"),
      row("level", "", "info, assumption, exclusion or warning"),
      row("message", "", "What happened"),
      row("n", "records", "Number of records concerned")))))

  out <- data.table::rbindlist(lapply(names(parts), function(el) {
    d <- data.table::copy(parts[[el]][[2]])
    d[, `:=`(element = el, rowUnit = parts[[el]][[1]])]
    d
  }))
  data.table::setcolorder(out, c("element", "column", "rowUnit", "unit",
                                 "meaning"))
  out
}

#' Does an output column match a guide column (with XX for a stage prefix)?
#' @keywords internal
#' @noRd
guideColumnPattern <- function(column) {
  p <- gsub("XX", "[A-Z]{2}", column, fixed = TRUE)
  paste0("^", p, "$")
}
