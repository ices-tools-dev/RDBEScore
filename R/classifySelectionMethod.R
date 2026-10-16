#' Classify RDBES selection methods by design quality
#'
#' Maps RDBES selection method codes (ICES code type `SelectionMethod`) to the
#' classes used when choosing an estimator, following "Use of selection
#' methods during estimation in the RDBES" in the RDBES data model
#' documentation:
#'
#' * `Probabilistic`: SRSWR, SRSWOR, UPSWR, UPSWOR, SYSS
#' * `Census`: CENSUS
#' * `Quasi-probabilistic`: NPQSRSWR, NPQSRSWOR, NPQSYSS
#' * `Non-probabilistic`: NPJS, NPCS, NPCLQS-T, NPCLQS-O, Unknown
#' * `Other`: FIXED, NotApplicable, D, R (depend on how the original or
#'   remaining selection was made)
#'
#' Codes not in this mapping return `"Unrecognised"`. The classification is
#' an RDBEScore convention, not part of the ICES vocabulary.
#'
#' @param x Character vector of selection method codes.
#'
#' @return Character vector of the same length as `x`. `NA` where `x` is
#'   `NA` or empty.
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
#' @export
#'
#' @examples
#' classifySelectionMethod(c("SRSWOR", "NPQSRSWOR", "NPCS", "CENSUS"))
classifySelectionMethod <- function(x) {
  x <- as.character(x)
  cls <- selectionMethodClasses()
  out <- cls$selectionClass[match(x, cls$selectMeth)]
  out[is.na(out) & !isBlank(x)] <- "Unrecognised"
  out
}
