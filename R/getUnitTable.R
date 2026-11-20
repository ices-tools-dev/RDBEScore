#' Table of the units of one sampling stage
#'
#' Step of stage-wise raising (see [raisingScheme()]). Returns one row per
#' unit of one stage (for example every haul in FO), built from that stage's
#' own table. Units that were sampled but did not catch the species are
#' therefore included, which matters for counts and auxiliary sums.
#'
#' Each row has the unit id, the parent id, the ids of all stages above up to
#' DE with `DEstratumName`, the design variables of the stage
#' (`XXstratumName`, `XXnumTotal`, `XXnumSamp`, `XXincProb`, `XXauxVarTot`,
#' `XXauxVarValue`, `XXselectMeth`, `XXsamp`, ...), any extra `fields`, and
#' `sampled`.
#'
#' A unit counts as **sampled** when `XXsamp` is not `"N"` and it has at
#' least one record in the stage below. Units marked sampled without records
#' below are logged and treated as not sampled.
#'
#' @param RDBESDataObj An `RDBESDataObject` of hierarchy 1, 2 or 5.
#' @param stage Stage table name: `"VS"`, `"FT"` or `"FO"` in hierarchy 1, `"FT"` or `"FO"` in
#'   hierarchy 2,
#'   `"OS"`, `"LE"` or (when SS links to FT) `"FT"` in hierarchy 5.
#' @param fields Optional extra columns of the stage table, or of VD (for
#'   stage tables with a `VDid`, such as VS and LE, joined through `VDid`).
#' @param verbose Logical. Print log entries as messages. Default `FALSE`.
#'
#' @return A `data.table`, one row per unit. The log is attached as
#'   attribute `"ratioLog"`.
#' @seealso [getStageWeights()], [computeStageFactors()]
#' @section Development review:
#' - AI-assisted: Yes
#' - Review: pending
#' - Notes/scope:
#' @export
getUnitTable <- function(RDBESDataObj, stage, fields = NULL,
                         verbose = FALSE) {
  sc <- ratioStageChain(RDBESDataObj)
  chain <- sc$chain
  stages <- sc$stages
  if (!stage %in% stages) {
    stop("stage must be one of ", paste(stages, collapse = ", "), ".",
         call. = FALSE)
  }
  step <- paste0("getUnitTable (", stage, ")")
  log <- newRatioLog()
  tbl <- RDBESDataObj[[stage]]
  if (is.null(tbl) || nrow(tbl) == 0) {
    stop("The ", stage, " table is empty.", call. = FALSE)
  }
  pos <- match(stage, chain)
  parentTb <- chain[pos - 1]
  childTb <- chain[pos + 1]
  idCol <- paste0(stage, "id")
  parentId <- paste0(parentTb, "id")

  designCols <- intersect(paste0(stage, RDBEScore::designVariables),
                          names(tbl))
  vdFields <- if ("VDid" %in% names(tbl)) {
    grep("^VD", fields, value = TRUE)
  } else {
    NULL
  }
  ownFields <- setdiff(fields, vdFields)
  badFields <- setdiff(ownFields, names(tbl))
  if (length(badFields) > 0) {
    stop("Field(s) not in ", stage, ": ", paste(badFields, collapse = ", "),
         call. = FALSE)
  }
  u <- data.table::copy(tbl[, unique(c(idCol, parentId, designCols,
                                        ownFields)), with = FALSE])

  if (length(vdFields) > 0) {
    vd <- RDBESDataObj$VD
    badVD <- setdiff(vdFields, names(vd))
    if (is.null(vd) || length(badVD) > 0 || !"VDid" %in% names(tbl)) {
      stop("VD field(s) not available: ",
           paste(c(badVD, if (is.null(vd)) "no VD table"), collapse = ", "),
           call. = FALSE)
    }
    u[, VDid := tbl$VDid]
    u <- merge(u, vd[, c("VDid", vdFields), with = FALSE], by = "VDid",
               all.x = TRUE)
  }

  # Ids of all stages above, up to DE
  k <- pos - 1
  while (k >= 2) {
    tb <- chain[k]
    up <- chain[k - 1]
    link <- RDBESDataObj[[tb]][, c(paste0(tb, "id"), paste0(up, "id")),
                               with = FALSE]
    if (!paste0(up, "id") %in% names(u)) {
      u <- merge(u, link, by = paste0(tb, "id"), all.x = TRUE)
    }
    k <- k - 1
  }
  de <- RDBESDataObj$DE[, intersect(c("DEid", "DEstratumName",
                                      "DEauxVarTot", "DEauxVarName"),
                                    names(RDBESDataObj$DE)), with = FALSE]
  u <- merge(u, de, by = "DEid", all.x = TRUE)

  # Sampled: XXsamp not "N" and at least one record in the stage below
  sampCol <- paste0(stage, "samp")
  flagged <- if (sampCol %in% names(u)) {
    is.na(u[[sampCol]]) | u[[sampCol]] != "N"
  } else {
    rep(TRUE, nrow(u))
  }
  hasChild <- u[[idCol]] %in% RDBESDataObj[[childTb]][[idCol]]
  noChild <- sum(flagged & !hasChild)
  if (noChild > 0) {
    log <- addToRatioLog(log, step, "warning",
      paste0(stage, " units marked sampled but without ", childTb,
             " records: treated as not sampled"), noChild, verbose)
  }
  u[, sampled := flagged & hasChild]
  data.table::setcolorder(u, c(idCol, parentId))
  data.table::setorderv(u, idCol)
  attr(u, "ratioLog") <- log
  u[]
}
