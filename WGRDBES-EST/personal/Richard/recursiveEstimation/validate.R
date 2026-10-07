# Validation of the recursive estimation prototype against independent
# implementations (survey package, estimMC, doEstimationRatio,
# doEstimationForAllStrata). Run from the package root.

suppressMessages(devtools::load_all(quiet = TRUE))
suppressMessages(library(survey))
library(SDAResources)
source("WGRDBES-EST/personal/Richard/recursiveEstimation/recursiveEstim.R")

checks <- list()
record <- function(name, ours, ref) {
  ours <- as.numeric(ours)
  ref <- as.numeric(ref)
  ok <- isTRUE(all.equal(unname(ours), unname(ref), tolerance = 1e-8))
  checks[[name]] <<- data.frame(check = name, ours = signif(ours, 10),
                                reference = signif(ref, 10), equal = ok)
}
# The SDAResources test objects have SAsamp == "N" on every SA row although
# all values are present (conversion artifact) - treat them as sampled
markSampled <- function(x) {
  x$SA <- copy(x$SA)
  x$SA[, SAsamp := "Y"]
  x
}
# node estimates are sparse: an absent (node, var) entry is a zero total
valueAt <- function(fit, nodes, variable) {
  r <- fit$nodes[var == variable]
  out <- r$t[match(nodes, r$node)]
  out[is.na(out)] <- 0
  out
}
rootEst <- function(fit, variable = NULL) {
  r <- fit$nodes[node == "ROOT"]
  if (!is.null(variable)) r <- r[var == variable]
  r
}

# ---- A: two-stage SRSWOR, survey::apiclus2 ----------------------------------
x <- Pckg_survey_apiclus2_H1
x$SA <- copy(x$SA)
# 6 schools have NA enroll and SAsamp == "N". To replicate
# svytotal(na.rm = TRUE) they stay in the design with y = 0 (domain exclusion)
x$SA[SAsamp == "N", SAsamp := "Y"]
# FO is a dummy CENSUS table with FOnumSamp = 10 but one row per FT
tr <- suppressWarnings(designTree(x, "SA", strictSampleSize = FALSE))
fit <- foldEstimate(tr, leavesSA(tr, x, "SAsampWtMes", na = "zero"))
data(api)
dclus2 <- svydesign(id = ~dnum + snum, fpc = ~fpc1 + fpc2, data = apiclus2)
ref <- svytotal(~enroll, dclus2, na.rm = TRUE)
record("A apiclus2 total", rootEst(fit)$t, coef(ref))
record("A apiclus2 SE", sqrt(rootEst(fit)$v), SE(ref))

# ---- B: stratified SRSWOR, SDAResources::agstrat -----------------------------
x <- markSampled(Pckg_SDAResources_agstrat_H1)
tr <- suppressWarnings(designTree(x, "SA", strictSampleSize = FALSE))
fit <- foldEstimate(tr, leavesSA(tr, x, "SAsampWtMes"))
data(agstrat)
agstrat$Nh <- c(NC = 1054, NE = 220, S = 1382, W = 422)[agstrat$region]
dstr <- svydesign(id = ~1, strata = ~region, fpc = ~Nh, data = agstrat)
ref <- svytotal(~acres92, dstr)
record("B agstrat total", rootEst(fit)$t, coef(ref))
record("B agstrat SE", sqrt(rootEst(fit)$v), SE(ref))

# ---- C: ratio estimator at the VS stage, SDAResources::agsrs ----------------
# Lohr example: y = acres92, auxiliary x = acres87, t_x = 964,470,625
x <- markSampled(Pckg_SDAResources_agsrs_H1)
data(agsrs)
x$VS <- copy(x$VS)
rowInAgsrs <- as.integer(sub(".* ", "", x$VS$VSunitName))
x$VS[, `:=`(VSauxVarValue = agsrs$acres87[rowInAgsrs],
            VSauxVarTot = 964470625, VSauxVarName = "acres87")]
tr <- suppressWarnings(designTree(x, "SA", ratio = c(VS = "aux"),
                                  strictSampleSize = FALSE))
fit <- foldEstimate(tr, leavesSA(tr, x, "SAsampWtMes"))
# the VS units must carry the agsrs acres92 values for the aux join to be right
stopifnot(all.equal(valueAt(fit, paste0("VS:", x$VS$VSid), "SAsampWtMes"),
                    agsrs$acres92[rowInAgsrs]))
dsrs <- svydesign(id = ~1, fpc = ~rep(3078, 300), data = agsrs)
ref <- predict(svyratio(~acres92, ~acres87, dsrs), total = 964470625)
record("C agsrs ratio total", rootEst(fit)$t, ref$total)
record("C agsrs ratio SE", sqrt(rootEst(fit)$v), ref$se)

# ---- D: single-stage parity with estimMC (SRSWOR, SRSWR, UPSWR) -------------
for (m in c("SRSWOR", "SRSWR", "UPSWR")) {
  xm <- markSampled(Pckg_SDAResources_agsrs_H1)
  xm$VS <- copy(xm$VS)
  xm$VS[, VSselectMeth := m]
  # artificial strictly positive size measure, only for formula parity
  # (one county is 0 on every agsrs size variable)
  sz <- agsrs$farms87[rowInAgsrs] + 1
  if (m == "UPSWR") xm$VS[, VSselProb := sz / (sum(sz) * 3078 / 300)]
  tr <- suppressWarnings(designTree(xm, "SA", strictSampleSize = FALSE))
  fit <- foldEstimate(tr, leavesSA(tr, xm, "SAsampWtMes"))
  y <- valueAt(fit, paste0("VS:", xm$VS$VSid), "SAsampWtMes")
  mc <- estimMC(y, as.numeric(xm$VS$VSnumSamp), as.numeric(xm$VS$VSnumTotal),
                m, selProb = xm$VS$VSselProb)
  st <- fit$strata[table == "VS"]
  record(paste("D estimMC", m, "total"), st$t, mc$est.total)
  record(paste("D estimMC", m, "var"), st$v, mc$var.total)
}

# ---- E: SA weight-ratio raising, H8 numbers at age vs doEstimationRatio -----
x <- H8ExampleEE1
tr <- suppressWarnings(designTree(x, "BV", bvAssess = "Age",
                                  ratio = c(SA = "weight"),
                                  strictSampleSize = FALSE))
fit <- foldEstimate(tr, leavesBV(tr, x, classMeas = "Age"))
ours <- fit$strata[table == "SA"]
ours[, SAid := as.numeric(sub("^SS:", "", parent))]  # SS and SA are 1:1 in H8
stopifnot(all(x$SA$SSid == x$SA$SAid))
ours[, Age := sub("^N\\|Age=", "", var)]
ref <- suppressWarnings(doEstimationRatio(H8ExampleEE1, "AgeComp", "Weight"))
cmp <- merge(ours[, .(SAid, Age, t)],
             ref[, .(SAid, Age = as.character(Age), NumbersAtAge)],
             by = c("SAid", "Age"), all = TRUE)
record("E H8 N-at-age per SA (rows)", nrow(cmp), nrow(ref))
record("E H8 N-at-age per SA (sum)", sum(cmp$t), sum(cmp$NumbersAtAge))
record("E H8 N-at-age per SA (max abs diff)",
       max(abs(cmp$t - cmp$NumbersAtAge)), 0)

# ---- F: HT totals per TE stratum, H8, vs doEstimationForAllStrata -----------
tr <- suppressWarnings(designTree(x, "SA", strictSampleSize = FALSE))
fit <- foldEstimate(tr, leavesSA(tr, x, "SAsampWtMes"))
estObj <- suppressWarnings(createRDBESEstObject(H8ExampleEE1, 8, "SA"))
old <- as.data.table(doEstimationForAllStrata(estObj, "SAsampWtMes"))
oldTE <- old[recType == "TE", .(stratum = stratumName, old = est.total)]
newTE <- fit$strata[table == "TE", .(stratum, new = t)]
cmp <- merge(oldTE, newTE, by = "stratum")
record("F H8 TE strata (n)", nrow(cmp), nrow(oldTE))
record("F H8 TE strata totals (max rel diff)",
       max(abs(cmp$new / cmp$old - 1)), 0)

res <- do.call(rbind, checks)
rownames(res) <- NULL
print(res, right = FALSE)
if (!all(res$equal)) stop("Some checks failed")
