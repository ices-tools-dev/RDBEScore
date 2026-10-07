# Demo: sprat numbers-at-age by quarter from H8ExampleEE1, three ways
#   1. design-based, SA stage HT (raise boxes by SAnumTotal / SAnumSamp)
#   2. design-based, SA stage weight ratio (SAtotalWtMes / SAsampWtMes)
#   3. as 2, then combined ratio to CL landings per quarter
# and the vignette approach (addCLtoLowerCS + doBVestimCANUM) for reference.
# Run from the package root.

suppressMessages(devtools::load_all(quiet = TRUE))
source("WGRDBES-EST/personal/Richard/recursiveEstimation/recursiveEstim.R")

x <- H8ExampleEE1
stopifnot(uniqueN(x$LE$LEarea) == 1, uniqueN(x$LE$LEmetier6) == 1,
          uniqueN(x$SA$SAspeCodeFAO) == 1)

# H8ExampleEE1 has two weeks with 2 VS rows but VSnumSamp = 1 (TEid 1, 3);
# kept as in doEstimationForAllStrata, i.e. each row raised by 11/1

# Variance: weeks (TE) are SRSWOR 2-3 of 4 per month, and most weeks have a
# single vessel, so the within-week variance is not estimable and the
# unbiased variance is NA. Here the TE stage variance is approximated as WR
# (ultimate cluster at the PSU): conservative, point estimates unchanged.
wrApprox <- "TE"
quarterOf <- function(tree, nodes) {
  m <- ancestorValue(tree, nodes, x, "TEstratumName")
  paste0("Q", ceiling(match(m, month.name) / 3))
}

runDesign <- function(saStage) {
  ratio <- if (saStage == "ratio") c(SA = "weight") else character(0)
  tr <- suppressWarnings(designTree(x, "BV", bvAssess = "Age", ratio = ratio,
                                    strictSampleSize = FALSE,
                                    varianceAsWR = wrApprox))
  lv <- leavesBV(tr, x, classMeas = "Age")
  lv <- rbind(lv, leavesSA(tr, x, "SAsampWtMes"))
  # domain = quarter, from the TE stratum (month) of each leaf
  lv[, var := paste0(var, "#", quarterOf(tr, node))]
  fit <- foldEstimate(tr, lv)
  r <- fit$nodes[node == "ROOT"]
  r[, c("var", "quarter") := tstrsplit(var, "#")]
  r[, method := paste("design, SA", saStage)]
  list(tree = tr, fit = fit, root = r[])
}

ht <- runDesign("HT")
rt <- runDesign("ratio")

# landed weight estimated from the sample vs CL (kg -> g)
cl <- x$CL[CLarea == unique(x$LE$LEarea) & CLmetier6 == unique(x$LE$LEmetier6) &
             CLspecFAO == unique(x$SA$SAspeCodeFAO)]
X <- cl[, .(X = sum(CLoffWeight) * 1000), by = .(quarter = paste0("Q", CLquar))]
landings <- merge(X, rbind(ht$root, rt$root)[var == "SAsampWtMes",
                                              .(quarter, method, t, se = sqrt(v))],
                  by = "quarter")
cat("\nLanded weight (t): CL vs estimated from the sample design\n")
print(landings[, .(quarter, method, CL = X / 1e6, est = t / 1e6, se = se / 1e6)],
      digits = 4)

# 3. ratio to CL landings, linearised variance
yVars <- unique(grep("^N\\|", sub("#.*", "", rt$root$var), value = TRUE))
fitNoDom <- {
  tr <- rt$tree
  lv <- rbind(leavesBV(tr, x, classMeas = "Age"), leavesSA(tr, x, "SAsampWtMes"))
  foldEstimate(tr, lv)
}
trUpper <- suppressWarnings(designTree(x, "SA", ratio = c(SA = "weight"),
                                       strictSampleSize = FALSE,
                                       varianceAsWR = wrApprox))
cl3 <- ratioToTotal(fitNoDom, trUpper, yVars, "SAsampWtMes",
                    domainOf = function(nodes) quarterOf(trUpper, nodes),
                    X = setNames(X$X, X$quarter))
# sanity: the ratio estimate of the x variable itself must reproduce CL
chk <- ratioToTotal(fitNoDom, trUpper, "SAsampWtMes", "SAsampWtMes",
                    function(nodes) quarterOf(trUpper, nodes),
                    setNames(X$X, X$quarter))
stopifnot(isTRUE(all.equal(chk$est, X$X[match(chk$domain, X$quarter)])),
          all(abs(chk$se) < 1e-6 * chk$est))

# vignette approach for reference
vig <- rbindlist(lapply(list(Q1 = list(1:3, 1), Q4 = list(10:12, 4)), function(q) {
  b <- addCLtoLowerCS(x, list(LEarea = "27.3.d.28.1", LEmetier6 = "OTM_SPF_16-31_0_0",
                              TEstratumName = month.name[q[[1]]], SAspeCodeFAO = "SPR"),
                      list(CLarea = "27.3.d.28.1", CLquar = q[[2]],
                           CLmetier6 = "OTM_SPF_16-31_0_0", CLspecFAO = "SPR"),
                      combineStrata = TRUE, lowerHierarchy = "C",
                      CLfields = "CLoffWeight")
  r <- doBVestimCANUM(b, "sumCLoffWeight", classUnits = "Ageyear",
                      classBreaks = 1:12)
  r[, .(quarter = paste0("Q", q[[2]]), Age = as.character(Group), est = totNum,
        method = "vignette doBVestimCANUM")]
}))

age <- rbind(
  ht$root[grepl("^N\\|", var), .(quarter, Age = sub("N\\|Age=", "", var), est = t,
                                se = sqrt(v), method)],
  rt$root[grepl("^N\\|", var), .(quarter, Age = sub("N\\|Age=", "", var), est = t,
                                se = sqrt(v), method)],
  cl3[, .(quarter = domain, Age = sub("N\\|Age=", "", var), est, se,
          method = "design, SA ratio + ratio to CL")],
  vig[, .(quarter, Age, est, se = NA_real_, method)])
age[, Age := as.integer(sub("\\+", "", Age))]
wide <- dcast(age, quarter + Age ~ method, value.var = "est")
cat("\nNumbers at age (millions)\n")
num <- setdiff(names(wide), c("quarter", "Age"))
wide[, (num) := lapply(.SD, function(z) round(z / 1e6, 2)), .SDcols = num]
print(wide[order(quarter, Age)])
cat("\nRelative SE (%) of numbers at age\n")
cv <- dcast(age[!is.na(se)], quarter + Age ~ method,
            value.var = "se", fun.aggregate = function(z) z[1])
cvv <- dcast(age[!is.na(se)], quarter + Age ~ method, value.var = "est")
for (m in setdiff(names(cv), c("quarter", "Age")))
  cv[[m]] <- round(100 * cv[[m]] / cvv[[m]], 1)
print(cv[order(quarter, Age)])
