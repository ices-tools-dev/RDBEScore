# AI-assisted: Yes
# Review: rix133

# Evaluate a call to a deprecated function, muffling only its deprecation
# warning, so that its other warnings can still be tested
withoutDeprecation <- function(expr) {
  withCallingHandlers(expr, deprecatedWarning = function(w) {
    invokeRestart("muffleWarning")
  })
}
