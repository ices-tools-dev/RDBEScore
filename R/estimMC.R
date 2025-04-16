#' Multiple Count Estimator for Population Total and Variance
#'
#' @param y numeric variable to be estimated
#' @param sampled numeric total number of units sampled
#' @param total numeric total number of units int the population
#' @param method character selection method code e.g SRSWOR
#' @param selProb the selection probabilities (if known)
#' @param incProb the inclusion probabilities (if known)
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#' out, or FALSE if you don't.  The default is FALSE.
#'
#' @return list of 7 elements including the population mean, total
#' (and their variance), the algorithm name used and the I order
#' inclusion probabilities
#' @export
#'
#' @examples
#' estimMC(c(3, 4, 4, 5), c(4, 4, 4, 4), c(8, 8, 8, 8))
estimMC <- function(y, sampled, total, method = "SRSWOR", selProb = NULL,
                    incProb = NULL, verbose = FALSE) {
  implementedMethods <- c(
    "^SRSWR$",
    "^SRSWOR$",
    "^CENSUS$",
    "^UPSWR$",
    "^UPSWOR$"
  )
  if (!any(sapply(implementedMethods, grepl, method))) {
    stop(paste("Sampling method ", method, "not implemented!"))
  }

  n <- length(y)
  if (!all(sapply(c(y, sampled, total), is.numeric))) {
    stop("y, sampled and total must, be numeric vectors!")
  }

  if (any(length(sampled) != n, length(total) != n)) {
    stop("y, sampled and total must, be vectors of same length!")
  }
  # TODO include also quasi random
  if (grepl("^SRS", method) || grepl("^CENSUS$", method)) {


    if (any(is.na(sampled) | is.na(total))  ){
      stop("sampled and total must be provided - NAs not allowed!")
    }

    enk <- sampled / total
    if (grepl("WOR$", method) || grepl("^CENSUS$", method)) {
      enkl <- enk %*% t((sampled - 1) / (total - 1))
      diag(enkl) <- enk
    }

    if (grepl("WR$", method)) {
      enkl <- (enk %*% t((sampled - 1) / total))
    }
  }


  if (grepl("^UPS", method)) {


    if (grepl("WOR$", method)) {

      if (( length(incProb) == 0 && is.null(incProb)) || (any(is.na(incProb)))){
        stop("incProb must be provided - NAs or NULL not allowed!")
      }

      enk <- incProb
      print(paste0("UPSWOR variance calculation requires second order",
      " inclusion probabilities which are not available in the RDBES -",
      " variance cannot be calculated"))
      enkl <- NA
    }

    if (grepl("WR$", method)) {

      if (( length(selProb) == 0 && is.null(selProb)) || (any(is.na(selProb)))){
        stop("selProb must be provided - NAs or NULL not allowed!")
      }

      enk <- n * selProb
      enkl <- enk %*% t((n-1) * selProb)
    }
  }

  if (verbose){
    print("Esimating total using Generalized Horvitz-Thompson aka Mutiple-Count")
  }

  # Generalized Horvitz-Thompson estimator
  est.algorithm <- "Generalized Horvitz-Thompson aka Mutiple-Count"
  estFunction <- function(y,enk){
    sum(y / enk)
  }

  if (verbose){
    print("Esimating variance using Sen-Yates-Grundy estimate")
  }

  # Sen-Yates-Grundy estimate of variance
  var.algorithm <- "Sen-Yates-Grundy"
  varFunction <- function(y,enk,enkl){
    # Sen-Yates-Grundy estimate of variance
    var.algorithm <- "Sen-Yates-Grundy"
    if (length(enkl) == 1 && is.na(enkl)) return(NA) # NA if we don't have enkl
    partY <- (y / enk) %*% t(rep(1,n)) - t((y / enk) %*% t(rep(1,n)) )
    diag(partY) <- NA
    partY <- partY^2
    var.mat <- 0.5 * (((enk %*% t(enk)) - enkl) / enkl)
    diag(var.mat) <- NA
    var.mat <- var.mat * partY
    var.total <- sum(var.mat, na.rm = T)
  }

  # Run the estimation using the functions we just defined
  estimResult <- estim(y, enk, enkl, method, estFunction, varFunction)

  # Calculate the means
  est.mean <- estimResult$est.total / mean(total)
  #var.mean <- estimResult$var.total / sum(total) * mean(sampled)
  var.mean <- estimResult$var.total / max(total)^2


  return(list(
    est.total = estimResult$est.total,
    est.mean = est.mean,
    est.algorithm = est.algorithm,
    var.total = estimResult$var.total,
    var.mean = var.mean,
    var.algorithm = var.algorithm,
    PI = estimResult$PI
  ))
}
