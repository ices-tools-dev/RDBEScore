#' Generic function for estimation of population total and variance
#'
#' @param y numeric variable to be estimated
#' @param enk expected value of k
#' @param enkl expected value of k, given l
#' @param method character selection method code e.g SRSWOR
#' @param estFunction the function to use to estimate total given
#' parameters y and enk
#' @param varFunction the function to use to estimate variance given
#' parameters y,enk and enkl
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
estim <- function(y,
                  enk,
                  enkl,
                  method = "SRSWOR",
                  estFunction,
                  varFunction,
                  verbose = FALSE) {

  n <- length(y)

  if (verbose){
    print("Esimating total using user-supplied function")
  }

  # Total
  est.algorithm <- "User supplied"
  est.total <- estFunction(y,enk)

  if (verbose){
    print("Esimating variance using user-supplied function")
  }

  # Variance
  var.algorithm <- "User supplied"
  var.total <- varFunction(y,enk,enkl)

  PI <- enkl

  return(list(
    est.total = est.total,
    est.algorithm = est.algorithm,
    var.total = ifelse(n < 2, NaN, var.total),
    var.algorithm = var.algorithm,
    PI = PI
  ))
}
