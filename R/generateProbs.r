#' Generate vector of selection or inclusion probabilities
#'
#' @param x RDBES data frame
#' @param probType  Value == "selection" | "inclusion" ,
# for selection or inclusion probabilities respectively
#'
#' @return A vector or probabilities
#' @export
#'
#' @examples
#' \dontrun{
#' generateProbs(x = y, probType = "inclusion")
#' }
generateProbs <- function(x, probType) {

  # Only allow "inclusion" or "selection" probType at the moment
  if (!probType %in% c("inclusion", "selection")) {
    stop(paste0("Unallowed value supplied for 'probType': ", probType))
  }

  methColNames <- grep("^..selectMeth$", names(x), value = TRUE)
  a <- as.character(unique(x[, ..methColNames]))


  if (sum(is.na(a)) > 0) stop("cannot proceed: NAs in SSselectMeth")
  if (length(a) > 1) stop("two different selection methods")

  numSampColNames <- grep("^..numSamp$", names(x), value = TRUE)
  vecSmallN <- x[[numSampColNames]]
  numTotalColNames <- grep("^..numTotal$", names(x), value = TRUE)
  vecBigN <- x[[numTotalColNames]]

  vecProb <- NA

  if (probType == "selection") {
    vecProbColNames <- grep("^..selProb$", names(x), value = TRUE)
    vecProb <- x[[vecProbColNames]]

    print(a)
    if (a %in% c("SRSWR", "SRSWOR")) {
      if (a == "SRSWR") vecProb <- 1 / vecBigN
      if (a == "SRSWOR") stop("depends on order")
    }
    if (a %in% c("UPSWR", "UPSWOR")) {
      if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
      vecProb <- vecProb
    }
    if (a == "CENSUS"){
      vecProb <- rep(1,length(vecProb))
    }
    x[[vecProbColNames]] <- vecProb
  }


  if (probType == "inclusion") {
    vecProbColNames <- grep("^..incProb$", names(x), value = TRUE)
    vecProb <- x[[vecProbColNames]]

    if (length(a) > 1) {
      stop("two different selection methods")
    } else {
      print(a)
      if (a %in% c("SRSWR", "SRSWOR")) {
        if (sum(is.na(vecBigN)) > 0) stop("cannot proceed: NAs in total")
        if (sum(is.na(vecSmallN)) > 0) stop("cannot proceed: NAs in sampled")
        if (a == "SRSWR") vecProb <- 1 - (1 - 1 / vecBigN)^vecSmallN
        if (a == "SRSWOR") vecProb <- vecSmallN / vecBigN
      }
      if (a %in% c("UPSWR", "UPSWOR")) {
        if (sum(is.na(vecProb)) > 0) stop("cannot proceed: NAs in sampProb")
        vecProb <- vecProb
      }
      if (a == "CENSUS"){
        vecProb <- rep(1,length(vecProb))
      }
    }
    x[[vecProbColNames]] <- vecProb
  }

  x
}
