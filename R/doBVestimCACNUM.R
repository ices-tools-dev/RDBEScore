##' Estimate Catch at Number (CANUM) for Biological Variables
#'
#' This function estimates catch at number (CANUM) for a specified biological variable, such as age or length. It aggregates data based on specified columns and generates a "plus group" for the highest value in the defined classes. The function supports grouping by various units (e.g., age, length, weight) and calculates required indices, totals, and proportions for the groups.
#'
#' @param bv A `data.table` containing biological data, with columns for the biological variable, class units (e.g., `Ageyear`, `Lengthmm`, `Weightg`), and other relevant variables.
#' @param addColumns A character vector of additional column names used to group the data for aggregation (e.g., `BVfishId` and other identifiers).
#' @param classUnits A character string specifying the class units of the biological variable to use for grouping (e.g., "Ageyear", "Lengthmm", "Weightg"). Default is "Ageyear".
#' @param classBreaks A numeric vector specifying the breakpoints for classifying the biological variable. The last value defines the lower bound of the "plus group". Default is `1:8` for age groups.
#' @param verbose Logical, if `TRUE`, prints detailed information about the process. Default is `FALSE`.
#'
#' @return A `data.table` containing the aggregated results, including groupings, calculated means, proportions, indices, and totals for the specified biological variable.
#'
#' @details The function performs the following steps:
#' \itemize{
#'   \item Validates the presence of the `classUnits` in the biological variable data.
#'   \item Reshapes the input data using `dcast` and groups the biological variable into classes using `cut()`.
#'   \item Aggregates mean weights and lengths by the defined classes, along with calculating proportions and indices based on the sample size.
#'   \item A "plus group" is created for values exceeding the highest `classBreaks` value.
#'   \item Calculates total weights, catch numbers, and performs a sanity check to ensure there are no rounding errors in the final results.
#' }
#' @export
doBVestimCANUM <- function(bv, addColumns,
                         classUnits = "Ageyear",
                         classBreaks = 1:8,
                         verbose = FALSE){
  rightF <- "BVvalUnitScale"
  #the class unit must be one of "Sex" "Lengthmm" "Ageyear"  "Weightg"  "SMSF"
  if(!(classUnits %in% unique(bv[[rightF]]))){
  stop("The class unit must be present in data column BVvalUnitScale ",
       "the available values are: ", paste0(unique(bv[[rightF]]), collapse = ", "))
  }

  #extract raw values
  leftF <- paste0(c("BVfishId", addColumns), collapse = "+")

  bv_wide <- data.table::dcast(bv, formula(paste0(leftF, "~", rightF)), value.var = "BVvalueMeas")
  bv_wide$target <- as.numeric(bv_wide[[classUnits]])

  classLabs <- switch(classUnits,
                      Ageyear = c(classBreaks[-length(classBreaks)], paste0(max(classBreaks), "+")),
                      c(paste0(classBreaks[-length(classBreaks)], "-", classBreaks[-1]),  paste0(max(classBreaks), "+")))

  # Create the 'plus group' by using cut() to assign groups based on classBreaks
  bv_wide$Group <- cut(bv_wide$target, breaks = c(classBreaks, Inf),
                             include.lowest = TRUE, right = FALSE,
                             labels =classLabs)

  bv_wide$Lengthmm <- as.numeric(bv_wide$Lengthmm)
  bv_wide$Weightg <- as.numeric(bv_wide$Weightg)

  #aggregate values
  a <- bv_wide[, .(WeightgMean = mean(Weightg, na.rm = TRUE),
                   WeightgLen = sum(!is.na(Weightg)),
                   LengthmmMean = mean(Lengthmm, na.rm = TRUE)),
               by = c("Group",addColumns)]

  b <- bv_wide[, .(lenMeas = sum(!is.na(Lengthmm)),
                   targetMeas = sum(!is.na(Group))),
               by = addColumns]

  targetWeights <- merge(a, b, by = addColumns)

  #remove the NA row
  targetWeights <- targetWeights[!is.na(targetWeights$Group), ]

  #add extra columns
  #targetWeights$MeanLengthCm <- targetWeights$Lengthmm / 10
  targetWeights$plusGroup <- classBreaks[length(classBreaks)]

  #calculate required values
  targetWeights$propSample <- targetWeights$WeightgLen / targetWeights$targetMeas
  targetWeights$WeightIndex <- targetWeights$propSample * (targetWeights$WeightgMean / 1000)

  # Calculate the sum of WeightIndex for each group defined by addColumns
  targetWeights[, WeightIndexSum := sum(WeightIndex), by = addColumns]

  targetWeights$TWCoef <- targetWeights$sumCLoffWeight / targetWeights$WeightIndexSum
  targetWeights$totWeight <-  targetWeights$WeightIndex * targetWeights$TWCoef
  targetWeights$totNum <- targetWeights$totWeight / (targetWeights$WeightgMean / 1000)

  # Sanity check with tolerance to avoid rounding error
  weights <- targetWeights$totNum * (targetWeights$WeightgMean / 1000)
  expected_sum <- sum(unique(targetWeights$sumCLoffWeight))


  # Use all.equal to compare with tolerance or manually check the difference
  if(!isTRUE(all.equal(sum(weights), expected_sum))) {
    stop("Strange problem: sums do not match within tolerance")
  }

  targetWeights
}
