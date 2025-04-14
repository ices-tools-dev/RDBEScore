##' Estimate Catch at Number (CANUM) for Biological Variables
##'
##' This function estimates catch at number (CANUM) for a specified biological variable, such as age or length. It aggregates data based on specified columns and generates a "plus group" for the highest value in the defined classes. The function supports grouping by various units (e.g., age, length, weight) and calculates required indices, totals, and proportions for the groups.
##'
##' @param bv A `data.table` containing biological data, with columns for the biological variable, class units (e.g., `Ageyear`, `Lengthmm`, `Weightg`), and other relevant variables.
##' @param addColumns A character vector of additional column names used to group the data for aggregation (e.g., `BVfishId` and other identifiers).
##' @param classUnits A character string specifying the class units of the biological variable to use for grouping (e.g., "Ageyear", "Lengthmm", "Weightg"). Default is "Ageyear".
##' @param classBreaks A numeric vector specifying the breakpoints for classifying the biological variable. The last value defines the lower bound of the "plus group". Default is `1:8` for age groups.
##' @param verbose Logical, if `TRUE`, prints detailed information about the process. Default is `FALSE`.
##'
##' @return A `data.table` containing the aggregated results, including groupings, calculated means, proportions, indices, and totals for the specified biological variable.
##'
##' @details The function performs the following steps:
##' \itemize{
##'   \item Validates the presence of the `classUnits` in the biological variable data.
##'   \item Reshapes the input data using `dcast` and groups the biological variable into classes using `cut()`.
##'   \item Aggregates mean weights and lengths by the defined classes, along with calculating proportions and indices based on the sample size.
##'   \item A "plus group" is created for values exceeding the highest `classBreaks` value.
##'   \item Calculates total weights, catch numbers, and performs a sanity check to ensure there are no rounding errors in the final results.
##' }
##'
##' ### Mathematical Logic:
##' Let:
##' \itemize{
##'   \item \eqn{W_{mean}} be the mean weight for each group.
##'   \item \eqn{L_{mean}} be the mean length for each group.
##'   \item \eqn{n_W} be the number of weight measurements in each group.
##'   \item \eqn{N} be the total number of measurements in the sample.
##'   \item \eqn{P} be the proportion of the sample represented by each group.
##'   \item \eqn{I_W} be the weight index for each group.
##'   \item \eqn{S} be the sum of weight indices across all groups.
##'   \item \eqn{C} be the total catch weight.
##'   \item \eqn{T_W} be the total weight for each group.
##'   \item \eqn{C_{num}} be the total catch number for each group.
##' }
##'
##' The calculations are as follows:
##' \enumerate{
##'   \item Proportion of sample:
##'   \deqn{P = \frac{n_W}{N}}
##'
##'   \item Weight Index:
##'   \deqn{I_W = P \times \left( \frac{W_{mean}}{1000} \right)}
##'
##'   \item Sum of Weight Indices:
##'   \deqn{S = \sum I_W}
##'
##'   \item Total Weight Coefficient:
##'   \deqn{\mathrm{TWCoef} = \frac{C}{S}}
##'
##'   \item Total Weight per Group:
##'   \deqn{T_W = I_W \times \mathrm{TWCoef}}
##'
##'   \item Total Catch Number per Group:
##'   \deqn{C_{num} = \frac{T_W}{\left( \frac{W_{mean}}{1000} \right)}}
##' }
##'
##' @export
##' @importFrom stats formula
doBVestimCANUM <- function(bv, addColumns,
                           classUnits = "Ageyear",
                           classBreaks = 1:8,
                           verbose = FALSE){
  rightF <- "BVvalUnitScale"

  # Validate the presence of classUnits in the data
  if(!(classUnits %in% unique(bv[[rightF]]))){
    stop("The class unit must be present in data column BVvalUnitScale ",
         "the available values are: ", paste0(unique(bv[[rightF]]), collapse = ", "))
  }

  # Create the formula for reshaping
  leftF <- paste0(c("BVfishId", addColumns), collapse = "+")

  # Reshape data from long to wide format
  bv_wide <- data.table::dcast(bv, formula(paste0(leftF, "~", rightF)), value.var = "BVvalueMeas")

  # Extract the target biological variable
  bv_wide$target <- as.numeric(bv_wide[[classUnits]])

  # Define class labels, including the "plus group"
  classLabs <- switch(classUnits,
                      Ageyear = c(classBreaks[-length(classBreaks)], paste0(max(classBreaks), "+")),
                      c(paste0(classBreaks[-length(classBreaks)], "-", classBreaks[-1]),  paste0(max(classBreaks), "+")))

  # Assign groups based on classBreaks, creating a "plus group" for the highest class
  bv_wide$Group <- cut(bv_wide$target, breaks = c(classBreaks, Inf),
                       include.lowest = TRUE, right = FALSE,
                       labels = classLabs)

  # Ensure numerical columns are correctly typed
  bv_wide$Lengthmm <- as.numeric(bv_wide$Lengthmm)
  bv_wide$Weightg <- as.numeric(bv_wide$Weightg)

  # Aggregate mean weights and lengths, and count measurements per group
  a <- bv_wide[, .(
    WeightgMean = mean(Weightg, na.rm = TRUE),
    WeightgLen = sum(!is.na(Weightg)),
    LengthmmMean = mean(Lengthmm, na.rm = TRUE)
  ), by = c("Group", addColumns)]

  # Count total measurements per group
  b <- bv_wide[, .(
    lenMeas = sum(!is.na(Lengthmm)),
    targetMeas = sum(!is.na(Group))
  ), by = addColumns]

  # Merge aggregated data
  targetWeights <- merge(a, b, by = addColumns)

  # Remove rows with NA groups
  targetWeights <- targetWeights[!is.na(targetWeights$Group), ]

  # Assign the "plus group" boundary
  targetWeights$plusGroup <- classBreaks[length(classBreaks)]

  # Calculate the proportion of the sample
  targetWeights$propSample <- targetWeights$WeightgLen / targetWeights$targetMeas
  # \[
  # P = \frac{n_W}{N}
  # \]

  # Calculate the Weight Index
  targetWeights$WeightIndex <- targetWeights$propSample * (targetWeights$WeightgMean / 1000)
  # \[
  # I_W = P \times \left( \frac{W_{mean}}{1000} \right)
  # \]

  # Sum of Weight Indices per group defined by addColumns
  targetWeights[, WeightIndexSum := sum(WeightIndex), by = addColumns]
  # \[
  # S = \sum I_W
  # \]

  # Calculate the Total Weight Coefficient
  # Assuming 'sumCLoffWeight' is a column in 'bv' that represents total catch weight per group
  # If 'sumCLoffWeight' is not present, it should be computed or passed as an additional parameter
  if(!"sumCLoffWeight" %in% names(targetWeights)){
    stop("The column 'sumCLoffWeight' must be present in the data for calculating TWCoef.")
  }

  targetWeights$TWCoef <- targetWeights$sumCLoffWeight / targetWeights$WeightIndexSum
  # \[
  # \text{TWCoef} = \frac{C}{S}
  # \]

  # Calculate total weight per group
  targetWeights$totWeight <-  targetWeights$WeightIndex * targetWeights$TWCoef
  # \[
  # T_W = I_W \times \text{TWCoef}
  # \]

  # Calculate total catch number per group
  targetWeights$totNum <- targetWeights$totWeight / (targetWeights$WeightgMean / 1000)
  # \[
  # C_{num} = \frac{T_W}{\left( \frac{W_{mean}}{1000} \right)}
  # \]

  # Sanity check to ensure no rounding errors
  weights <- targetWeights$totNum * (targetWeights$WeightgMean / 1000)
  expected_sum <- sum(unique(targetWeights$sumCLoffWeight))

  if(!isTRUE(all.equal(sum(weights), expected_sum))) {
    stop("Strange problem: sums do not match within tolerance")
  }

  return(targetWeights)
}
