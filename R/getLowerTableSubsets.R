#' Get Lower Table from Several Upper Table Fields
#'
#' This function takes a list of subsets, a target lower level table name, and a list of tables.
#' It returns a unique data frame containing the rows of the target lower level table that are associated with
#' the given values of the upper table field in each subset. The function can also add the subset values to
#' the result for reference.
#'
#' @param subsets A named list of vectors. Each vector contains values for a specific upper table field.
#' @param tblName A character string specifying the name of the target lower level table.
#' @param rdbesTables A RDBESData object containing the tables.
#' @param combineStrata A logical value indicating whether to include the strata information in the result.
#'   If `TRUE`, and if any strata has more than one value, those values are collapsed into a single string
#'   and appended to the result with a warning.
#' @param verbose A logical value indicating whether to print informative text.
#'
#' @return A unique data frame containing the rows of the target lower level table that are associated with
#'   the given values of the upper table field in each subset. If `combineStrata = TRUE`, the result will also include
#'   a column for each subset with the corresponding collapsed values.
#'
#' @details The function recursively intersects the rows of the target lower level table that match
#' the values from each subset in the upper tables. It then ensures that only unique rows are returned,
#' based on the ID column of the target table.
#'
getLowerTableSubsets <- function(subsets, tblName, rdbesTables, combineStrata = TRUE, verbose = FALSE) {
  res <- list()
  # Process each subset
  for(ss in names(subsets)){
    res[[ss]] <- getLinkedDataFromLevel(ss, subsets[[ss]], rdbesTables, tblName, verbose)
  }

  # Function to get the intersection of IDs
  intesectIDs <- function(x, y, tblName){
    idCol <- paste0(tblName, "id")
    if(is.data.frame(x)){
      start <- x[[idCol]]
    } else {
      start <- x
    }
    intersect(start, y[[idCol]])
  }

  # Get the IDs from the first subset if only one subset is present
  if(length(res) == 1) {
    ids <- res[[1]][[paste0(tblName, "id")]]
  } else{
    ids <- Reduce(function(x, y) {intesectIDs(x, y, tblName)}, res)
  }

  # Bind the data together and filter based on intersected IDs
  res <- data.table::rbindlist(res, fill = TRUE)
  res <- res[get(paste0(tblName, "id")) %in% ids]
  res <- unique(res, by = paste0(tblName, "id"))

  if(combineStrata) {
    # Collapse multiple strata values into a single string if combineStrata = TRUE
    lengths <- sapply(subsets, length)
    if(any(lengths > 1)){
      subsets[lengths > 1] <- sapply(subsets[lengths > 1], paste0, collapse = "|")
      items <- subsets[lengths > 1]
      for(i in 1:length(items)){
        warning(names(items[i]),
                " is collapsed in the result into: \"",items[i],"\"\n")
      }
    }
  } else {
   stop("combineStrata must be TRUE for now")
  }

  # Combine the data with the strata subsets
  res <- cbind(res, as.data.frame(subsets))

  return(res)
}

