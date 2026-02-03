#' Get Field from Upper Table
#'
#' This function gets a field from an upper table using a list of tables for any
#' row. It takes four arguments: `field`, `values`, `tbls`, and `level`. The
#' function checks if the start of the provided field matches the provided level.
#' If it does, the function returns the rows from the corresponding table in the
#' provided list of tables that have the provided values for the specified field.
#' Otherwise, the function finds the previous table in the list of tables and
#' gets the values for its `id` column that match the provided values for the
#' specified field. The function then calls itself recursively with these new
#' values and returns the result.
#' NB! running on the RDBESDataObject will work properly if it is sorted by default the tables are not
#' in the correct order for a spesific hierarchy.
#'
#' @param field A character string specifying the field to get.
#' @param values A vector of values to match for the specified field.
#' @param tbls A named list of data frames containing tables at different levels.
#' @param level A character string specifying the level to get data from.
#' @param verbose (Optional) Set to TRUE if you want informative text printed
#'
#' @return A data frame containing rows from an upper table that match the
#'  provided values for the specified field.
#'
#' @examples
#' # it is important to run these functions only on sorted RDBESDataObject
#' RDBEScore:::upperTblData("SAid", c(1), sort(H8ExampleEE1), "DE", verbose = TRUE)
#' DE <- data.table::data.table(DEid = c(1, 2))
#' SD <- data.table::data.table(SDid = c(1, 2), DEid = c(1, 2))
#' VS <- data.table::data.table(VSid = c(1, 2),SDid =c(1,2),  value = c(10, 20))
#' tbls <- list(DE = DE, SD = SD, VS = VS)
#' RDBEScore:::upperTblData("VSid", c(1), tbls, "DE")
#' @keywords internal
upperTblData <- function(field, values, tbls, level, verbose = FALSE){
  #check if tables are of correct type
  if(!is.list(tbls)) stop("tbls must be a list")
  if(!level %in% names(tbls)) stop(level, " must be a character string in the names of the tables")

  # Validate that all non-NULL tables are data.frames
  for(i in seq_along(tbls)){
    if(!is.null(tbls[[i]]) && !is.data.frame(tbls[[i]])){
      # Try to access field to generate the expected error message
      get(field)
    }
  }

  start <- substr(field, start=1, stop=2)
  if(start == level){
    res <- tbls[[level]]
    return(res[get(field) %in% values,])
  }
  tbl <- tbls[[start]]
  currTbl <- which(start == names(tbls))
  if(verbose){
    print(paste0(start,": ", paste0(values, collapse = ", ")))
  }
  #skip NULL tables
  tc <- -1
  if((currTbl+tc) < 1) stop("No table found")
  prevTbl <- names(tbls)[currTbl+tc]

  while(is.null(tbls[[prevTbl]])){
    if(verbose){
      print(paste0("Skipping NULL table: ", prevTbl))
    }
    tc <- tc - 1
    if((currTbl+tc) < 1) stop("No table found")
    prevTbl <- names(tbls)[currTbl+tc]
  }

  # Now check if prevTbl is empty (0 rows) OR if prevTblfield doesn't exist in current table
  prevTblfield <-  paste0(names(tbls)[currTbl+tc], "id")

  # Check if we need to bypass this table (either empty or field doesn't exist in current table)
  needs_bypass <- FALSE
  if(!is.null(tbls[[prevTbl]]) && is.data.frame(tbls[[prevTbl]]) && nrow(tbls[[prevTbl]]) == 0){
    needs_bypass <- TRUE
  } else if(!prevTblfield %in% colnames(tbl)){
    needs_bypass <- TRUE
  }

  if(needs_bypass){
    # Try to find a table we can bypass to
    tc_bypass <- tc - 1
    found_bypass <- FALSE

    while((currTbl + tc_bypass) >= 1){
      bypass_tbl_name <- names(tbls)[currTbl + tc_bypass]
      bypass_tbl <- tbls[[bypass_tbl_name]]
      bypass_field <- paste0(bypass_tbl_name, "id")

      if(!is.null(bypass_tbl) && is.data.frame(bypass_tbl)){
        # Check if current table has this field
        if(bypass_field %in% colnames(tbl)){
          # Check if this table is non-empty OR if we're at the target level
          if(nrow(bypass_tbl) > 0 || bypass_tbl_name == level){
            found_bypass <- TRUE
            break
          }
        }
      }
      tc_bypass <- tc_bypass - 1
    }

    if(found_bypass){
      # Bypass to the found table
      if(verbose){
        print(paste0("Bypassing ", prevTbl, " to ", names(tbls)[currTbl + tc_bypass]))
      }
      tc <- tc_bypass
      prevTbl <- names(tbls)[currTbl + tc]
      prevTblfield <- paste0(prevTbl, "id")
    } else {
      # No bypass possible - return empty result
      if(verbose){
        print(paste0("Cannot bypass ", prevTbl, " - returning empty"))
      }
      return(tbls[[level]][0,])
    }
  }

  prevTblvalues <- tbl[get(field) %in% values, get(prevTblfield)]

  # Check if all values are NA (no actual link to this table) or if there are no values
  if(length(prevTblvalues) == 0 || all(is.na(prevTblvalues))){
    # No matches or all NAs - check if we can bypass this table
    if(!is.null(tbl) && is.data.frame(tbl)){
      # Look for next non-NULL, non-empty table going backwards
      tc_bypass <- tc - 1
      found_bypass <- FALSE

      while((currTbl + tc_bypass) >= 1){
        bypass_tbl_name <- names(tbls)[currTbl + tc_bypass]
        bypass_tbl <- tbls[[bypass_tbl_name]]
        bypass_field <- paste0(bypass_tbl_name, "id")

        if(!is.null(bypass_tbl) && is.data.frame(bypass_tbl) && nrow(bypass_tbl) > 0){
          # Found a non-empty table, check if current table has its field
          if(bypass_field %in% colnames(tbl)){
            found_bypass <- TRUE
          }
          break
        }
        tc_bypass <- tc_bypass - 1
      }

      if(found_bypass){
        # Bypass the table with no matches or all NAs
        if(verbose){
          if(length(prevTblvalues) == 0){
            print(paste0("No matches in table ", prevTbl, ", bypassing"))
          } else {
            print(paste0("All NAs in table ", prevTbl, ", bypassing"))
          }
        }
        # When bypassing, directly recurse without updating local variables
        # to avoid issues with table subsetting
        bypass_prevTbl <- names(tbls)[currTbl + tc_bypass]
        bypass_prevTblfield <- paste0(bypass_prevTbl, "id")
        bypass_prevTblvalues <- tbl[get(field) %in% values, get(bypass_prevTblfield)]
        return(upperTblData(bypass_prevTblfield, bypass_prevTblvalues, tbls, level, verbose))
      }
      # If no bypass found, continue with empty prevTblvalues
    }
  }

  upperTblData(prevTblfield,prevTblvalues, tbls, level, verbose)
}
