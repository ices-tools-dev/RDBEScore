
# 16/10/23 - Easier to hard-code the table order - it doesn't change much
# and my code to read the xsd files wasn't interpreting them correctly.
# This will need updating if the order of the tables in the hierarchies changes.


myHierarchyTables <- list()
myHierarchyTables[["H1"]] <- c("DE", "SD", "VS" ,"FT" ,"FO" ,"SS" ,"SA" ,"FM" ,"BV")
myHierarchyTables[["H2"]] <- c("DE", "SD", "FT", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H3"]] <- c("DE", "SD", "TE", "VS", "FT", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H4"]] <- c("DE", "SD", "OS", "FT", "LE", "SS", "SA", "FM", "BV")
#myHierarchyTables[["H5"]] <- c("DE", "SD", "OS", "FT", "LE", "SS", "SA", "FM", "BV")
myHierarchyTables[["H5"]] <- c("DE", "SD", "OS", "LE", "FT", "SS", "SA", "FM", "BV")
myHierarchyTables[["H6"]] <- c("DE", "SD", "OS", "FT", "FO", "LE", "SS", "SA", "FM", "BV")
#myHierarchyTables[["H7"]] <- c("DE", "SD", "OS", "LE", "SS", "SA", "FM", "BV")
myHierarchyTables[["H7"]]<- c("DE", "SD", "OS", "SS", "LE", "SA", "FM", "BV")
myHierarchyTables[["H8"]] <- c("DE", "SD", "TE", "VS", "FT", "LE", "SS", "SA", "FM", "BV")
#myHierarchyTables[["H9"]] <- c("DE", "SD", "LO", "TE", "SS", "LE", "SA", "FM", "BV")
myHierarchyTables[["H9"]] <- c("DE", "SD", "LO", "TE", "SS", "SA" ,"LE" ,"FM", "BV")
myHierarchyTables[["H10"]] <- c("DE", "SD", "VS", "TE", "FT", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H11"]] <- c("DE", "SD", "LO", "TE", "FT", "LE", "SS", "SA", "FM", "BV")
#myHierarchyTables[["H12"]] <- c("DE", "SD", "LO", "TE", "FT", "LE", "SS", "SA", "FM", "BV")
myHierarchyTables[["H12"]] <- c("DE", "SD", "LO", "TE", "LE", "SS", "SA", "FM", "BV") # CHECK
#myHierarchyTables[["H13"]] <- c("DE", "SD", "FT", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H13"]] <- c("DE", "SD", "FO", "SS", "SA", "FM", "BV") # CHECK


# Add on some values describing the table
myHierarchyTablesDF <-
  lapply(myHierarchyTables,
       function(x){
         data.frame(table = x,
                    lowerHierarchy = FALSE,
                    optional = FALSE,
                    samplingUnit = TRUE,
                    sortOrder = seq(1:length(x)))
  })

# Combine into a single data frame
myHierarchyTablesDF <- data.table::rbindlist(myHierarchyTablesDF,idcol=TRUE)
names(myHierarchyTablesDF)[names(myHierarchyTablesDF) == ".id"] <- "hierarchy"

# Set FM and BV to lower hierarchy
myHierarchyTablesDF[myHierarchyTablesDF$table %in% c("FM","BV"),"lowerHierarchy"] <- TRUE

## Make some specifc changes for hierarchies with option tables and tables that aren't a sampling unit

# Set FT in H5 to be optional and not a sampling unit
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H5" & myHierarchyTablesDF$table == "FT","optional"] <- TRUE
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H5" & myHierarchyTablesDF$table == "FT","samplingUnit"] <- FALSE

# Set FT in H8 to be optional and not a sampling unit
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H8" & myHierarchyTablesDF$table == "FT","optional"] <- TRUE
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H8" & myHierarchyTablesDF$table == "FT","samplingUnit"] <- FALSE

# Set LE in H7 to not be a sampling unit
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H7" & myHierarchyTablesDF$table == "LE","samplingUnit"] <- FALSE

# Set LE in H9 to not be a sampling unit
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H9" & myHierarchyTablesDF$table == "LE","samplingUnit"] <- FALSE


tablesInRDBESHierarchies <- myHierarchyTablesDF
usethis::use_data(tablesInRDBESHierarchies, overwrite = TRUE)
