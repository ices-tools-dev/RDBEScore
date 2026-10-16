
# The table order per hierarchy is hard-coded, from the parent > child
# nesting in the ICES XSD files in data-raw/xsd (2022 and 2023 versions) and
# the CS data model (data-raw/dataFormat/RDBES Data Model CS.xlsx).
# A table without children in the XSD (a leaf, e.g. LE under SS in H7) comes
# right after its parent and is not a sampling unit.
# This will need updating if the order of the tables in the hierarchies changes.


myHierarchyTables <- list()
myHierarchyTables[["H1"]] <- c("DE", "SD", "VS" ,"FT" ,"FO" ,"SS" ,"SA" ,"FM" ,"BV")
myHierarchyTables[["H2"]] <- c("DE", "SD", "FT", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H3"]] <- c("DE", "SD", "TE", "VS", "FT", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H4"]] <- c("DE", "SD", "OS", "FT", "LE", "SS", "SA", "FM", "BV")
myHierarchyTables[["H5"]] <- c("DE", "SD", "OS", "LE", "FT", "SS", "SA", "FM", "BV")
myHierarchyTables[["H6"]] <- c("DE", "SD", "OS", "FT", "LE", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H7"]] <- c("DE", "SD", "OS", "SS", "LE", "SA", "FM", "BV")
myHierarchyTables[["H8"]] <- c("DE", "SD", "TE", "VS", "LE", "FT", "SS", "SA", "FM", "BV")
myHierarchyTables[["H9"]] <- c("DE", "SD", "LO", "TE", "SS", "SA" ,"LE" ,"FM", "BV")
myHierarchyTables[["H10"]] <- c("DE", "SD", "VS", "TE", "FT", "FO", "SS", "SA", "FM", "BV")
myHierarchyTables[["H11"]] <- c("DE", "SD", "LO", "TE", "FT", "LE", "SS", "SA", "FM", "BV")
myHierarchyTables[["H12"]] <- c("DE", "SD", "LO", "TE", "LE", "FT", "SS", "SA", "FM", "BV")
myHierarchyTables[["H13"]] <- c("DE", "SD", "FO", "FT", "SS", "SA", "FM", "BV")


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

# Set LE in H6 to be optional and not a sampling unit (a leaf under FT)
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H6" & myHierarchyTablesDF$table == "LE","optional"] <- TRUE
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H6" & myHierarchyTablesDF$table == "LE","samplingUnit"] <- FALSE

# Set FT in H8 to be optional and not a sampling unit (a leaf under LE).
# The XSD says FT is mandatory (minOccurs 1), but it is kept optional here
# so that createRDBESEstObject() does not change for H8
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H8" & myHierarchyTablesDF$table == "FT","optional"] <- TRUE
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H8" & myHierarchyTablesDF$table == "FT","samplingUnit"] <- FALSE

# Set LE in H7 to not be a sampling unit
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H7" & myHierarchyTablesDF$table == "LE","samplingUnit"] <- FALSE

# Set LE in H9 to not be a sampling unit
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H9" & myHierarchyTablesDF$table == "LE","samplingUnit"] <- FALSE

# Set LE in H11 to not be a sampling unit. It is mandatory and links FT to SS
# (SS.LEid in the CS data model). The 2022 XSD puts SS under FT instead
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H11" & myHierarchyTablesDF$table == "LE","samplingUnit"] <- FALSE

# Set FT in H12 to be optional and not a sampling unit (a leaf under LE)
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H12" & myHierarchyTablesDF$table == "FT","optional"] <- TRUE
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H12" & myHierarchyTablesDF$table == "FT","samplingUnit"] <- FALSE

# Set FT in H13 to be optional and not a sampling unit (a leaf under FO)
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H13" & myHierarchyTablesDF$table == "FT","optional"] <- TRUE
myHierarchyTablesDF[myHierarchyTablesDF$hierarchy == "H13" & myHierarchyTablesDF$table == "FT","samplingUnit"] <- FALSE


tablesInRDBESHierarchies <- myHierarchyTablesDF
usethis::use_data(tablesInRDBESHierarchies, overwrite = TRUE)
