## Load the csv files from our data-raw
ddir <- "./data-raw/exampleData/"

dataDate <- "2025_02_11"

outputDir <- paste0("./tests/testthat/h1_v_",gsub("_","",dataDate),"/")

H1_Example <- createRDBESDataObject(paste0(ddir, "H1_",dataDate,".zip"),
                                       castToCorrectDataTypes = TRUE)
validateRDBESDataObject(H1_Example)

#copy the zip file to tests
file.copy(paste0(ddir, "H1_",dataDate,".zip"),
          paste0(outputDir, "H1_Example.zip"), overwrite = TRUE)

#saveRDS(H1_Example, file=paste0(outputDir,"H1_Example.rds"))


H1Example <- H1_Example
# Save the data
usethis::use_data(H1Example, overwrite = TRUE)

# make a different zip file according to the changed download format

# Generates random number for the temp import folder name
randInt <- paste0(sample(1:100, 3), collapse = "")
tmp <- paste0(tempdir(), "/downloadImport", randInt)
dir.create(tmp)
all_unzipped <- c()
unzipFile <- function(x, tmp) {

  if (!file.exists(x)) {
    return()
  }

  if (RDBEScore:::is.zip(x)) {
    unzipped <- utils::unzip(x, exdir= tmp)
    unzipped <- basename(unzipped)
    unzipped <- unzipped[grep("*.csv", unzipped)]
    intersected <- intersect(unzipped, all_unzipped)
    if(length(intersected) != 0) {
      warning(paste0("Duplicate unzipped files detected:\n",  paste0("\t", intersected, "\n", collapse="\n")))
    }
    all_unzipped <<- c(all_unzipped, unzipped)
    return(unzipped)
  }

}

# the files are not used currently but can be if we want to
files <- unzipFile(paste0(ddir, "H1_",dataDate,".zip"), tmp)

#make folders in tmp
dir.create(paste0(tmp, "/H1"))
dir.create(paste0(tmp, "/HVD"))
dir.create(paste0(tmp, "/HSL"))
#put files to the folders
hsl_file <- "SpeciesList.csv"
his_file <- "IndividualSpecies.csv"
hvd_file <- "VesselDetails.csv"
file.copy(paste0(tmp, "/", hsl_file), paste0(tmp, "/HSL"), overwrite = TRUE)
file.copy(paste0(tmp, "/", his_file), paste0(tmp, "/HSL"), overwrite = TRUE)
file.copy(paste0(tmp, "/", hvd_file), paste0(tmp, "/HVD"), overwrite = TRUE)
#all others files
h1_files <- setdiff(files, c(hsl_file, hvd_file, his_file))
for (file in h1_files) {
  file.copy(paste0(tmp, "/", file), paste0(tmp, "/H1"), overwrite = TRUE)
}
#remove all csv files from the tmp folder
unlink(paste0(tmp, "/", files), recursive = F)



#zip the temp so it has the same format as the download
# Zip the 'H1', 'HVD', and 'HSL' folders without including the 'tmp' directory

#define the name and path of the final ZIP file
final_zip <- paste0(getwd(),file.path(outputDir, "H1_Example_fd.zip"))

zip::zipr(zipfile = final_zip,
          files = c("H1", "HVD", "HSL", "Disclaimer.txt"),
          root = tmp)

#make a new Directory in tmp called H2 and zip it
final_zip <- paste0(getwd(),file.path(outputDir, "H1_H2_Example_fd.zip"))
dir.create(paste0(tmp, "/H2"))
zip::zipr(zipfile = final_zip,
          files = c("H1","H2", "HVD", "HSL", "Disclaimer.txt"),
          root = tmp)


# remove the temp folder
unlink(tmp, recursive = T)

