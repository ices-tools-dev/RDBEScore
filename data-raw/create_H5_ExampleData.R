## Load the csv files from our data-raw
ddir <- "./data-raw/exampleData/"

H5Example <- createRDBESDataObject(paste0(ddir, "H5_2023_10_16.zip"),
                                       castToCorrectDataTypes = TRUE)
validateRDBESDataObject(H5Example)

# Save the data
usethis::use_data(H5Example, overwrite = TRUE)
