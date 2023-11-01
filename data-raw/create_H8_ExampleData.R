## Load the csv files from our data-raw
ddir <- "./data-raw/exampleData/"

H8Example <- readRDS(paste0(ddir, "H8_2023_10_31.rds"))
validateRDBESDataObject(H8Example)

# Save the data
usethis::use_data(H8Example, overwrite = TRUE)
