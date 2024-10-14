## Load the csv files from our data-raw
ddir <- "./data-raw/exampleData/"

#this data is created by Richard Meitern (contact: richard.meitern@ut.ee) internally
H8ExampleEE1 <- readRDS(paste0(ddir, "H8_2023_10_31.rds"))
#update from version 1.19.18 to 1.19.26
H8ExampleEE1$CL$CLlanVal <- H8ExampleEE1$CL$CLtotOffLanVal
H8ExampleEE1$CL$CLtotOffLanVal <- NULL
validateRDBESDataObject(H8ExampleEE1)

# Save the data
usethis::use_data(H8ExampleEE1, overwrite = TRUE)
