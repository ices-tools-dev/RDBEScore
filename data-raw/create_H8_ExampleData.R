## Load the csv files from our data-raw
ddir <- "./data-raw/exampleData/"

#this data is created by Richard Meitern (contact: richard.meitern@ut.ee) internally
H8ExampleEE1 <- readRDS(paste0(ddir, "H8_2025_02_11.rds"))
RDBEScore::validateRDBESDataObject(H8ExampleEE1)

# Save the data
usethis::use_data(H8ExampleEE1, overwrite = TRUE)
