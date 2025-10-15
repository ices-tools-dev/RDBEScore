## Load the csv files from our data-raw
ddir <- "./data-raw/exampleData/"

#this data is created by Richard Meitern (contact: richard.meitern@ut.ee)
# internally it resembles Estonian onshore sampling data
H7Example <- readRDS(paste0(ddir, "H7_2025_10_13.rds"))

H7Example$VD <- NULL
H7Example$DE$DEsampScheme <- gsub("EST_","", H7Example$DE$DEsampScheme)
H7Example$SL <- H7Example$SL[H7Example$SL$SLspeclistName %in%
                               unique(H7Example$SS$SSspecListName),]

H7Example$IS <- H7Example$IS[H7Example$IS$SLid %in% H7Example$SL$SLid,]
#add 0 row VD
H7Example$VD <- data.table::copy(H1Example$VD[F,])


RDBEScore::validateRDBESDataObject(H7Example)
# Save the data
usethis::use_data(H7Example, overwrite = TRUE)
