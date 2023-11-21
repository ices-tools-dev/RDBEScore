
# Get the list of aphiaids from the ICES vocabulary
#codeTypes <- icesVocab::getCodeTypeList()
icesSpecWoRMS <- icesVocab::getCodeList(code_type = "SpecWoRMS")

# Change the key to a char (so it is easier to join to the RDBES speCode field)
icesSpecWoRMS$Key <- as.character(icesSpecWoRMS$Key)

# Add a date field
icesSpecWoRMS$DateDownloaded <- Sys.Date()

# Save the data
usethis::use_data(icesSpecWoRMS, overwrite = TRUE)

