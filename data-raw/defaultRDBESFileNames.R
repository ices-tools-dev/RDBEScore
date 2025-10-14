# Define the default file names from the RDBES download
DefaultFileNames <- list(
  "DE" = "Design",
  "SD" = "SamplingDetails",
  "VS" = "VesselSelection",
  "FT" = "FishingTrip",
  "FO" = "FishingOperation",
  "TE" = "TemporalEvent",
  "LO" = "Location",
  "OS" = "OnshoreEvent",
  "LE" = "LandingEvent",
  "SS" = "SpeciesSelection",
  "SA" = "Sample",
  "FM" = "FrequencyMeasure",
  "BV" = "BiologicalVariable",
  "VD" = "VesselDetails",
  "SL" = "SpeciesList",
  "IS" = "IndividualSpeciesInSpeciesList",
  "CL" = "CommercialLanding",
  "CE" = "CommercialEffort"
)

# Save the data
usethis::use_data(DefaultFileNames, overwrite = TRUE)
