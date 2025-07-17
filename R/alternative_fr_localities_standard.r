##
## Harmonizing the localities for the First Records database
##

### PREPARE WORKSPACE AND FILES ###

# Clean work space
graphics.off()
rm(list = ls())

# Call libraries
library(Hmisc)
library(stringr)
library(stringi)
library(data.table)
library(countrycode)
library(geonames)

# Import data (dynamic)
directory <- system("pwd", intern = TRUE)
raw_data <- file.path(directory, "data", "raw", "IntroData_raw.csv")
intro_data <- read.csv2(raw_data,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)
setDT(intro_data) # Convert to data.table

# Select relevant columns and delete empty rows
locality_temp <- intro_data[, .(Country)]
locality_temp[is.na(locality_temp)] <- "" # replace NAs with empty strings
locality <- locality_temp[Country != ""] # remove empty rows

# Store verbatim values in verbatimLocality and create a new columns
locality[, verbatimLocality := Country]
locality[, country_std := NA_character_]
locality[, locality_std := NA_character_]

sum(is.na(locality$locality_std))

# Procude pre-processed file
output_file <- file.path(
  directory,
  "tmp",
  "locality_pre-process.csv"
) # determine path
write.csv(locality,
  output_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
) # generate table

### HARMONIZE COUNTRIES AND LOCALITIES ###

# 1) Direct match with countrycode to standardize country names
locality[, locality_std := countrycode(
  Country,
  origin = "country.name",
  destination = "country.name.en",
  nomatch = NA,
  warn = TRUE
)]

sum(is.na(locality$locality_std)) # Check how many NAs

# Procude mid-processed file
output_file <- file.path(
  directory,
  "tmp",
  "locality_mid-process1.csv"
) # determine path
write.csv(locality,
  output_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
) # generate table

# 2) Fuzzy match for those not matched
unmatched <- locality[ # Function to get unmatched country names
  is.na(locality_std) &
    !is.na(Country),
  unique(Country)
]
valid_names <- unique(countrycode::codelist$country.name.en)

fuzzy_matches <- sapply(unmatched, function(x) {
  if (nchar(x) < 5) return(NA_character_)  # ignore very short entries

  dists <- stringdist::stringdist(tolower(x), tolower(valid_names), method = "jw")
  best_match <- valid_names[which.min(dists)]
  best_score <- min(dists, na.rm = TRUE)

  # only accept high-confidence matches
  if (best_score <= 0.15 && startsWith(tolower(best_match), substr(tolower(x), 1, 2))) {
    return(best_match)
  } else {
    return(NA_character_)
  }
}, USE.NAMES = TRUE)

sum(is.na(locality$locality_std)) # Check how many NAs

# Procude mid-processed file
output_file <- file.path(
  directory,
  "tmp",
  "locality_mid-process2.csv"
) # determine path
write.csv(locality,
  output_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
) # generate table

# 3) Custom locality mapping
custom_mapping_file <- file.path(
  directory, "data",
  "external",
  "custom_locality_mapping.csv"
)
custom_map <- fread(custom_mapping_file, sep = ",", encoding = "UTF-8")

# Match lowercased versions
locality[, Country_lc := tolower(Country)]
custom_map[, variant_lc := tolower(variant)]

# Add index from custom map
locality[, custom_idx := match(Country_lc, custom_map$variant_lc)]

# Apply harmonized values using that index
locality[!is.na(custom_idx), locality_std := fifelse(
  !is.na(custom_map$locality_standard[custom_idx]),
  custom_map$locality_standard[custom_idx],
  fifelse(
    !is.na(custom_map$country_standard[custom_idx]),
    custom_map$country_standard[custom_idx],
    locality_std
  )
)]

# Assign country_std from custom map only if NA
locality[
  !is.na(custom_idx) & is.na(country_std),
  country_std := custom_map$country_standard[custom_idx]
]

# Clean up
locality[, c("Country_lc", "custom_idx") := NULL]

sum(is.na(locality$locality_std)) # Check how many NAs
# Procude mid-processed file
output_file <- file.path(
  directory,
  "tmp",
  "locality_mid-process3.csv"
) # determine path
write.csv(locality,
  output_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
) # generate table

# Fill in Country_std
locality[is.na(country_std) & !is.na(locality_std), country_std := locality_std]
# Get valid country names with ISO codes (i.e., sovereign countries)
valid_countries <- unique(
  na.omit(countrycode::codelist$country.name.en[
    !is.na(countrycode::codelist$iso3c)
  ])
)

# Keep only matches to official countries
locality[!country_std %in% valid_countries, country_std := NA]

# Ensure locality_std is always filled if country_std is present
locality[is.na(locality_std) & !is.na(country_std), locality_std := country_std]

# Exceptions for specific cases — overwrite if regex pattern is matched
exceptions_locality <- list(
  "Falkland" = "Sub-Antarctic territories",
  "East Antarctica" = "Antarctica"
)

# Overwrite locality_std and set country_std to NA for exceptions except Congo
for (pattern in names(exceptions_locality)) {
  locality[grepl(pattern, Country, ignore.case = TRUE), `:=`(
    locality_std = exceptions_locality[[pattern]],
    country_std = NA_character_
  )]
}

# For Congo, overwrite both country_std and locality_std
locality[grepl("Congo", Country, ignore.case = TRUE), `:=`(
  country_std = "Democratic Republic of the Congo",
  locality_std = "Democratic Republic of the Congo"
)]

# Write harmonized output
output_file <- file.path(directory, "tmp", "locality_harmonized.csv")
write.csv(locality, output_file, row.names = FALSE, fileEncoding = "UTF-8")



### CHECK ###
# Unique harmonized output
matched_unique <- unique(locality[, .(locality_std)])
output_file <- file.path(directory, "outputs", "locality_matched.csv")
write.csv(matched_unique,
  output_file,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
# Filter rows where harmonization (incl. fuzzy match) failed
locality_unmatched <- locality[is.na(country_std)]
# Optional: Keep only unique unmatched names for review
unmatched_unique <- unique(locality_unmatched[, .(verbatimLocality)])
# Define output path
unmatched_output <- file.path(directory, "tmp", "locality_unmatched.csv")
# Write to CSV
write.csv(unmatched_unique,
  unmatched_output,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
