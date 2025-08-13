library(openxlsx)
library(data.table)
library(tidyverse)

fr_localities_standard <- function(dat, save_to_disk = FALSE){

# Load location table

regions <- read.xlsx("data/config/AllLocations.xlsx", sheet = 2, na.strings = "")
regions <- regions[, c("locationID", "location", "location_var")]
regions$location_var <- tolower(regions$location_var)  # Set all to lowercase for matching
regions$location_lower <- tolower(regions$location)  # Set all to lowercase for matching

subregions <- read.xlsx("data/config/AllLocations.xlsx", sheet = 3, na.strings = "")
subregions <- subregions[, c("locationID", "location", "location_var", "gadm1_name", "gadm1_var")]
subregions$gadm1_var <- tolower(subregions$gadm1_var)  # Set all to lowercase for matching
subregions$Gadm1_lower <- tolower(subregions$gadm1_name)  # Set all to lowercase for matching

# Get duplicated names of subregions  
dup <- unique(gsub("\\s*\\(.*?\\)", "", subregions$gadm1_name)[duplicated(gsub("\\s*\\(.*?\\)", "", subregions$gadm1_name))])
  
#  dat <- fread("data/tmp/fr_main_dataset_step3.csv")
  dat <- rename(dat, "location_orig" = "locality")
  dat <- select(dat, -locationID)
  
  # Prepare for matching with regions
  dat_match1 <- dat ## use another dat set for region matching to keep the original names
  dat_match1$order <- 1:nrow(dat_match1)
  dat_match1$location_orig <- gsub("\\xa0|\\xc2", " ", dat_match1$location_orig)  # Replace special spaces
  dat_match1$location_orig <- gsub("^\\s+|\\s+$", "", dat_match1$location_orig)  # Trim leading/trailing whitespace
  dat_match1$location_orig <- gsub("  ", " ", dat_match1$location_orig)  # Replace double spaces
  dat_match1$location_orig <- gsub(" \\(the\\)", "", dat_match1$location_orig)  # Remove " (the)"
  dat_match1$location_lower <- tolower(dat_match1$location_orig)  # Lowercase for matching
  
  ## Step 1: Match names of 'dat' with region names of 'regions' and 'subregions' 
  dat_match_regions <- merge(dat_match1, regions, by.x = "location_lower", by.y = "location_lower", all.x = TRUE)
  dat_match_subregions <- merge(dat_match1, subregions, by.x = "location_lower", by.y = "Gadm1_lower", all.x = TRUE)
  
  ## Step 2a: Match based on keywords in 'regions' - after "location_var" column
  ind_keys_regions <- which(!is.na(regions$location_var))
  for (j in ind_keys_regions) {  # loop over rows with multiple country name variations
    location_var <- unlist(strsplit(regions$location_var[j], "; ")) # check if multiple country name variations provided
    for (k in location_var) {
      ind_match <- which(dat_match_regions$location_lower == k)
      if (length(unique(regions$location[j])) > 1) 
        cat(paste0("Warning: ", k, " matches multiple location names. Refine location_var!"))
      
      dat_match_regions$location[ind_match] <- regions$location[j]
      dat_match_regions$locationID[ind_match] <- regions$locationID[j]
    }
  }
  
  ## Step 2b: Match based on keywords in 'subregions' - after "gadm1_var" column
  ind_keys_subregions <- which(!is.na(subregions$gadm1_var))
  for (j in ind_keys_subregions) {  # loop over rows with multiple subregion name variations
    gadm1_var <- unlist(strsplit(subregions$gadm1_var[j], "; ")) # check if multiple subregion name variations provided
    for (k in gadm1_var) {
      ind_match <- which(dat_match_subregions$location_lower == k)
      if (length(unique(subregions$gadm1_name[j])) > 1) 
        cat(paste0("Warning: ", k, " matches multiple location names. Refine gadm1_var!"))
      
      dat_match_subregions$gadm1_name[ind_match] <- subregions$gadm1_name[j]
      dat_match_subregions$location[ind_match] <- subregions$location[j]
      dat_match_subregions$locationID[ind_match] <- subregions$locationID[j]
    }
  }
  
  ## Merge both data frames with standardized locations names ('region' and 'subregion')
  dat_match1 <- full_join(dat_match_subregions, 
                          dat_match_regions |> select(order, locationID, location), 
                          by="order") |> 
    mutate(locationID = coalesce(locationID.x, locationID.y),
           location = coalesce(location.x, location.y))|> 
    select(-locationID.x, -locationID.y, -location.x, -location.y, -location_var, -gadm1_var)
  
  
  ## final merging of both data sets with standardized region names to original data
  dat_match1 <- dat_match1[order(dat_match1$order),]
  if (!identical(dat_match1$taxon_orig,dat$taxon_orig)) stop("Data sets not sorted equally!")
  
  dat$locationID <- dat_match1$locationID
  dat$location <- dat_match1$location
  dat$stateProvince <- dat_match1$gadm1_name
  
  ## Check if any locations in the original dataframe correspond to duplicated locations in the world. 
  if (any(dat$location_orig %in% dup)) {
    # Extract the matching names from dat$location_orig
    matching_names <- unique(dat$location_orig[dat$location_orig %in% dup])
    warning(paste(
      "\n    Warning: Unresolved terms in file.The following location name(s) correspond to multiple subregions in the world:",
      paste(matching_names, collapse = ", "),
      ". Please modify the original location name(s) by including the country name in parentheses(), and try again (e.g: Amazonas (Colombia)) \n"
    ))
  }
  
  #dat_regnames <- merge(dat,regions[,c("locationID","location")],by.x="region",by.y="location",all.x=T)
  dat_regnames <- dat
  
  ## Remove duplicated entries
  dat_regnames <- dat_regnames[!duplicated(dat_regnames), ]
  write_regnames <- dat_regnames
  
  # Clean locations and locationID 
  write_regnames <- write_regnames |> 
    select(-c(stateProvince, locationID)) |> 
    left_join(regions |> select(location, locationID), by = "location")
  
  
  ## output ###############################################################################
  
  # Output: Save the file with standardized location names
  fr_main_dataset_step4 <- write_regnames[, c("locationID", "location", "taxonID", "taxon",
                                       "habitat",	"firstRecordEvent",	"verbatimFirstRecordEvent", 
                                       "confidenceFirstRecordEvent",	"occurrenceStatus",	"establishmentMeans",
                                       "degreeOfEstablishment", "pathway",	"datasetName",	"bibliographicCitation",	
                                       "accessRights"
  )]
  
  if (save_to_disk == TRUE){
    as.data.table(fr_main_dataset_step4, "data/tmp/fr_main_dataset_4.csv")
  
  # Check and export missing locations
  missing <- dat_regnames$location_orig[is.na(dat_regnames$locationID)]
  if (length(missing) > 0) {
    write.table(sort(unique(missing)),"data/tmp/check_missing_locations.csv" ,row.names = F,col.names=F)
  }
  }
  return(fr_main_dataset_step4)
}