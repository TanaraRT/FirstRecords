
##########################################################################
##                                                                      ##
##                        FIRST RECORDS WORKFLOW                        ##
##               Step 4: Standardisation of location names              ##
##             using a translation table "AllLocations.xlsx"            ##
##                                                                      ##
## M. Gómez-Suárez, H. Seebens, T. Renard Truong                        ##
## vx.x, 2025                                                           ##
##########################################################################


fr_localities_standard <- function(dataset, save_to_disk = FALSE){

  if (is.null(dataset) || !is.data.frame(dataset)) {
    stop("Invalid input: dataset must be a data.frame or data.table")
  }
  
  ## load locations reference table
  reference_locations <- read.xlsx("data/config/AllLocations.xlsx") 
  setDT(reference_locations)
  reference_locations_unique <- reference_locations[!duplicated(location)]
  reference_locations_unique <-  reference_locations_unique[, location := tolower(location)]
  reference_locations_unique <-  reference_locations_unique[, gadm1_name := tolower(location)]
  fwrite (reference_locations_unique, "data/tmp/reference_locations_unique.csv")

  # Get duplicated names of subregions  
  dup <- unique(gsub("\\s*\\(.*?\\)", "", reference_locations$gadm1_name)[duplicated(gsub("\\s*\\(.*?\\)", "", reference_locations$gadm1_name))])
  
  # prepare a copy of dataset for matching with regions
  dataset_copy <- copy(dataset) # copy
  dataset_copy$locality <- gsub("\\xa0|\\xc2", " ", dataset_copy$locality)  # Replace special spaces
  dataset_copy$locality <- gsub("^\\s+|\\s+$", "", dataset_copy$locality)  # Trim leading/trailing whitespace
  dataset_copy$locality <- gsub("  ", " ", dataset_copy$locality)  # Replace double spaces
  dataset_copy$locality <- gsub(" \\(the\\)", "", dataset_copy$locality)  # Remove " (the)"
  setDT(dataset_copy)  
  dataset_copy$order <- 1:nrow(dataset_copy)
  dataset_copy$locality <- tolower(dataset_copy$locality)  # Lowercase for matching
  
  ## Step 1a: Match localities of dataset with reference locations in dataset_match1
  dataset_match1 <- merge(dataset_copy, reference_locations_unique, by.x = "locality", by.y = "location", all.x = TRUE)
  dataset_match1[, locationID := locationID.y]
  dataset_match1[, locationID.y := NULL]
  dataset_match1[, locationID.x := NULL]
  
  ## Step 1b: Match localities of 'dataset' with reference gadm1_name in dataset_match2
  dataset_match2 <- merge(dataset_copy, reference_locations_unique, by.x = "locality", by.y = "gadm1_name", all.x = TRUE)
  dataset_match2[, locationID := locationID.y]
  dataset_match2[, locationID.y := NULL]
  dataset_match2[, locationID.x := NULL]
  
  ## Step 2a: Match localities of 'dataset' with reference location keywords (after "location_var" column) in dataset_match1
  ind_keys_regions <- which(!is.na(reference_locations_unique$location_var))
  for (j in ind_keys_regions) {  # loop over rows with multiple country name variations
    location_var <- unlist(strsplit(reference_locations_unique$location_var[j], "; ")) # check if multiple country name variations provided
    for (k in location_var) {
      ind_match <- which(dataset_match1$locality == k)
      if (length(unique(reference_locations_unique$location[j])) > 1) 
        cat(paste0("⚠ Warning: ", k, " matches multiple location names. Refine location_var!"))
      
      dataset_match1$locality[ind_match] <- reference_locations_unique$location[j]
      dataset_match1$locationID[ind_match] <- reference_locations_unique$locationID[j]
    }
  }
  
  ## Step 2b: Match localities of 'dataset' with reference gadm1_name keywords (after "gadm1_var" column) in dataset_match2
  ind_keys_subregions <- which(!is.na(reference_locations_unique$gadm1_var))
  for (j in ind_keys_subregions) {  # loop over rows with multiple subregion name variations
    gadm1_var <- unlist(strsplit(reference_locations_unique$gadm1_var[j], "; ")) # check if multiple subregion name variations provided
    for (k in gadm1_var) {
      ind_match <- which(dataset_match2$Location_lower == k)
      if (length(unique(reference_locations_unique$gadm1_name[j])) > 1) 
        cat(paste0("⚠ Warning: ", k, " matches multiple location names. Refine gadm1_var!"))
      
      dataset_match2$gadm1_name[ind_match] <- reference_locations_unique$gadm1_name[j]
      dataset_match2$locality[ind_match] <- reference_locations_unique$location[j]
      dataset_match2$locationID[ind_match] <- reference_locations_unique$locationID[j]
    }
  }
  
  ## Merge both data frames with standardized locations names ('dataset_match1' and 'dataset_match2')
  dataset_match3 <- full_join(dataset_match2, 
                              dataset_match1 |> select(order, locationID, locality), 
                          by="order") |> 
    mutate(locationID = coalesce(locationID.x, locationID.y),
           locality = coalesce(locality.x, locality.y))|> 
    select(-locationID.x, -locationID.y, -locality.x, -locality.y, -location_var, -gadm1_var)

  
  ## final merging of both data sets with standardized region names to original data
  dataset_match3 <- dataset_match3[order(dataset_match3$order),]
  if (!identical(dataset_match3$taxon,dataset$taxon)) stop("Datasets not sorted equally!")
  
  dataset$locationID <- dataset_match3$locationID
  dataset$country <- dataset_match3$locality
  dataset$locality <- dataset_match3$gadm1_name
  dataset$region <- dataset_match3$sinas_region
  
  fr_main_dataset_step4 <- dataset[, c("locationID", "country", "taxonID", "taxon",
                                            "habitat",	"firstRecordEvent",	"verbatimFirstRecordEvent", 
                                            "confidenceFirstRecordEvent",	"occurrenceStatus",	"establishmentMeans",
                                            "degreeOfEstablishment", "pathway",	"datasetName",	"bibliographicCitation",	
                                            "accessRights"
  )]
  
  if (save_to_disk == TRUE){
    fwrite(fr_main_dataset_step4, "data/tmp/fr_main_dataset_4.csv")
  }
  
  ## Check if any locations in the original dataframe correspond to duplicated locations in the world. 
  if (any(dataset$verbatimLocation %in% dup)) {
    # Extract the matching names from dat$Location_orig
    matching_names <- unique(dataset$verbatimLocation[dataset$verbatimLocation %in% dup])
    warning(paste(
      "\n    ⚠ Warning: Unresolved terms in the dataset.The following location name(s) correspond to multiple subregions in the world:",
      paste(matching_names, collapse = ", "),
      ". Please modify the original location name(s) by including the country name in parentheses(), and try again (e.g: Amazonas (Colombia). ) \n"
    ))
  }
  
  # Produce location table
  location_table <- dataset[, .(locationID, locality, country, verbatimLocation, region)]
  location_table <- location_table[!is.na(locationID)]
#  location_table <- location_table[!rowSums(is.na(location_table)) == ncol(location_table)]
  setorder(location_table, -locationID)
  location_table <- unique(location_table)  # Remove duplicated entries
  fwrite(location_table, "data/outputs/location_table.csv")
  
  # Check and export missing locations
  missing <- dataset[is.na(locationID), .(locality, country, verbatimLocation)]
#  if (length(missing) > 0) {
#    fwrite(data.table(missingLocation = sort(unique(missing))), "data/tmp/missing_Locations.csv")
#  }
  
  if (nrow(missing) > 0) {
    fwrite(
      data.table(missingLocation = sort(unique(missing$verbatimLocation))),
      "data/tmp/check_missing_locations.csv"
    )
  }
  return(fr_main_dataset_step4)
}

