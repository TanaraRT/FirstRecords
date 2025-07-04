##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                        Standardize taxonomy                          ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## vx.x, 2025                                                           ##
##########################################################################
# PROBLEMS: fr_main_dataset_step2 doesn't have Taxon ID, and shouldn't include all the detailed columns

fr_taxons_standard <- function (dataset = NULL){
  if (is.null(dataset) || !is.data.frame(dataset)) {
    stop("Invalid input: dataset must be a data.frame or data.table")
  }
  # Basic data cleaning
  dataset[, Taxon := str_replace_all(
    tolower(trimws(Taxon)), "\\p{Zs}|\\s+", " ")]
  dataset <- dataset[Taxon != "" & !is.na(Taxon)]

  # Apply GBIF function
  gbif_result <- check_GBIF_taxa(taxon_names=dataset, 
               column_name_taxa="Taxon")
  matched_taxa <- gbif_result[[1]]
  #str(matched_taxa)
  mismatches <- gbif_result[[2]]
 # str(mismatches)
  
  # Clean matched_taxa and mismatches
  matched_taxa <- unique(matched_taxa) # remove duplicates
  names(matched_taxa)
  
  matched_taxa$GBIFstatus[is.na(matched_taxa$GBIFstatus)] <- "NoMatch"
  #matched_taxa <- matched_taxa[,!names(matched_taxa)%in%c("GBIFstatus",
  #                                                              "GBIFmatchtype",
  #                                                              "GBIFtaxonRank",
  #                                                              "GBIFusageKey",
   #                                                             "GBIFnote",
    #                                                            "GBIFstatus_Synonym",
     #                                                           "species",
      #                                                          "genus","family",
       #                                                        "phylum","kingdom"
        #                                                        )]
  
  oo <- order(mismatches$Taxon)
  mismatches <- unique(mismatches[oo,])
  
  # Export unique species list
  unique_species_list <- unique(matched_taxa[,c("originalNameUsage","Taxon", "scientificName","GBIFstatus",
                            "GBIFstatus_Synonym", "GBIFmatchtype", "GBIFtaxonRank",
                            "GBIFusageKey","GBIFnote","species","genus","family",
                            "order","class","phylum","kingdom"
    )])
  
  oo <- order(unique_species_list$kingdom,unique_species_list$phylum,unique_species_list$class,unique_species_list$order,unique_species_list$Taxon)
  unique_species_list <- unique(unique_species_list[oo,])

  # Save original column order before merging
  unique_species_list[, sequence := .I]
  original_order <- names(unique_species_list)

  ## assign taxon ID unique to individual taxa #############
  ## identify unique taxa (obtained from GBIF)
  unique_species_list[, sequence := .I]  # Assigns row number to a new column "sequence"
  uni_taxa <- unique(na.omit(unique_species_list[, .(scientificName)]))  # Keep unique, non-NA scientific names
  uni_taxa[, taxonID := .I]  # Assign a unique taxonID to each row
 
   # Merge taxonID into unique_species_list
  taxonomy_table <- merge(unique_species_list, uni_taxa, by = "scientificName", all = TRUE)
  taxonomy_table[is.na(taxonID), taxonID := max(taxonID, na.rm = TRUE) + .I]
  setorder(taxonomy_table, sequence)
  taxonomy_table[, sequence := NULL]
  
  # Fix column order: remove 'sequence' before setting column order
  original_order <- setdiff(original_order, "sequence")
  setcolorder(taxonomy_table, c(original_order, "taxonID"))
  
  # Add taxon ID back to dataset
  taxon_id <- unique(taxonomy_table[, .(taxonID, Taxon)])
  dataset <- merge(dataset, taxon_id, by = "Taxon", all.x = TRUE)
  fwrite(dataset, "tmp/fr_main_dataset1_with_taxon_id.csv")
  
  # Output final result
  fwrite(taxonomy_table, "tmp/taxonomy_table.csv")
  fwrite(matched_taxa, "tmp/fr_main_dataset_step2a.csv")
  fwrite(mismatches[order(mismatches$Taxon), ], "tmp/fr_check_missing_taxa_2a.csv")

  cat("Step 2a completed: species names have been standardized\n") 
  
#return (taxonomy_table)
  return(list(
    matched_taxa = matched_taxa,
    mismatches = mismatches,
    unique_species_list = taxonomy_table
  ))
  }