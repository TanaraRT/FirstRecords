##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                        Standardize taxonomy                          ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## vx.x, 2025                                                           ##
##########################################################################

fr_taxons_standard <- function (fr_mdataset){
  
  # Basic data cleaning
  fr_mdataset[, originalNameUsage := str_replace_all(
    tolower(trimws(originalNameUsage)), "\\p{Zs}|\\s+", " ")]
  fr_mdataset <- fr_mdataset[originalNameUsage != "" & !is.na(originalNameUsage)]
  
  # Apply GBIF function
  gbif_result <- CheckGBIFTax(fr_mdataset)
  taxon_dataset <- gbif_result[[1]]
  mismatches <- gbif_result[[2]]
  
  print("hey1")
  
  
  # Clean taxon_dataset and mismatches
  taxon_dataset <- unique(taxon_dataset) # remove duplicates
  taxon_dataset$GBIFstatus[is.na(taxon_dataset$GBIFstatus)] <- "NoMatch"
  taxon_dataset <- taxon_dataset[,!names(taxon_dataset)%in%c("GBIFstatus",
                                                                "GBIFmatchtype",
                                                                "GBIFtaxonRank",
                                                                "GBIFusageKey",
                                                                "GBIFnote",
                                                                "GBIFstatus_Synonym",
                                                                "species",
                                                                "genus","family",
                                                                "class","order",
                                                                "phylum","kingdom"
                                                                )]
  
  
  oo <- order(mismatches$Taxon)
  mismatches <- unique(mismatches[oo,])
  
  # Export full species list
  fullspeclist <- 
    unique(taxon_dataset[,c("Taxon_orig","Taxon", "scientificName","GBIFstatus",
                            "GBIFstatus_Synonym", "GBIFmatchtype", "GBIFtaxonRank",
                            "GBIFusageKey","GBIFnote","species","genus","family",
                            "order","class","phylum","kingdom"
    )])
  oo <- order(fullspeclist$kingdom,fullspeclist$phylum,fullspeclist$class,fullspeclist$order,fullspeclist$Taxon)
  fullspeclist <- unique(fullspeclist[oo,])

  ## assign taxon ID unique to individual taxa #############
  ## identify unique taxa (obtained from GBIF)
  fullspeclist[, sequence := .I]  # Assigns row number to a new column "sequence"
  uni_taxa <- unique(na.omit(fullspeclist[, .(scientificName)]))  # Keep unique, non-NA scientific names
  uni_taxa[, taxonID := .I]  # Assign a unique taxonID to each row
  
  # Merge taxonID into fullspeclist
  fullspeclist_2 <- merge(fullspeclist, uni_taxa, by = "scientificName", all = TRUE)
  fullspeclist_2[is.na(taxonID), taxonID := max(taxonID, na.rm = TRUE) + .I]
  setorder(fullspeclist_2, sequence)
  fullspeclist_2[, sequence := NULL]
  print("hey")
  # Add taxon ID back to dataset
  taxon_id <- unique(fullspeclist_2[, .(taxonID, originalNameUsage)])
  fr_mdataset <- merge(fr_mdataset, taxon_id, by = "originalNameUsage", all.x = TRUE)
  fwrite(fr_mdataset, "output/fr_mdataset_step1.csv")
  print("hey2")
  
  # Output final result
  fwrite(fullspeclist_2, "tmp/fr_full_taxa_list.csv")
  fwrite(taxon_dataset, "tmp/fr_taxon_standards_intermediate.csv")
  fwrite(mismatches[order(Taxon)], "tmp/fr_check_missing_taxa.csv")
  
  return(list(
    taxon_dataset = taxon_dataset,
    mismatches = mismatches,
    fullspeclist = fullspeclist_2
    ))}