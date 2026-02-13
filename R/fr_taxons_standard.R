##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                        Standardize taxonomy                          ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## v2.0, October 2025                                                   ##
##########################################################################

fr_taxons_standard <- function(dataset = NULL, 
                               use_log = FALSE, 
                               save_to_disk = FALSE,
                               data_dir = data_dir
                               ){
  
  if (is.null(dataset) || !is.data.table(dataset)) {
    stop("Error: argument 'dataset' must be a non-null data.table.")
  }
  
  # --- Open log file ---
  if (use_log == TRUE){
    log_file <- file.path(data_dir, "output", paste0("log_file_", Sys.Date(), ".txt"))
    if (file.exists(log_file)) {
      sink(log_file, append = TRUE)  # Open log file for appending
    } else {
      sink(log_file, append = FALSE) # Create new log file
    }
  }
  cat("\nSTEP 2: Standardize taxa") 

  # --- 1. Clean taxon names ---
  dataset[, taxon := str_squish(trimws(taxon))] # remove leading, trailing and multiple white spaces
  dataset <- dataset[taxon != "" & !is.na(taxon)] # delete rows without taxa
  cat("\n   - Removed leading and trailing whitespace\n   - Deleted extra internal white spaces\n   - Deleted empty rows") 
  
  # --- 2. Call GBIF check function ---
  sink()
  gbif_result <- check_GBIF_taxa(taxon_names = dataset, 
                                 column_name_taxa = "taxon",
                                 save_interm=TRUE,
                                 data_dir=data_dir)
  matched_taxa <- unique(gbif_result[[1]]) # extract unique standardized species names
  mismatches <- unique(gbif_result[[2]][order(gbif_result[[2]]$taxon)]) # extract unique unmatched species names, ordered by taxa
  
  setDT(matched_taxa)
  setDT(mismatches)
  
  matched_taxa[, GBIFstatus := fifelse(is.na(GBIFstatus), "NoMatch", GBIFstatus)] # replace unmatched species by "NoMatch" in matched_taxa
  sink(log_file, append = TRUE)
  cat("\n   - Standardized taxon names across the GBIF backbone taxonomy") 
  
  # --- 3. Define taxonomic groups: fill in the column taxaGroup, based on class and order ---
  matched_taxa$taxaGroup <- NA
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa,class=="Mammalia")$scientificName] <- "Mammals"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa,class=="Aves")$scientificName] <- "Birds"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa,class%in%c("Cephalaspidomorphi","Actinopterygii","Elasmobranchii","Sarcopterygii", "Petromyzonti"))$scientificName] <- "Fishes"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa,order%in%c("Polypteriformes", "Acipenseriformes", "Lepisosteiformes", "Amiiformes", "Osteoglossiformes", "Hiodontiformes", 
                                                                                       "Elopiformes", "Albuliformes", "Notacanthiformes", "Anguilliformes", "Saccopharyngiformes", "Clupeiformes", "Ceratodontiformes",
                                                                                       "Gonorynchiformes", "Cypriniformes", "Characiformes", "Gymnotiformes", "Siluriformes", "Salmoniformes", "Esociformes", 
                                                                                       "Osmeriformes", "Ateleopodiformes", "Stomiiformes", "Aulopiformes", "Myctophiformes", "Lampriformes", "Polymixiiformes", 
                                                                                       "Percopsiformes", "Batrachoidiformes", "Lophiiformes", "Gadiformes", "Ophidiiformes", "Mugiliformes", "Atheriniformes", 
                                                                                       "Beloniformes", "Cetomimiformes", "Cyprinodontiformes", "Stephanoberyciformes", "Beryciformes", "Zeiformes", 
                                                                                       "Gobiesociformes", "Gasterosteiformes", "Syngnathiformes", "Synbranchiformes", "Tetraodontiformes", "Pleuronectiformes", 
                                                                                       "Scorpaeniformes", "Perciformes"))$scientificName] <- "Fishes"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa,class%in%c("Reptilia", "Testudines","Squamata", "Crocodylia"))$scientificName] <- "Reptiles"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa,class=="Amphibia")$scientificName] <- "Amphibians"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa, class%in%c("Insecta"))$scientificName] <- "Insects"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa, class%in%c("Arachnida", "Pycnogonida"))$scientificName] <- "Arachnids"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa, class%in%c("Collembola", "Chilopoda", "Diplopoda", "Diplura", "Merostomata", "Pauropoda", "Protura", "Symphyla"))$scientificName] <- "Other arthropods"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,class%in%c("Branchiopoda","Hexanauplia","Maxillopoda","Ostracoda","Malacostraca", "Copepoda"))$scientificName] <- "Crustaceans"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,family%in%c("Elminiidae"))$scientificName] <- "Crustaceans"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum=="Mollusca")$scientificName] <- "Molluscs"
  matched_taxa$taxaGroup[matched_taxa$scientificName %in% subset(matched_taxa, phylum %in% "Tracheophyta")$scientificName] <- "Vascular plants"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa,phylum%in%c("Bryophyta","Anthocerotophyta", "Marchantiophyta"))$scientificName] <- "Bryophytes"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Rhodophyta","Chlorophyta","Charophyta","Cryptophyta","Euglenozoa","Haptophyta","Foraminifera","Ciliophora","Ochrophyta","Myzozoa","Cercozoa"))$scientificName] <- "Algae"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Ascomycota", "Dothideomycetes", "Sordariomycetes", "Chytridiomycota","Basidiomycota","Microsporidia","Zygomycota", "Entomophthoromycota"))$scientificName] <- "Fungi"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Actinobacteria","Chlamydiae","Cyanobacteria","Firmicutes","Proteobacteria"))$scientificName |
                           matched_taxa$class == "Ichthyosporea"] <- "Bacteria and protozoans"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,kingdom%in%c("Bacteria", "Protozoa","Euglenozoa"))$scientificName] <- "Bacteria and protozoans"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Oomycota"))$scientificName] <- "Oomycetes"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,kingdom%in%c("Viruses"))$scientificName] <- "Viruses"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Annelida", "Nematoda", "Platyhelminthes", "Sipuncula", "Nemertea", "Onychophora", "Acanthocephala"))$scientificName] <- "Annelids, nematodes, platyhelminthes, and other worms"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Bryozoa", "Entoprocta", "Chaetognatha", "Cnidaria", "Ctenophora", "Echinodermata", "Phoronida", "Porifera", "Rotifera", "Xenacoelomorpha","Brachiopoda"))$scientificName] <- "Other aquatic animals"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,class=="Ascidiacea")$scientificName] <- "Other aquatic animals"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa, phylum%in%c("Foraminifera","Cercozoa","Ciliophora","Ochrophyta","Oomycota","Myzozoa","Peronosporea", "Bigyra"))$scientificName] <- "SAR"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa, genus%in%c("Plasmodium"))$scientificName] <- "SAR"
  matched_taxa$taxaGroup[is.na(matched_taxa$taxaGroup)] <- "Other"
  
  cat("\n   - Allocated taxonomic groups")

  # --- 4. Creating and adding unique taxonIDs ---
  unique_taxa <- unique(na.omit(matched_taxa[, .(taxon)])) # extract unique, non-missing taxon names from matched_taxa
  unique_taxa[, taxonID := .I] # assign a unique integer ID (taxonID) to each taxon ('.I returns the row index, which is used as an ID)
  matched_taxa <- merge(matched_taxa, unique_taxa, by = "taxon", all = TRUE) # merge taxonID in matched_taxa
  cat("\n   - Allocated taxon IDs")
  
  # --- 5. Write output ---
  
  ## Create fr_main_dataset_step2
  fr_main_dataset_step2 <- matched_taxa[, c("locationID", "verbatimLocation", "locality", "country", "region", "taxonID", "taxon",
                                            "habitat",	"firstRecordEvent",	"verbatimFirstRecordEvent", 
                                            "confidenceFirstRecordEvent",	"occurrenceStatus",	"establishmentMeans",
                                            "degreeOfEstablishment", "pathway",	"datasetName",	"bibliographicCitation",	
                                            "accessRights", "originalNameUsage", "scientificName", "scientificNameAuthorship", 
                                            "GBIFstatus","GBIFstatus_Synonym", "GBIFmatchtype", "GBIFtaxonRank",
                                            "GBIFusageKey","GBIFnote","species","genus","family",
                                            "order","class","phylum","kingdom", "taxaGroup"
  )]
  
  if (save_to_disk == TRUE){
    filename <- file.path(data_dir, "tmp", "fr_main_dataset_step2.csv")
    fwrite(fr_main_dataset_step2, filename)
    cat("\n  - Updated dataset available in 'tmp' folder\n ")
  }
  
  ## Create a temporary taxonomy table
  temp_taxonomy_table <- unique(matched_taxa[,c("taxonID", "taxon", "originalNameUsage", "scientificName", "scientificNameAuthorship", 
                                           "GBIFstatus","GBIFstatus_Synonym", "GBIFmatchtype", "GBIFtaxonRank",
                                           "GBIFusageKey","GBIFnote","species","genus","family",
                                           "order","class","phylum","kingdom", "taxaGroup"
  )])
  
  filename <- file.path(data_dir, "tmp", "temp_taxonomy_table.csv")
  fwrite(temp_taxonomy_table, filename)
  
  ## Create the unmatched taxa table
  filename <- file.path(data_dir, "tmp", "fr_check_unresolved_taxa.csv")
  fwrite(mismatches, filename)
  
  cat("\nStep 2 completed: taxa have been standardized. Unmatched taxa and a temporary taxonomy table are available in the 'tmp' folder\n ")
  
  if (use_log == TRUE){
    sink()
  }
  
  return(fr_main_dataset_step2)
}