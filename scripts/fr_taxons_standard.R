##########################################################################
##                                                                      ##
##                       FIRST RECORDS WORKFLOW                         ##
##                        Standardize taxonomy                          ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## vx.x, 2025                                                           ##
##########################################################################

fr_taxons_standard <- function(dataset = NULL) {
  stopifnot(!is.null(dataset) && is.data.table(dataset))
  
  # 1. Clean taxon names
  dataset[, Taxon := str_squish(tolower(trimws(Taxon)))]
  dataset <- dataset[Taxon != "" & !is.na(Taxon)]
  
  # 2. Call GBIF check function
  gbif_result <- check_GBIF_taxa(taxon_names = dataset, column_name_taxa = "Taxon")
  matched_taxa <- unique(gbif_result[[1]])
  mismatches <- unique(gbif_result[[2]][order(gbif_result[[2]]$Taxon)])

  matched_taxa[, GBIFstatus := fifelse(is.na(GBIFstatus), "NoMatch", GBIFstatus)]
  
  # 3. Define taxonomic groups
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
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Rhodophyta","Chlorophyta","Charophyta","Cryptophyta","Haptophyta"))$scientificName] <- "Algae"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Ascomycota", "Dothideomycetes", "Sordariomycetes", "Chytridiomycota","Basidiomycota","Microsporidia","Zygomycota", "Entomophthoromycota"))$scientificName] <- "Fungi"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Actinobacteria","Chlamydiae","Cyanobacteria","Firmicutes","Proteobacteria"))$scientificName |
                           matched_taxa$class == "Ichthyosporea"] <- "Bacteria and protozoans"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,kingdom%in%c("Bacteria", "Protozoa","Euglenozoa"))$scientificName] <- "Bacteria and protozoans"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,kingdom%in%c("Viruses"))$scientificName] <- "Viruses"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Annelida", "Nematoda", "Platyhelminthes", "Sipuncula", "Nemertea", "Onychophora", "Acanthocephala"))$scientificName] <- "Annelids, nematodes, platyhelminthes, and other worms"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,phylum%in%c("Bryozoa", "Entoprocta", "Chaetognatha", "Cnidaria", "Ctenophora", "Echinodermata", "Phoronida", "Porifera", "Rotifera", "Xenacoelomorpha","Brachiopoda"))$scientificName] <- "Other aquatic animals"
  matched_taxa$taxaGroup[matched_taxa$scientific%in%subset(matched_taxa,class=="Ascidiacea")$scientificName] <- "Other aquatic animals"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa, phylum%in%c("Foraminifera","Cercozoa","Ciliophora","Ochrophyta","Oomycota","Myzozoa","Peronosporea", "Bigyra"))$scientificName] <- "SAR"
  matched_taxa$taxaGroup[matched_taxa$scientificName%in%subset(matched_taxa, genus%in%c("Plasmodium"))$scientificName] <- "SAR"
  
  cat("Step 2b completed: taxonomic groups have been allocated\n")

  # 4. Create taxonomy table with unique taxonIDs
  matched_taxa[, sequence := .I]
  unique_taxa <- unique(na.omit(matched_taxa[, .(scientificName)]))
  unique_taxa[, taxonID := .I]
  
  taxonomy_table <- merge(matched_taxa, unique_taxa, by = "scientificName", all = TRUE)
  taxonomy_table[is.na(taxonID), taxonID := max(taxonID, na.rm = TRUE) + .I]
  
  taxonomy_table <- unique(taxonomy_table[,c("taxonID", "Taxon", "originalNameUsage", "scientificName", "scientificNameAuthorship", 
                                             "GBIFstatus","GBIFstatus_Synonym", "GBIFmatchtype", "GBIFtaxonRank",
                                             "GBIFusageKey","GBIFnote","species","genus","family",
                                             "order","class","phylum","kingdom", "taxaGroup"
      )])
  
  # 5. Add taxonID to the main dataset (ensure unique Taxon values)
  taxon_id_lookup <- unique(taxonomy_table[, .(Taxon, taxonID)])
  fr_main_dataset_step2 <- merge(matched_taxa, taxon_id_lookup, by = "Taxon", all.x = TRUE)
  
  setcolorder(fr_main_dataset_step2, c(setdiff(names(fr_main_dataset_step2), "taxonID"), "taxonID"))
  
  fr_main_dataset_step2 <- fr_main_dataset_step2[, c("locationID", "verbatimLocation", "locality", "country", "continent", "taxonID", "Taxon",
                                                     "habitat",	"firstRecordEvent",	"verbatimFirstRecordEvent", 
                                                     "confidenceFirstRecordEvent",	"occurenceStatus",	"establishmentMeans",
                                                     "degreeOfEstablishment",	"datasetName",	"bibliographicCitation",	
                                                     "accessRights"
  )]
  
  
  # 6. Write outputs
  fwrite(taxonomy_table, "tmp/taxonomy_table.csv")
  fwrite(fr_main_dataset_step2, "tmp/fr_main_dataset_step2.csv")
  fwrite(mismatches, "tmp/fr_check_missing_taxa_2a.csv")
  
  cat("Step 2c completed: taxon ID have been allocated\n")
  

  return(
    fr_main_dataset_step2
   )
}
