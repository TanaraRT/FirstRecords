##########################################################################
##                                                                      ##
##                        FIRST RECORDS WORKFLOW                        ##
##                            Main workflow                             ##
##                   -----------------------------                      ##
##                                                                      ##
## H. Seebens, T. Renard Truong                                         ##
## v2.0, 2025                                                           ##
##########################################################################

## 1) PREPARE WORKSPACE AND IMPORT DATA ##################################

cat("\nINITIALIZATION: Prepare workspace and import data") 

# clean workspace
graphics.off()
rm(list = ls())

# call libraries
library(Hmisc)
library(stringr)
library(stringi)
library(data.table)
library(rgbif)
library(worrms)
library(openxlsx)
library(tidyverse)

# import data
fr_raw_data <- read.csv2('data/raw/IntroData_raw.csv', fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

# convert to data.table
setDT(fr_raw_data) 

# load functions
source("scripts/fr_prepare_main_dataset.r")
source("scripts/fr_taxons_standard.r")
source("scripts/check_GBIF_taxa.r")
source("scripts/OverwriteTaxonNames.r")
source("scripts/StandardiseLocationNames.r")

cat("\nIntialization completed: library, functions and raw data loaded\n ") 


## 1) PREPARATION OF DATASET #############################################
cat("\nSTEP 1: Prepare main dataset: fr_main_dataset") 
fr_main_dataset_1 <- fr_prepare_main_dataset(fr_raw_data)

## 2) STANDARDIZATION OF TAXA ############################################
cat("\nSTEP 2: Standardize taxa\n") 
results_step2 <- fr_taxons_standard (fr_main_dataset_1)
fr_main_dataset_2a <- results_step2$taxon_dataset # without Taxon ID
fr_fullspeclist <- results_step2$fullspeclist # with Taxon ID
fr_mismatches <- results_step2$mismatches
fr_main_dataset_2b <- OverwriteTaxonNames(fr_main_dataset_2a, fr_fullspeclist, fr_mismatches)
  
## 3) STANDARDIZATION OF LOCALITIES #######################################
cat("\nSTEP 3: Standardize localities\n") 
fr_main_dataset_step3 <- StandardiseLocationNames(fr_main_dataset_2b)





