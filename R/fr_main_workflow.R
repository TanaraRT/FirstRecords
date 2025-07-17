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
fr_input_data <- read.csv2('data/input/IntroData_raw.csv', fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)

# convert to data.table
setDT(fr_input_data) 

# load functions
source("R/fr_prepare_main_dataset.r")
source("R/fr_taxons_standard.r")
source("R/check_GBIF_taxa.r")
source("R/fr_years_standard.r")
source("R/fr_localities_standard.r")
source("R/fr_terms_standard.r")
source("R/standardize_and_filter_terms.r")

cat("\nIntialization completed: library, functions and input data loaded\n ") 

## 1) PREPARATION OF DATASET #############################################
cat("\nSTEP 1: Prepare main dataset: fr_main_dataset") 
fr_main_dataset_1 <- fr_prepare_main_dataset(dataset = fr_input_data, save_to_disk = TRUE)

## 2) STANDARDIZATION OF TAXA ############################################
cat("\nSTEP 2: Standardize taxa\n") 
fr_main_dataset_2 <- fr_taxons_standard(dataset = fr_main_dataset_1, save_to_disk = TRUE)

## 3) STANDARDIZATION OF YEARS############################################
cat("\nSTEP 3: Standardize years\n") 
fr_main_dataset_3 <- fr_years_standard(dataset = fr_main_dataset_2, save_to_disk = TRUE)

## 4) STANDARDIZATION OF LOCALITIES #######################################
cat("\nSTEP 4: Standardize localities\n") 
fr_main_dataset_4 <- fr_localities_standard(fr_main_dataset_3, save_to_disk = TRUE)

## 5) STANDARDIZATION OF REMAINING TERMS ################################
cat("\nSTEP 5: Standardize remaining terms\n") 
fr_final_dataset <- fr_terms_standard(fr_main_dataset_4)