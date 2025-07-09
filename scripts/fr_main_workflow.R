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
source("scripts/fr_years_standard.r")
source("scripts/StandardiseLocationNames.r")

cat("\nIntialization completed: library, functions and raw data loaded\n ") 


## 1) PREPARATION OF DATASET #############################################
cat("\nSTEP 1: Prepare main dataset: fr_main_dataset") 
fr_main_dataset_1 <- fr_prepare_main_dataset(dataset = fr_raw_data, save_to_disk = TRUE)

## 2) STANDARDIZATION OF TAXA ############################################
cat("\nSTEP 2: Standardize taxa\n") 
fr_main_dataset_2a <- fr_taxons_standard (dataset = fr_main_dataset_1)

## 3) STANDARDIZATION OF FIRST RECORDS####################################
fr_main_dataset_3 <- fr_years_standard (dataset = fr_main_dataset_2a)
  
## 4) STANDARDIZATION OF LOCALITIES #######################################
cat("\nSTEP 3: Standardize localities\n") 
fr_main_dataset_step3 <- StandardiseLocationNames(fr_main_dataset_2b)





