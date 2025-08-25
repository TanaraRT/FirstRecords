##########################################################################
##                                                                      ##
##                        FIRST RECORDS WORKFLOW                        ##
##                            Main workflow                             ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, August 2025                                                    ##
##########################################################################

## 1) PREPARE WORKSPACE AND IMPORT DATA ##################################

cat("\nINITIALIZATION: Prepare workspace and import data") 

# --- Clean workspace ---
graphics.off()
rm(list = ls())

# --- Call libraries ---
suppressPackageStartupMessages({
  library(Hmisc)
  library(gsubfn)
  library(stringr)
  library(stringi)
  library(data.table)
  library(rgbif)
  library(worrms)
  library(openxlsx)
  library(tidyverse)
})

# --- Import data ---
fr_input_data <- read.csv2('data/input/IntroData_raw.csv', fileEncoding = "UTF-8",
  stringsAsFactors = FALSE
)
setDT(fr_input_data) 

# --- Define 'outputs' folder ---
# coming soon

# --- Create 'tmp' folder if needed ---
# coming soon

# --- Load functions ---
source("R/fr_prepare_main_dataset.r")
source("R/fr_taxons_standard.r")
source("R/check_GBIF_taxa.r")
source("R/fr_years_standard.r")
source("R/fr_localities_standard.r")
source("R/fr_terms_standard.R")
source("R/standardize_and_filter_terms.r")

cat("\nIntialization completed: library, functions and input data loaded\n ") 

## 1) PREPARATION OF DATASET #############################################
cat("\nSTEP 1: Prepare main dataset") 
fr_main_dataset_1 <- fr_prepare_main_dataset(dataset = fr_input_data,
                                             use_log = TRUE, # TRUE to record progress in log file in 'outputs' folder
                                             save_to_disk = TRUE) # TRUE to save fr_main_dataset_1 in 'tmp' folder
cat("\nStep 1 completed: main dataset 'fr_main_dataset' ready to be processed\n ") 

## 2) STANDARDIZATION OF TAXA ############################################
cat("\nSTEP 2: Standardize taxa") 
fr_main_dataset_2 <- fr_taxons_standard(dataset = fr_main_dataset_1,
                                        use_log = TRUE, # TRUE to record progress in log file in 'outputs' folder
                                        save_to_disk = TRUE) # TRUE to save fr_main_dataset_2 in 'tmp' folder
cat("\nStep 2 completed: taxa have been standardized in 'fr_main_dataset_2'. Unmatched taxa are available in the 'tmp' folder and a taxonomy table is available in the 'outputs' folder\n ")

## 3) STANDARDIZATION OF YEARS############################################
cat("\nSTEP 3: Standardize years") 
fr_main_dataset_3 <- fr_years_standard(dataset = fr_main_dataset_2, 
                                       fr_column_name = "firstRecordEvent", 
                                       use_log = TRUE, # TRUE to record progress in log file in 'outputs' folder
                                       save_to_disk = TRUE) # TRUE to save fr_main_dataset_3 in 'tmp' folder

cat("\nStep3 completed: first records (years) have been standardized in 'fr_main_dataset_3'. Years that couldn't be standardized are available in the 'tmp' folder\n ")

## 4) STANDARDIZATION OF LOCALITIES #######################################
cat("\nSTEP 4: Standardize localities") 
fr_main_dataset_4 <- fr_localities_standard(fr_main_dataset_3, 
                                            use_log = TRUE, # TRUE to record progress in log file in 'outputs' folder
                                            save_to_disk = TRUE) # TRUE to save fr_main_dataset_4 in 'tmp' folder
cat("\nStep 4 completed: locations have been standardized and the location table is available in data/outputs folder\n ") 

## 5) STANDARDIZATION OF REMAINING TERMS ################################
fr_final_dataset <- fr_terms_standard(fr_main_dataset_4, 
                                      use_log = TRUE) # TRUE to record progress in log file in 'outputs' folder
cat("\n  Final dataset available in data/output folder\n ")
