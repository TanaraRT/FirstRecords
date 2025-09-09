##########################################################################
##                                                                      ##
##                        FIRST RECORDS WORKFLOW                        ##
##                            Initialization                            ##
##                   -----------------------------                      ##
##                                                                      ##
## T. Renard Truong, H. Seebens                                         ##
## v2.0, August 2025                                                    ##
##########################################################################

fr_initialization <- function (data_dir=NULL, input_file=NULL){
  
  if (!dir.exists(data_dir)){
    stop("Data directory not found: ", data_dir)
  }

  # --- Call libraries ---
  packages <- c("Hmisc", "gsubfn", "stringr", "stringi", "data.table", "rgbif",
                "worrms", "openxlsx", "tidyverse") 
  
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])] # check which of them is not yet installed
  if (length(new.packages)) install.packages(new.packages); rm(new.packages) # install them
  
  # load all required packages
  l <- sapply(packages, function(s) suppressMessages( 
                                        suppressWarnings(
                                            require(s, quietly=T, character.only = TRUE))
                                        )
                                    ) 
  rm(packages, l)

  # --- Define or create the 'config' folder ---
  config <- file.path(data_dir, "config")
  if (!dir.exists(config)) {
    dir.create(config, recursive = TRUE)
  }
  cat("\n  - Configuration folder is ready at:", config, ". Please make sure configuration files are uploaded in this folder.\n")

  # --- Define or create the 'input' folder ---
  input <- file.path(data_dir, "input")
  if (!dir.exists(input)) {
    dir.create(input, recursive = TRUE)
  }
  cat("\n  - Input folder is ready at:", input, ". Please make sure input dataset is uploaded in this folder.\n")
  
  # --- Construct full path to input file ---
  filename <- file.path(input, input_file)
  if (!file.exists(filename)){
    stop("Input file not found: ", filename)
  }
  
  # --- Define or create the 'output' folder ---
  output <- file.path(data_dir, "output")
  if (!dir.exists(output)) {
    dir.create(output, recursive = TRUE)
  }
  cat("\n  - Output folder is ready at:", output, "\n")

# --- Define and create 'tmp' folder ---
  tmp <- file.path(data_dir, "tmp")
  if (!dir.exists(tmp)) {
    dir.create(tmp, recursive = TRUE)
  }
  cat("\n  - Temporary folder is ready at:", tmp, "\n")

# --- Load functions ---
  source(file.path("R","fr_prepare_main_dataset.r"))
  source(file.path("R","fr_taxons_standard.r"))
  source(file.path("R","check_GBIF_taxa.r"))
  source(file.path("R","fr_years_standard.r"))
  source(file.path("R","fr_localities_standard.r"))
  source(file.path("R","fr_terms_standard.R"))
  source(file.path("R","standardize_and_filter_terms.r"))

# --- Import data ---
  filename <- file.path(input, input_file)
  fr_input_data <- read.csv2(filename, fileEncoding = "UTF-8",
                           stringsAsFactors = FALSE
  )
  setDT(fr_input_data) 
  
  cat("\n  - Functions and input data loaded\n")
  
  return(fr_input_data)
  }