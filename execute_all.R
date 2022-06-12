# ==============================================================================
# EXECUTE FILES IN ORDER
# ==============================================================================
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("scrape_data_import.R")
source("model_classification.R")
source("bean_textMining.R")