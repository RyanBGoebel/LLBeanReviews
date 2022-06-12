# ==============================================================================
# INSTALL DEPENDENCIES
# ==============================================================================
list.of.packages <- c("stringr", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(stringr)
library(reshape2)

# ==============================================================================
# READ IN LL BEAN DATA AS ROWS OF CHARACTER STRINGS
# ==============================================================================

LLBean <- (read.csv("llbean_full.csv"))

# ==============================================================================
# UPDATE VARIABLE NAMES TO MATCH SPONSOR-PROVIDED DATA
# ==============================================================================
names(LLBean)[names(LLBean) == "review_text"] <- "reviewText"
names(LLBean)[names(LLBean) == "review_title"] <- "reviewTitle"
names(LLBean)[names(LLBean) == "description"] <- "itemGroup"

LLB <- LLBean
rm(LLBean)