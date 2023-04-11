# Reading census data
#
# Date updated:   2023-04-11
# Auhtor:         Christian Vedel 
# Purpose:        This script reads csv-files containing census data and merges them



# ==== Libraries ====
library(tidyverse)
source("00_functions.R")

# ==== Loading standardized data ====
all_dirs = list.dirs("Link lives census")
all_dirs = all_dirs[grep("standardized_sources/census/", all_dirs)]