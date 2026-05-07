# 999_main.R
# Date updated:   2026-03-21
# Author:         Christian Vedel
#
# Purpose:        Master script — runs all steps in order
#
# Usage:          source("999_main.R")
#                 Or run interactively section by section.

# ==== Options ====
# OVERWRITE: 0 = never overwrite; 1 = overwrite if older than 7 days; 2 = always overwrite
OVERWRITE = 1

# ==== Helper ====
run_script = function(path, overwrite = OVERWRITE){
  assign("OVERWRITE", overwrite, envir = .GlobalEnv)
  cat("\n================================================================\n")
  cat("Running:", path, "(OVERWRITE =", overwrite, ")\n")
  cat("================================================================\n")
  source(path, echo = FALSE)
  cat("Done:   ", path, "\n")
}

# ==== 0xx: Data construction ====
run_script("000_Functions.R")
run_script("001_Read_census_data.R", overwrite = 2) # Always overwrite, as this is the base data
run_script("002_Linking_geo_data.R", overwrite = 2)
run_script("003_Occupations_data.R", overwrite = 2)
run_script("004_Aggregate_demographic_data.R")
run_script("005_Limfjord_regions .R")
run_script("006_Sound_toll_data.R")
run_script("007_Archaeological_data_clean.R")
run_script("008_Market_access.R")
run_script("009_Archaeological_monte_carlo.R")
run_script("010_Distance_to_market_towns.R")
run_script("011_Parish_soil.R")

# ==== 1xx: Regression data assembly ====
run_script("101_Constructing_arch_reg_data.R")
run_script("102_Matching_soil_types.R")
run_script("103_Constructing_pop_reg.R")

# ==== 2xx: Results and figures ====
run_script("201_Main_map.R")
run_script("202_Sound_toll_results.R")
run_script("203_Pop_results.R")
run_script("204_Archaeological_results.R")
run_script("205_Pop_mechanism.R")

cat("\n================================================================\n")
cat("All scripts completed.\n")
cat("================================================================\n")
