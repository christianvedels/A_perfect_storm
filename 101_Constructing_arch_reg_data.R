# Regression archaeological data
# Date updated:   2024-06-16
# Author:         Christian Vedel 
# Purpose:        Construction of regression data for archaeological findings. 
#
# Output:         'Reg_arch_coins.csv'


# ==== Library ====
library(tidyverse)
library(foreach)
source("000_Functions.R")

# ==== Load data ====
# Guessmax = 2000 to ensure that GIS_ID is not read as numeric
geo_data = read_csv2("Data/Geo.csv", guess_max = 2000)
market_access = read_csv2("Data/MA_estimates.csv", guess_max = 2000) 

# ==== Market access ====
market_access = market_access %>% 
  mutate(
    delta_lMA = log(MA_after_before)
  ) %>% 
  select(GIS_ID, delta_lMA, theta, alpha) %>% 
  pivot_wider(
    names_from = c(theta, alpha),
    values_from = delta_lMA,
    names_glue = "delta_lMA_theta_{-theta}_alpha_{alpha}"
  )

# ==== Main_panels and save ====
# These are the two main panels of archaeological findings used in the paper
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

buildings = get_dataframe_by_name(
  filename = get_file_info("Buildings_unif"),
  dataset = "https://doi.org/10.7910/DVN/P7STIM", # DOI
  server = "dataverse.harvard.edu",
  .f = load0 # Function to read the file
)

coins = get_dataframe_by_name(
  filename = get_file_info("Coin findings_unif.Rdata"),
  dataset = "https://doi.org/10.7910/DVN/P7STIM", # DOI
  server = "dataverse.harvard.edu",
  .f = load0 # Function to read the file
)

# ==== Construct reg data
buildings = construct_panel(res_is$Overall_Buildings$samples)
buildings %>% 
  left_join(geo_data, by = "GIS_ID") %>% 
  left_join(market_access, by = "GIS_ID") %>% 
  write_csv2("Data/Reg_arch_buildings.csv")

load("Data/Tmp_arch_samples/Coin findings.Rdata")
coins = construct_panel(res_is$`Overall_Coin findings`$samples)
coins %>% 
  left_join(geo_data, by = "GIS_ID") %>% 
  left_join(market_access, by = "GIS_ID") %>% 
  write_csv2("Data/Reg_arch_coins.csv")


# ==== Based on normal distribution ====
load("Data/Tmp_arch_samples_norm/Buildings.Rdata")
buildings = construct_panel(res_is$Overall_Buildings$samples)
buildings %>% 
  left_join(geo_data, by = "GIS_ID") %>% 
  left_join(market_access, by = "GIS_ID") %>% 
  write_csv2("Data/Reg_arch_buildings_norm.csv")

load("Data/Tmp_arch_samples_norm/Coin findings.Rdata")
coins = construct_panel(res_is$`Overall_Coin findings`$samples)
coins %>% 
  left_join(geo_data, by = "GIS_ID") %>% 
  left_join(market_access, by = "GIS_ID") %>% 
  write_csv2("Data/Reg_arch_coins_norm.csv")
