# Construction of regression population data
# Date updated:   2023-04-22
# Auhtor:         Christian Vedel 
#
# Purpose:        
# Output:         'Reg_pop.csv'


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

# The following files can be recreated with 009_Archaeological_monte_carlo.R
# But this takes a while. They can also be donwloaded here: 
# https://www.dropbox.com/scl/fo/nxuv09eraovysu8bge9nu/h?dl=0&rlkey=a1d1lzxe04fzx6xa3fe0xvgq4

load("Data/Tmp_arch_samples/Buildings.Rdata")
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
