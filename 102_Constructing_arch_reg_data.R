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

# ==== Loading arch data ====
# The following files can be recreated with 009_Archaeological_monte_carlo.R
# But this takes a while. They can also be donwloaded here: 
# https://www.dropbox.com/scl/fo/nxuv09eraovysu8bge9nu/h?dl=0&rlkey=a1d1lzxe04fzx6xa3fe0xvgq4

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

# ==== Main_panel ====



