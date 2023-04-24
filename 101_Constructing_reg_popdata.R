# Construction of regression population data
# Date updated:   2023-04-22
# Auhtor:         Christian Vedel 
#
# Purpose:        
# Output:         'Reg_pop.csv'


# ==== Library ====
library(tidyverse)

# ==== Load data ====
# Guessmax = 2000 to ensure that GIS_ID is not read as numeric
parish_pop = read_csv2("Data/Popdata.csv", guess_max = 2000)
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

# ==== Pop data ====
# Excluding inconsistent parishes
parish_pop = parish_pop %>% 
  filter(consistent==1)

# Joining geo data
parish_pop = parish_pop %>% 
  left_join(geo_data, by = "GIS_ID")

# Excluding Northern Schleswig
parish_pop = parish_pop %>% 
  filter(
    !County %in% c(
      "Toender",
      "Aabenraa",
      "Soenderborg",
      "Haderslev"
    )
  )

# Join MA data
parish_pop = parish_pop %>% 
  left_join(
    market_access, by = "GIS_ID"
  )
  
# ==== Save result ====
parish_pop %>% write_csv2("Data/Parish_pop.csv")