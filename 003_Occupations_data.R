# Adding occupations
#
# Date updated:   2023-04-13
# Auhtor:         Christian Vedel 
# Purpose:        Join on data on HISCO codes
#
# Output:         'merged_data' enriched with HISCO codes + occupational categories


# HISCO codes generated from automatic HISCO classifier. 
# See my website for more information: https://sites.google.com/view/christianvedel 

# ==== Libraries ====
library(tidyverse)

# ==== Load data ====
load("Data/tmp_census.Rdata")
hisco = read_csv2("Data/LL_hisco_codes_clean.csv") # Available on request: christian-vs@sam.sdu.dk

# ==== Data cleaning ====
hisco %>% 
  select(pa_id, Year, Erhverv, Stilling_i_husstanden, hisco1:en_hisco_text5) %>% 
  rename(
    Occupation = Erhverv,
    Household_position = Stilling_i_husstanden,
  )
