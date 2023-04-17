# Aggregate demographic data
#
# Date updated:   2023-04-17
# Auhtor:         Christian Vedel 
# Purpose:        Join on data on HISCO codes and summarise
#
# Output:         'popdata.csv', demographic data aggregated to parish level 
#

# ==== Libraries ====
library(tidyverse)
library(fst)

# ==== Load data ====
merged_data = read_fst("Data/tmp_census.fst") 

# ==== Age_cats ====
Age_cats = function(age, lower, upper){
  
}

# ==== Summarise at parish level ====
# - Population 
# - Age groups
# - Occupational groups
# - HISCO first digit
# - Males
# - Females
# - FLFP

merged_data %>% 
  group_by(Year, GIS_ID) %>% 
  summarise(
    Pop = n()
  )
  
  
  
  
  