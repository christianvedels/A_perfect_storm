# Aggregate demographic data
#
# Date updated:   2023-04-14
# Auhtor:         Christian Vedel 
# Purpose:        Join on data on HISCO codes and summarise
#
# Output:         'popdata.csv', demographic data aggregated to parish level 

# ==== Libraries ====
library(tidyverse)
source("000_Functions.R")

# ==== Load data ====
load("Data/tmp_census.Rdata")

# ==== Age groups ====
# Sample toy data for development. Commented out in final run
merged_data = merged_data %>% sample_n(10000)

merged_data = merged_data %>%
  mutate(
    age_category = case_when(
      age < 5 ~ "Under 5",
      age < 15 ~ "Under 15",
      age >= 15 & age < 25 ~ "15-24",
      age >= 25 & age < 35 ~ "25-34",
      age >= 35 & age < 65 ~ "35-64",
      age >= 65 & age < 80 ~ "65-79",
      age >= 80 ~ "80 and over"
    )
  )


merged_data %>% 
  group_by(Year, GIS_ID, age_category) %>% 
  count()



