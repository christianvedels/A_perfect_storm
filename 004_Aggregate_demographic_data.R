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

# ==== Age / gender /  groups ====
# Sample toy data for development. Commented out in final run
merged_data = merged_data %>% sample_n(10000)

merged_data %>%
  mutate(
    age_category = case_when(
      age < 15 ~ "Under 15",
      age >= 15 & age < 25 ~ "15-24",
      age >= 25 & age < 35 ~ "25-34",
      age >= 35 & age < 45 ~ "35-44",
      age >= 45 & age < 55 ~ "45-54",
      age >= 55 & age < 65 ~ "55-64",
      age >= 65 & age < 75 ~ "65-74",
      age >= 75 & age < 85 ~ "75-84",
      age >= 85 ~ "85 and over"
    )
  )


