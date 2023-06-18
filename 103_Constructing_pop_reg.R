# Pop reg data
# Date updated:   2023-06-12
# Auhtor:         Christian Vedel 
#
# Purpose:        Constructs the regression data used in the analysis        
# Output:         Pop_reg.csv

# ==== Libraries ====
library(tidyverse)
library(foreach)

# ==== Read data ====
reg_pop = read_csv2("Data/Popdata.csv", guess_max = 2000)
geo_data = read_csv2("Data/Geo.csv", guess_max = 2000)
market_access = read_csv2("Data/MA_estimates.csv", guess_max = 2000) 
market_towns = read_csv2("Data/Market_towns.csv")
dist_mt = read_csv2("Data/Distance_to_market_town.csv", guess_max = 2000)

# ==== Small dataficiations ====
# Reshaping MA estimates
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

# Adding geo and MA to samples
reg_pop = reg_pop %>%
  left_join(geo_data, by = "GIS_ID") %>%
  left_join(market_access, by = "GIS_ID") %>%
  mutate(Year = relevel(factor(Year), ref = "1801")) %>%
  fastDummies::dummy_cols("limfjord_placement") %>% 
  filter(consistent == 1) %>% 
  drop_na(GIS_ID)

# Adding market town dummmy
mt = market_towns %>% 
  distinct(GIS_ID) %>% 
  mutate(Market_town = 1)

reg_pop = reg_pop %>% 
  left_join(mt, by = "GIS_ID") %>% 
  mutate(
    Market_town = ifelse(is.na(Market_town), 0, Market_town)
  )

# Adding distance to market town and indicator of within 5 km of market town
reg_pop = reg_pop %>% 
  left_join(dist_mt, by = "GIS_ID") %>% 
  mutate(
    Within_5km_of_mt = Distance_market_town < 5
  )

# Adding child women ratio
reg_pop = reg_pop %>% 
  mutate(
    Child_women_ratio = (Age_1_4) / (Age_15_24_f + Age_25_34_f + Age_35_44_f),
    Child_women_ratio_migr = (Age_1_4_migr) / (Age_15_24_migr_f + Age_25_34_migr_f + Age_35_44_migr_f)
  ) %>% 
  mutate(
    Child_women_ratio = ifelse(
      is.finite(Child_women_ratio), Child_women_ratio, NA
    ),
    Child_women_ratio_migr = ifelse(
      is.finite(Child_women_ratio_migr), Child_women_ratio_migr, NA
    )
  )

# ==== Save data ====
write_csv2(reg_pop, "Data/Pop_reg.csv")
