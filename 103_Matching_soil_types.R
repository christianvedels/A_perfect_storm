# Matching GIS_ID based on soil types
# Date updated:   2023-04-29
# Auhtor:         Christian Vedel 
#
# Output:         'Matched_GIS_ID.csv'


# ==== Libraries ====
library(tidyverse)
source("000_Functions.R")
library(caret)
library(foreach)

# ==== Load data ====
geo_data = read_csv2("Data/Geo.csv", guess_max = 2000)
soil = read_csv2("Data/Parish_soil.csv", guess_max = 20000)

# ==== Data for estimation ====
# Make limfjord dummies
geo_data = geo_data %>% 
  fastDummies::dummy_columns("limfjord_placement")

# How often does the soil type occur
soil_frequency = soil %>% 
  filter(pct > 0) %>% 
  group_by(SOIL_TYPE) %>% 
  count()

# Filter such that it is only soil types appearing in more than 10% 
# of parishes
p10 = (geo_data %>% NROW()*0.10) %>% round()
soil_frequency = soil_frequency %>% 
  filter(n>p10)

soil = soil %>% filter(SOIL_TYPE %in% soil_frequency$SOIL_TYPE)  

# Make soil data wide
soil = soil %>% 
  mutate(
    SOIL_TYPE = sub_scandi(SOIL_TYPE)
  ) %>% 
  pivot_wider(names_from = SOIL_TYPE, values_from = pct)

# Limit geo data to west limfjord and reference
geo_data = geo_data %>% 
  filter(limfjord_placement %in% c("west", "not"))

# Join data
reg_data = geo_data %>% 
  select(GIS_ID, limfjord_placement_west) %>% 
  left_join(soil, by = "GIS_ID") %>% 
  mutate(limfjord_placement_west = factor(limfjord_placement_west, levels = c("0", "1"))) %>% 
  select(-area_parish)

# ==== Run estimator of propensity ====
set.seed(20)
# Define the cross-validation parameters
ctrl = trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Define the XGBoost model
Model = train(
  x = reg_data %>% select(-GIS_ID, -limfjord_placement_west) %>% data.frame(), 
  y = reg_data %>% select(limfjord_placement_west) %>% unlist(),
  method = "xgbTree", 
  trControl = ctrl,  # Cross-validation control
  verbose = TRUE
  )

# Print the results
plot(Model)

propensity = Model %>% predict(
  newdata = reg_data %>% select(-GIS_ID, -limfjord_placement_west) %>% data.frame(),
  type = "prob"
)

# ==== Matching ====
reg_data$propensity = propensity[,2]

reg_data %>% 
  ggplot(aes(propensity, fill = limfjord_placement_west)) + geom_density(alpha = 0.5)

set.seed(20)
west_GIS_ID = reg_data %>% 
  filter(limfjord_placement_west == 1) %>% 
  # Shuffle data
  sample_frac(1)

donors = reg_data %>% 
  filter(limfjord_placement_west == 0)

# Loop that makes gready matching
matches = foreach(i = west_GIS_ID$GIS_ID, .combine = "bind_rows") %do% {
  prop_i = west_GIS_ID %>% 
    filter(GIS_ID == i) %>% 
    select(GIS_ID, propensity)
  
  sqdif = (prop_i$propensity - donors$propensity)^2
  match_i = donors[which.min(sqdif),] %>% 
    select(GIS_ID, propensity) %>% 
    rename(GIS_ID_match = GIS_ID, propensity_match = propensity)
  
  # Update donors by removing used match
  donors = donors[-which.min(sqdif), ]
  
  # Return results
  data.frame(
    prop_i, match_i
  )
}

# Check new propensity score distribution
matches %>% 
  ggplot(aes(propensity, fill = "1")) + 
  geom_density(alpha = 0.5) + 
  geom_density(aes(propensity_match, fill = "0"), alpha = 0.5)


matched_ids = matches %>% 
  select(GIS_ID, GIS_ID_match) %>% 
  pivot_longer(GIS_ID:GIS_ID_match, values_to = "GIS_ID") %>% 
  select(GIS_ID)

# ==== Reshape ====
matched_ids = matched_ids %>% 
  left_join(matches, by = "GIS_ID") %>% 
  select(GIS_ID, propensity)

matched_ids = matched_ids %>% 
  left_join(matches %>% select(ends_with("_match")), by = c("GIS_ID"="GIS_ID_match"))

matched_ids = matched_ids %>% 
  mutate(
    propensity = ifelse(is.na(propensity), propensity_match, propensity)
  ) %>% 
  select(GIS_ID, propensity)

# Add back Limfjord placement
geo_data = geo_data %>% select(GIS_ID, limfjord_placement)
matched_ids = matched_ids %>% left_join(geo_data, by = "GIS_ID")


# ==== Save results ====
matched_ids %>% write_csv2("Data/Matched_parishes.csv")
