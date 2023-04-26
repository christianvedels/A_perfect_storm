# Aggregate demographic data
#
# Date updated:   2023-04-19
# Auhtor:         Christian Vedel 
# Purpose:        Aggregate data to parish level
#
# Output:         'Popdata.csv', demographic data aggregated to parish level 
#

# ==== Libraries ====
library(tidyverse)
library(fst)

# ==== Load data ====
merged_data = read_fst("Data/tmp_census.fst") 

# ==== Misc functions ====
# Small functions used in this script in particular
Age_cats = function(age, lower, upper){
  x = ifelse(age < upper & age >= lower, 1, 0)
  sum(x, na.rm = TRUE)
}

# ==== Summarise at parish level ====
# - Population 
# - Age groups
# - Occupational groups
# - HISCO first digit
# - Males
# - Females
# - FLFP

agg_data = merged_data %>% 
  group_by(Year, GIS_ID) %>% 
  mutate(age = as.numeric(age)) %>% 
  summarise(
    Pop = n(),
    Age_0_1 = Age_cats(age, 0, 1),
    Age_1_4 = Age_cats(age, 1, 5),
    Age_5_14 = Age_cats(age, 5, 15),
    Age_15_24 = Age_cats(age, 15, 25),
    Age_25_34 = Age_cats(age, 25, 35),
    Age_35_44 = Age_cats(age, 35, 45),
    Age_45_54 = Age_cats(age, 45, 55),
    Age_55_64 = Age_cats(age, 55, 65),
    Age_65_125 = Age_cats(age, 65, 125),
    Fishing = sum(Fishing, na.rm = TRUE),
    Manufacturing = sum(Manufacturing, na.rm = TRUE),
    Farming = sum(Farming, na.rm = TRUE),
    Building = sum(Building, na.rm = TRUE),
    Merchants = sum(Merchants, na.rm = TRUE),
    Born_different_county = sum(Born_different_county, na.rm = TRUE),
    hisco_1st_digit0 = sum(hisco_1st_digit0),
    hisco_1st_digit1 = sum(hisco_1st_digit1),
    hisco_1st_digit2 = sum(hisco_1st_digit2),
    hisco_1st_digit3 = sum(hisco_1st_digit3),
    hisco_1st_digit4 = sum(hisco_1st_digit4),
    hisco_1st_digit5 = sum(hisco_1st_digit5),
    hisco_1st_digit6 = sum(hisco_1st_digit6),
    hisco_1st_digit7 = sum(hisco_1st_digit7),
    hisco_1st_digit8 = sum(hisco_1st_digit8),
    hisco_1st_digit9 = sum(hisco_1st_digit9),
    no_occupation_in_prime = sum(is.na(hisco1) & age >= 25 & age < 55)
  ) %>% 
  mutate(
    prime_labor_age = Age_25_34 + Age_35_44 + Age_45_54
  ) %>% 
  mutate(
    occupation_in_prime = prime_labor_age - no_occupation_in_prime
  ) %>% 
  select(-no_occupation_in_prime)

agg_data_m = merged_data %>% 
  filter(sex == "m") %>% 
  group_by(Year, GIS_ID) %>% 
  mutate(age = as.numeric(age)) %>% 
  summarise(
    Pop = n(),
    Age_0_1 = Age_cats(age, 0, 1),
    Age_1_4 = Age_cats(age, 1, 5),
    Age_5_14 = Age_cats(age, 5, 15),
    Age_15_24 = Age_cats(age, 15, 25),
    Age_25_34 = Age_cats(age, 25, 35),
    Age_35_44 = Age_cats(age, 35, 45),
    Age_45_54 = Age_cats(age, 45, 55),
    Age_55_64 = Age_cats(age, 55, 65),
    Age_65_125 = Age_cats(age, 65, 125),
    Fishing = sum(Fishing, na.rm = TRUE),
    Manufacturing = sum(Manufacturing, na.rm = TRUE),
    Farming = sum(Farming, na.rm = TRUE),
    Building = sum(Building, na.rm = TRUE),
    Merchants = sum(Merchants, na.rm = TRUE),
    Born_different_county = sum(Born_different_county, na.rm = TRUE),
    hisco_1st_digit0 = sum(hisco_1st_digit0),
    hisco_1st_digit1 = sum(hisco_1st_digit1),
    hisco_1st_digit2 = sum(hisco_1st_digit2),
    hisco_1st_digit3 = sum(hisco_1st_digit3),
    hisco_1st_digit4 = sum(hisco_1st_digit4),
    hisco_1st_digit5 = sum(hisco_1st_digit5),
    hisco_1st_digit6 = sum(hisco_1st_digit6),
    hisco_1st_digit7 = sum(hisco_1st_digit7),
    hisco_1st_digit8 = sum(hisco_1st_digit8),
    hisco_1st_digit9 = sum(hisco_1st_digit9),
    no_occupation_in_prime = sum(is.na(hisco1) & age >= 25 & age < 55)
  ) %>% 
  mutate(
    prime_labor_age = Age_25_34 + Age_35_44 + Age_45_54
  ) %>% 
  mutate(
    occupation_in_prime = prime_labor_age - no_occupation_in_prime
  ) %>% 
  select(-no_occupation_in_prime)

agg_data_f = merged_data %>% 
  filter(sex == "f") %>% 
  group_by(Year, GIS_ID) %>% 
  mutate(age = as.numeric(age)) %>% 
  summarise(
    Pop = n(),
    Age_0_1 = Age_cats(age, 0, 1),
    Age_1_4 = Age_cats(age, 1, 5),
    Age_5_14 = Age_cats(age, 5, 15),
    Age_15_24 = Age_cats(age, 15, 25),
    Age_25_34 = Age_cats(age, 25, 35),
    Age_35_44 = Age_cats(age, 35, 45),
    Age_45_54 = Age_cats(age, 45, 55),
    Age_55_64 = Age_cats(age, 55, 65),
    Age_65_125 = Age_cats(age, 65, 125),
    Fishing = sum(Fishing, na.rm = TRUE),
    Manufacturing = sum(Manufacturing, na.rm = TRUE),
    Farming = sum(Farming, na.rm = TRUE),
    Building = sum(Building, na.rm = TRUE),
    Merchants = sum(Merchants, na.rm = TRUE),
    Born_different_county = sum(Born_different_county, na.rm = TRUE),
    hisco_1st_digit0 = sum(hisco_1st_digit0),
    hisco_1st_digit1 = sum(hisco_1st_digit1),
    hisco_1st_digit2 = sum(hisco_1st_digit2),
    hisco_1st_digit3 = sum(hisco_1st_digit3),
    hisco_1st_digit4 = sum(hisco_1st_digit4),
    hisco_1st_digit5 = sum(hisco_1st_digit5),
    hisco_1st_digit6 = sum(hisco_1st_digit6),
    hisco_1st_digit7 = sum(hisco_1st_digit7),
    hisco_1st_digit8 = sum(hisco_1st_digit8),
    hisco_1st_digit9 = sum(hisco_1st_digit9),
    no_occupation_in_prime = sum(is.na(hisco1) & age >= 25 & age < 55)
  ) %>% 
  mutate(
    prime_labor_age = Age_25_34 + Age_35_44 + Age_45_54
  ) %>% 
  mutate(
    occupation_in_prime = prime_labor_age - no_occupation_in_prime
  ) %>% 
  select(-no_occupation_in_prime)
  

# Merge together data for all and male / female
agg_data = agg_data %>% 
  left_join(agg_data_m, by = c("Year", "GIS_ID"), suffix = c("","_m")) %>% 
  left_join(agg_data_f, by = c("Year", "GIS_ID"), suffix = c("","_f")) %>% 
  ungroup()

# ==== Plots for sanity check ====
agg_data %>% 
  pivot_longer(Age_0_1:Age_65_125) %>% 
  group_by(Year, name) %>% 
  summarise(
    Pop = sum(Pop, na.rm = TRUE),
    value = sum(value, na.rm = TRUE)
  ) %>% 
  mutate(
    share = value / Pop
  ) %>% 
  mutate(Year = as.numeric(as.character(Year))) %>% 
  ggplot(aes(Year, share, fill = name)) + geom_area(alpha = 0.5, col = "black") +
  scale_fill_brewer(palette = "Blues") + 
  theme_bw()

agg_data_m %>% 
  pivot_longer(Age_0_1:Age_65_125) %>% 
  group_by(Year, name) %>% 
  summarise(
    Pop = sum(Pop, na.rm = TRUE),
    value = sum(value, na.rm = TRUE)
  ) %>% 
  mutate(
    share = value / Pop
  ) %>% 
  mutate(Year = as.numeric(as.character(Year))) %>% 
  ggplot(aes(Year, share, fill = name)) + geom_area(alpha = 0.5, col = "black") +
  scale_fill_brewer(palette = "Blues") + 
  theme_bw()

agg_data_f %>% 
  pivot_longer(Age_0_1:Age_65_125) %>% 
  group_by(Year, name) %>% 
  summarise(
    Pop = sum(Pop, na.rm = TRUE),
    value = sum(value, na.rm = TRUE)
  ) %>% 
  mutate(
    share = value / Pop
  ) %>% 
  mutate(Year = as.numeric(as.character(Year))) %>% 
  ggplot(aes(Year, share, fill = name)) + geom_area(alpha = 0.5, col = "black") +
  scale_fill_brewer(palette = "Blues") + 
  theme_bw()


# Labor force
agg_data %>% 
  pivot_longer(
    Age_0_1:occupation_in_prime_f
  ) %>% 
  filter(
    name %in% c(
      "prime_labor_age", 
      "prime_labor_age_m", 
      "prime_labor_age_f", 
      "occupation_in_prime", 
      "occupation_in_prime_m", 
      "occupation_in_prime_f"
    )
  ) %>% 
  mutate(share = value / Pop) %>% 
  drop_na(GIS_ID) %>% 
  group_by(Year, name) %>% 
  summarise(
    share = mean(share, na.rm = TRUE)
  ) %>% 
  mutate(Year = as.numeric(as.character(Year))) %>% 
  ggplot(aes(Year, share, col = name)) + 
  geom_line(size = 2)


# ==== Consistent parishes ====
# Dummy indicating parishes observed throughout
agg_data %>% 
  group_by(Year) %>% 
  count()

Years_in_data = agg_data$Year %>% unique()
consistent = agg_data %>% 
  group_by(GIS_ID) %>% 
  count() %>% 
  filter(n < length(Years_in_data)) %>% 
  select(-n)

agg_data = agg_data %>% 
  mutate(
    consistent = as.numeric(!GIS_ID %in% consistent$GIS_ID)
  )
  
# Check if it works
agg_data %>% 
  filter(
    consistent == 1
  ) %>% 
  group_by(Year) %>% 
  count()

# ==== Save data ====
agg_data %>% 
  write_csv2("Data/Popdata.csv")
  