# Aggregate demographic data
#
# Date updated:   2024-06-16
# Auhtor:         Christian Vedel 
# Purpose:        Aggregate data to parish level
#
# Output:         'Popdata.csv', demographic data aggregated to parish level 

# ==== Libraries ====
library(tidyverse)
library(fst)
library(foreach)
source("000_Functions.R")

# Not sufficient memory on 32GB RAM laptop. Therefore the rather janky (but functional) loop
years = c("1787", "1801", "1834", "1840", "1845", "1850", "1860", "1880", "1901")
for(y in years){
  # ==== Load data ====
  merged_data = read_fst("Data/tmp_census.fst") 
  merged_data = merged_data %>% 
    filter(Year==y)
  
  # ==== Misc functions ====
  # Small functions used in this script in particular
  Age_cats = function(age, lower, upper){
    x = ifelse(age < upper & age >= lower, 1, 0)
    sum(x, na.rm = TRUE)
  }
  
  # ==== Function to aggregrate data ====
  aggregate_data = function(data, suffix = "") {
    data %>%
      group_by(Year, GIS_ID) %>%
      mutate(age = as.numeric(age)) %>%
      summarise(
        Pop = n(),
        Age_mean = mean(age, na.rm = TRUE),
        Age_0_1 = Age_cats(age, 0, 1),
        Age_1_4 = Age_cats(age, 1, 5),
        Age_5_14 = Age_cats(age, 5, 15),
        Age_15_24 = Age_cats(age, 15, 25),
        Age_25_34 = Age_cats(age, 25, 35),
        Age_35_44 = Age_cats(age, 35, 45),
        Age_45_54 = Age_cats(age, 45, 55),
        Age_55_64 = Age_cats(age, 55, 65),
        Age_65_125 = Age_cats(age, 65, 125),
        Age_max = max(age, na.rm = TRUE),
        Age_min = min(age, na.rm = TRUE),
        Age_q005 = quantile(age, probs = 0.05, na.rm = TRUE),
        Age_q025 = quantile(age, probs = 0.25, na.rm = TRUE),
        Age_median = quantile(age, probs = 0.50, na.rm = TRUE),
        Age_q075 = quantile(age, probs = 0.75, na.rm = TRUE),
        Age_q095 = quantile(age, probs = 0.95, na.rm = TRUE),
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
        no_occupation_in_prime = sum(is.na(hisco_1) & age >= 25 & age < 55)
      ) %>%
      mutate(
        prime_labor_age = Age_25_34 + Age_35_44 + Age_45_54,
        occupation_in_prime = prime_labor_age - no_occupation_in_prime
      ) %>%
      select(-no_occupation_in_prime) %>%
      rename_with(~ paste0(., suffix), -c(Year, GIS_ID))
  }
  
  # ==== Summarise at parish level ====
  # Aggregate data
  agg_data = merged_data %>% 
    aggregate_data()
  
  write_fst(agg_data, "Data/tmp_agg_data.fst", compress = 0)
  rm(agg_data)
  
  agg_data_m = merged_data %>% 
    filter(sex == "m") %>% 
    aggregate_data("_m")
  
  write_fst(agg_data_m, "Data/tmp_agg_data_m.fst", compress = 0)
  rm(agg_data_m)
  
  agg_data_f = merged_data %>% 
    filter(sex == "f") %>% 
    aggregate_data("_f")
  
  write_fst(agg_data_f, "Data/tmp_agg_data_f.fst", compress = 0)
  rm(agg_data_f)
  
  # ==== Aggregate data for people born in different county ====
  agg_data_migr = merged_data %>% 
    filter(Born_different_county == 1) %>% 
    aggregate_data("_migr")
  
  write_fst(agg_data_migr, "Data/tmp_agg_data_migr.fst", compress = 0)
  rm(agg_data_migr)
  
  agg_data_migr_m = merged_data %>% 
    filter(sex == "m" & Born_different_county == 1) %>% 
    aggregate_data("_migr_m")
  
  write_fst(agg_data_migr_m, "Data/tmp_agg_data_migr_m.fst", compress = 0)
  rm(agg_data_migr_m)
  
  agg_data_migr_f = merged_data %>% 
    filter(sex == "f" & Born_different_county == 1) %>% 
    aggregate_data("_migr_f")
  
  write_fst(agg_data_migr_f, "Data/tmp_agg_data_migr_f.fst", compress = 0)
  rm(agg_data_migr_f)
  
  # ==== 2nd and 3rd digit of HISCOs ====
  aggregate_hisco_data = function(data, cols, suffix = "") {
    data %>%
      group_by(Year, GIS_ID) %>%
      select(all_of(cols)) %>%
      summarise_all(sum0) %>%
      select(where(~ any(sum_special(.) != 0))) %>%
      rename_with(~ paste0(., suffix), -c(Year, GIS_ID))
  }
  
  hisco_2nd_all = aggregate_hisco_data(merged_data, names(merged_data)[grep("hisco_2nd_digit", names(merged_data))])
  write_fst(hisco_2nd_all, "Data/tmp_hisco_2nd_all.fst", compress = 0)
  rm(hisco_2nd_all)
  
  hisco_2nd_m = aggregate_hisco_data(merged_data %>% filter(sex == "m"), names(merged_data)[grep("hisco_2nd_digit", names(merged_data))], "_m")
  write_fst(hisco_2nd_m, "Data/tmp_hisco_2nd_m.fst", compress = 0)
  rm(hisco_2nd_m)
  
  hisco_2nd_f = aggregate_hisco_data(merged_data %>% filter(sex == "f"), names(merged_data)[grep("hisco_2nd_digit", names(merged_data))], "_f")
  write_fst(hisco_2nd_f, "Data/tmp_hisco_2nd_f.fst", compress = 0)
  rm(hisco_2nd_f)
  
  hisco_3rd_all = aggregate_hisco_data(merged_data, names(merged_data)[grep("hisco_3rd_digit", names(merged_data))])
  write_fst(hisco_3rd_all, "Data/tmp_hisco_3rd_all.fst", compress = 0)
  rm(hisco_3rd_all)
  
  hisco_3rd_m = aggregate_hisco_data(merged_data %>% filter(sex == "m"), names(merged_data)[grep("hisco_3rd_digit", names(merged_data))], "_m")
  write_fst(hisco_3rd_m, "Data/tmp_hisco_3rd_m.fst", compress = 0)
  rm(hisco_3rd_m)
  
  hisco_3rd_f = aggregate_hisco_data(merged_data %>% filter(sex == "f"), names(merged_data)[grep("hisco_3rd_digit", names(merged_data))], "_f")
  write_fst(hisco_3rd_f, "Data/tmp_hisco_3rd_f.fst", compress = 0)
  rm(hisco_3rd_f)
  
  # ==== Join data together ====
  agg_data = read_fst("Data/tmp_agg_data.fst") %>%
    left_join(read_fst("Data/tmp_agg_data_m.fst"), by = c("Year", "GIS_ID")) %>%
    left_join(read_fst("Data/tmp_agg_data_f.fst"), by = c("Year", "GIS_ID")) %>%
    left_join(read_fst("Data/tmp_hisco_2nd_all.fst"), by = c("Year", "GIS_ID")) %>%
    left_join(read_fst("Data/tmp_hisco_2nd_m.fst"), by = c("Year", "GIS_ID")) %>%
    left_join(read_fst("Data/tmp_hisco_2nd_f.fst"), by = c("Year", "GIS_ID")) %>%
    left_join(read_fst("Data/tmp_hisco_3rd_all.fst"), by = c("Year", "GIS_ID")) %>%
    left_join(read_fst("Data/tmp_hisco_3rd_m.fst"), by = c("Year", "GIS_ID")) %>%
    left_join(read_fst("Data/tmp_hisco_3rd_f.fst"), by = c("Year", "GIS_ID")) %>%
    left_join(read_fst("Data/tmp_agg_data_migr.fst"), by = c("Year", "GIS_ID")) %>%
    left_join(read_fst("Data/tmp_agg_data_migr_m.fst"), by = c("Year", "GIS_ID")) %>%
    left_join(read_fst("Data/tmp_agg_data_migr_f.fst"), by = c("Year", "GIS_ID")) %>%
    ungroup()
  
  write_fst(agg_data, paste0("Data/agg_data_y", y,".fst"), compress = 0)
}

agg_data = foreach(y = years, .combine = "bind_rows") %do% {
  read_fst(paste0("Data/agg_data_y", y,".fst"))
}

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

agg_data %>% 
  ggplot(aes(Age_mean, fill = Year)) + 
  geom_density(alpha = 0.5) + 
  facet_wrap(~Year, ncol = 1)


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
  