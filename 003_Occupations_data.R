# Adding occupations
#
# Date updated:   2023-04-26
# Auhtor:         Christian Vedel 
# Purpose:        Join on data on HISCO codes and categories
#
# Output:         'merged_data' enriched with HISCO codes + occupational categories
#
# HISCO codes generated from automatic HISCO classifier. 
# See my website for more information: https://sites.google.com/view/christianvedel 

# ==== Libraries ====
library(tidyverse)
library(foreach)
library(fst)

# ==== Load data ====
merged_data = read_fst("Data/tmp_census.fst") 
hisco = read_csv2("Data/LL_hisco_codes_clean.csv") # Available here: 
# https://www.dropbox.com/s/ov7ubxtqq21c6za/LL_hisco_codes_clean.csv?dl=0

# Deleter occ cols if any, to be able to rerun this multiple times:
merged_data = merged_data %>% 
  select(pa_id:Born_different_county)

# ==== Data cleaning ====
hisco = hisco %>% 
  select(pa_id, Year, Erhverv, Stilling_i_husstanden, hisco1:en_hisco_text5) %>% 
  rename(
    Occupation = Erhverv,
    Household_position = Stilling_i_husstanden,
  ) %>% 
  mutate(
    Year = as.character(Year),
    pa_id = as.character(pa_id)
  )

# Check uniqueness of ids
# hisco %>% group_by(Year, pa_id) %>% count() %>% filter(n>1)

# Check data quality in 1000 random subsamples
# set.seed(20)
# hisco %>% 
#   sample_n(1000) %>% 
#   write_csv2("Data/HISCO_to_check.csv")

# Check occupational observations
hisco %>% 
  group_by(Year) %>% 
  summarise(
    NA_Occupation = sum(is.na(Occupation)),
    NA_Household_position = sum(is.na(Household_position)),
    NA_both = sum(is.na(Occupation) & is.na(Household_position))
  )

# ==== Occupational categories ====
# Top 100 extracted
tmp = hisco %>%
  select(hisco1:hisco5) %>%
  pivot_longer(hisco1:hisco5) %>%
  drop_na(value)

hisco_desc = hisco::hisco %>%
  distinct(hisco, en_hisco_text)
   
tmp = tmp %>%
  select(value) %>%
  rename(hisco = value) %>%
  group_by(hisco) %>%
  count()

all_occs = tmp$n %>% sum()
# tmp %>% 
#   left_join(hisco_desc, by = "hisco") %>% 
#   arrange(-n) %>% 
#   ungroup() %>% 
#   top_n(100) %>% 
#   write_csv2("Data/Tmp_top100_hiscos.csv")

top100 = read_csv2("Data/Top100_HISCO.csv")

# How much is covered by these categories?
top100 %>% 
  mutate(pct = n/all_occs) %>% 
  group_by(Category) %>% 
  summarise(pct = sum(pct))

# # A tibble: 7 × 2
# Category          pct
# <chr>           <dbl>
#   1 building      0.0259 
# 2 farming       0.551  
# 3 fishing       0.00579
# 4 manufacturing 0.149  
# 5 merchant      0.0255 
# 6 seamen        0.0188 
# 7 NA            0.198 

top100 = top100 %>% 
  select(hisco, Category)

hisco = hisco %>% 
  left_join(top100, by = c(hisco1 = "hisco")) %>% 
  rename(
    Category1 = Category
  ) %>% 
  left_join(top100, by = c(hisco2 = "hisco")) %>% 
  rename(
    Category2 = Category
  ) %>% 
  left_join(top100, by = c(hisco3 = "hisco")) %>% 
  rename(
    Category3 = Category
  ) %>% 
  left_join(top100, by = c(hisco4 = "hisco")) %>% 
  rename(
    Category4 = Category
  ) %>% 
  left_join(top100, by = c(hisco5 = "hisco")) %>% 
  rename(
    Category5 = Category
  )


hisco = hisco %>% 
  rowwise() %>% 
  mutate(
    Farming = ifelse(
      "farming" %in% c(Category1, Category2, Category3, Category4, Category5), 1, 0
    ),
    Manufacturing = ifelse(
      "manufacturing" %in% c(Category1, Category2, Category3, Category4, Category5), 1, 0
    ),
    Seamen = ifelse(
      "seamen" %in% c(Category1, Category2, Category3, Category4, Category5), 1, 0
    ),
    Fishing = ifelse(
      "fishing" %in% c(Category1, Category2, Category3, Category4, Category5), 1, 0
    ),
    Building = ifelse(
      "building" %in% c(Category1, Category2, Category3, Category4, Category5), 1, 0
    ),
    Merchants = ifelse(
      "merchant" %in% c(Category1, Category2, Category3, Category4, Category5), 1, 0
    )
  )


hisco = hisco %>% select(-c(Category1:Category5))

# ==== Merge on HISCO codes and categories ====
# Merge on HISCO codes
merged_data0 = merged_data %>% 
  left_join(hisco, by = c("Year", "pa_id"))

# ==== Simple descriptive as a santity check ====
merged_data0 %>%  
  group_by(Year) %>% 
  summarise(
    Fishing = sum(Fishing, na.rm = TRUE)/n(),
    Manufacturing = sum(Manufacturing, na.rm = TRUE)/n(),
    Farming = sum(Farming, na.rm = TRUE)/n(),
    Building = sum(Building, na.rm = TRUE)/n(),
    Merchants = sum(Merchants, na.rm = TRUE)/n(),
    Seamen = sum(Seamen, na.rm = TRUE)/n(),
    n = n()
  )

merged_data = merged_data0
rm(merged_data0)

# ==== 0-9 first digit HISCO ====
# This part takes a long time to run

fix_hisco = function(x){
  x = as.character(x)
  x = ifelse(nchar(x)==4, paste0("0", x), x)
  return(x)
}

merged_data = merged_data %>% 
  # sample_n(1000) %>% 
  # Fix HISCO codes to char
  mutate_at(vars(starts_with("hisco")), fix_hisco) %>% 
  mutate(unique_hiscos = apply(.[, !grepl("^en_hisco_text", names(.)) & grepl("^hisco", names(.))], 1, function(x) unique(substr(x, 1, 1)))) %>% 
  mutate(
    hisco_1st_digit0 = as.numeric(grepl("0", unique_hiscos)),
    hisco_1st_digit1 = as.numeric(grepl("1", unique_hiscos)),
    hisco_1st_digit2 = as.numeric(grepl("2", unique_hiscos)),
    hisco_1st_digit3 = as.numeric(grepl("3", unique_hiscos)),
    hisco_1st_digit4 = as.numeric(grepl("4", unique_hiscos)),
    hisco_1st_digit5 = as.numeric(grepl("5", unique_hiscos)),
    hisco_1st_digit6 = as.numeric(grepl("6", unique_hiscos)),
    hisco_1st_digit7 = as.numeric(grepl("7", unique_hiscos)),
    hisco_1st_digit8 = as.numeric(grepl("8", unique_hiscos)),
    hisco_1st_digit9 = as.numeric(grepl("9", unique_hiscos))
  ) %>% select(-unique_hiscos)


# ==== Saving data enriched data ====
write_fst(merged_data, "Data/tmp_census.fst", compress = 0)


