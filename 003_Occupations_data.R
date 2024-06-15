# Adding occupations
#
# Date updated:   2024-06-15
# Auhtor:         Christian Vedel 
# Purpose:        Join on data on HISCO codes and categories
#
# Output:         'merged_data' enriched with HISCO codes + occupational categories
#
# HISCO codes generated from automatic HISCO classifier. 
# See https://arxiv.org/abs/2402.13604 

# ==== Libraries ====
library(tidyverse)
library(foreach)
library(fst)
library(dataverse)

# ==== Set dataverse env ====
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

# ==== Load data ====
merged_data = read_fst("Data/tmp_census.fst") 
hisco = get_dataframe_by_name(
  filename = "Census_HISCO_codes_clean.csv",
  dataset = "10.7910/DVN/WZILNI", # DOI
  server = "dataverse.harvard.edu",
  .f = function(x) read_csv(x) # Function to read the file
)

# Extract GIS_ID/RowID key for other projects
tmp = merged_data %>% 
  distinct(GIS_ID, RowID) %>% 
  drop_na(GIS_ID)

tmp %>% 
  write_csv("Data/RowID_GIS_ID_key.csv")

# Delete occ cols if any, to be able to rerun this multiple times:
merged_data = merged_data %>% 
  select(pa_id:Born_different_county)

# ==== Data cleaning ====
# 'Year' from 'Kilde'
x = hisco$Kilde %>% unique()

hisco = hisco %>% 
  mutate(Year = gsub("[^0-9]", "", Kilde))

hisco = hisco %>% 
  select(pa_id, Year, Erhverv, Stilling_i_husstanden, hisco_1:desc_5) %>% 
  rename(
    Occupation = Erhverv,
    Household_position = Stilling_i_husstanden,
  ) %>% 
  mutate(
    Year = as.character(Year),
    pa_id = as.character(pa_id)
  )

# Check uniqueness of ids
hisco %>% group_by(Year, pa_id) %>% count() %>% filter(n>1)

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

# ==== Merge on HISCO codes and categories ====
# Merge on HISCO codes
merged_data0 = merged_data %>% 
  left_join(hisco, by = c("Year", "pa_id"))
merged_data = merged_data0
rm(merged_data0)

# ==== 0-9 first digit HISCO ====
# This part takes a long time to run

fix_hisco = function(x){
  x = as.character(x)
  x = ifelse(nchar(x)==4, paste0("0", x), x)
  return(x)
}

merged_data0 = merged_data %>% 
  # sample_n(1000) %>%
  # Fix HISCO codes to char
  mutate_at(vars(starts_with("hisco")), fix_hisco) %>% 
  mutate(unique_hiscos = apply(.[, !grepl("^en_hisco_text", names(.)) & grepl("^hisco", names(.))], 1, function(x) unique(substr(x, 1, 1)))) %>% 
  mutate(unique_hiscos_2digit = apply(.[, !grepl("^en_hisco_text", names(.)) & grepl("^hisco", names(.))], 1, function(x) unique(substr(x, 1, 2)))) %>% 
  mutate(unique_hiscos_3digit = apply(.[, !grepl("^en_hisco_text", names(.)) & grepl("^hisco", names(.))], 1, function(x) unique(substr(x, 1, 3))))

# First digit
merged_data0 = merged_data0 %>% 
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

# Find the HISCO codes that appear in the data
appeared_hiscos = sort(unique(unlist(merged_data0$unique_hiscos_2digit)))

# Second digit
for (i in 0:99) {
  # Skip iterations if 'i' is not in appeared_hiscos
  if (!(sprintf("%02d", i) %in% appeared_hiscos)){
    next
  }
  
  cat(i, "         \r")
  col_name = paste0("hisco_2nd_digit", sprintf("%02d", i))
  merged_data0[col_name] <- as.numeric(grepl(sprintf("%02d", i), merged_data0$unique_hiscos_2digit))
}

# Find the HISCO codes that appear in the data
appeared_hiscos = sort(unique(unlist(merged_data0$unique_hiscos_3digit)))

# Third digit
for (i in 0:999) {
  # Skip iterations if 'i' is not in appeared_hiscos
  if (!(sprintf("%03d", i) %in% appeared_hiscos)){
    next
  }
    
  cat(i, "         \r")
  col_name = paste0("hisco_3rd_digit", sprintf("%03d", i))
  merged_data0[col_name] <- as.numeric(grepl(sprintf("%03d", i), merged_data0$unique_hiscos_3digit))
}


# Delete temporary variables
merged_data = merged_data0 %>% 
  select(-unique_hiscos_2digit) %>% 
  select(-unique_hiscos_3digit)

# ==== Saving data enriched data ====
write_fst(merged_data, "Data/tmp_census.fst", compress = 0)


