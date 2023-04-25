# Adding occupations
#
# Date updated:   2023-04-17
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
set.seed(20)
hisco %>% 
  sample_n(1000) %>% 
  write_csv2("Data/HISCO_to_check.csv")

# Check occupational observations
hisco %>% 
  group_by(Year) %>% 
  summarise(
    NA_Occupation = sum(is.na(Occupation)),
    NA_Household_position = sum(is.na(Household_position)),
    NA_both = sum(is.na(Occupation) & is.na(Household_position))
  )


# ==== Occupational categories ====
# Interesting occupations from top 100 occ: 
occ_cats = list(
  Fishing = c(
    "64100" # 17522	Fisherman, DeepSea or Inland and Coastal Water
  ), 
  
  Manufacturing = c(
    "75400", # 30211	Weaver, Specialisation Unknown
    "83110", # 28464	Blacksmith, General
    "79100", # 27631	Tailor, Specialisation Unknown
    "79510", # 27555	Hand and Machine Sewer, General
    "80110", # 22888	Shoemaker, General
    "81000", # 22412	Woodworker, Specialisation Unknown
    "77120", # 14526	Grain Miller
    "75000", # 11277	Spinners, Weavers, Knitters, Dyers and Related Workers, Specialisation Unknown
    "81990", # 10542	Other Cabinetmakers and Related Woodworkers
    "75220", # 9758	Spinner, Thread and Yarn
    "81925", # 6996	Cartwright
    "77610", # 6994	Baker, General
    "77310", # 6580	Butcher, General
    "81930", # 6257	Cooper
    "80320", # 4208	Saddler and Harness Maker
    "84222", # 2782	Watch and Clock Assembler or Repairer
    "77510", # 2603	Dairy Product Processor, General
    "75500", # 2334	Knitter, Specialisation Unknown
    "81230", # 2250	Wood Turner
    "89240", # 2041	Brick and Tile Moulder (Hand)
    "75710", # 1962	Rope Maker, General
    "89210", # 1648	Potter, General
    "87340", # 1524	Tinsmith
    "77810", # 1516	Brewer, General
    "96930", # 1355	Boiler Fireman (Fyrbøder)
    "92110", # 1343	Printer, General
    "76145", # 1285	Tanner
    "72550", # 1211	Coremaker (Hand)
    "17140", # 1051	Instrumentalist
    "84970", # 942 Plant Maintenance Mechanic
    "79475", # 939 Glove Cutter, Leather or Other Material
    "74490", # 932	Other Still and Reactor Operators
    "56070", # 902	Presser (Hand)
    "92625", # 885	Bookbinder (Hand or Machine)
    "78200", # 823	Cigar Maker, Specialisation Unknown
    "21220", # 805	Production Manager (except Farm)
    "83990", # 793	Other Blacksmiths, Toolmakers and MachineTool Operators Not Elsewhere Classified
    "88050" # 784	Goldsmith and Silversmith
  ),
  
  Farmer = c(
    "62120", # 612195	Farm Servant
    "61100", # 337582 General Farmer
    "62105", # 158442	FarmWorker, General
    "61115", # 62854 Small Subsistence Farmer (Husbandman)
    "62450", # 6888	FurBearing Animal Farm Worker
    "62400", # 2392	Livestock Workers
    "63220", # 2280	Forest Supervisor
    "63110", # 2094	Logger (General)
    "62510" # 2071	Dairy Farm Worker, General
  )
)


hisco0 = hisco
occ_cats = foreach(i = 1:length(occ_cats), .combine = "bind_rows") %do% {
  name_i = names(occ_cats)[i]
  tmp_i = data.frame(
    Category = name_i,
    hisco = occ_cats[[i]]
  )

  # Is this category in the actual HISCO codes in census?
  # There are up to 5 HISCO codes for each individual
  # A dataframe containing dummies for all 5 potential HISCOs is created
  # Next it is coerced into one dataframe
  category_dummies = data.frame(
    cat1 = hisco %>%
      mutate(hisco1 = as.character(hisco1)) %>%
      left_join(tmp_i, by = c("hisco1"="hisco")) %>%
      select(Category) %>%
      mutate(Category = ifelse(is.na(Category), 0, 1)) %>%
      unlist(),

    cat2 = hisco %>%
      mutate(hisco2 = as.character(hisco2)) %>%
      left_join(tmp_i, by = c("hisco2"="hisco")) %>%
      select(Category) %>%
      mutate(Category = ifelse(is.na(Category), 0, 1)) %>%
      unlist(),

    cat3 = hisco %>%
      mutate(hisco3 = as.character(hisco3)) %>%
      left_join(tmp_i, by = c("hisco3"="hisco")) %>%
      select(Category) %>%
      mutate(Category = ifelse(is.na(Category), 0, 1)) %>%
      unlist(),

    cat4 = hisco %>%
      mutate(hisco4 = as.character(hisco4)) %>%
      left_join(tmp_i, by = c("hisco4"="hisco")) %>%
      select(Category) %>%
      mutate(Category = ifelse(is.na(Category), 0, 1)) %>%
      unlist(),

    cat5 = hisco %>%
      mutate(hisco4 = as.character(hisco5)) %>%
      left_join(tmp_i, by = c("hisco5"="hisco")) %>%
      select(Category) %>%
      mutate(Category = ifelse(is.na(Category), 0, 1)) %>%
      unlist()
  )

  # Coerce into one dummy indicating whether the category is present or not
  category_dummies = category_dummies %>%
    transmute(dummy = cat1 + cat2 + cat3 + cat4 + cat5) %>%
    mutate(
      dummy = ifelse(dummy>0, 1, 0)
    )

  names(category_dummies) = name_i
  cat("\n", name_i)

  # Store results to dataframe
  hisco0 = hisco0 %>%
    bind_cols(category_dummies)

}

hisco = hisco0
rm(hisco0)
rm(category_dummies)

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
    Farmer = sum(Farmer, na.rm = TRUE)/n(),
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


