# Reading census data
#
# Date updated:   2023-04-13
# Auhtor:         Christian Vedel 
# Purpose:        This script reads csv-files containing census data and merges them
#
# Output:         'merged_data' and 'merged_data_raw', which are both a merge of Link Lives data

# ==== Toydata ====
# If TRUE only limited data will be loaded as toydata
toyrun = FALSE

# ==== Libraries ====
library(tidyverse)
source("000_functions.R")
library(foreach)
library(fst)

# ==== Loading standardized data ====
# How much to load?
if(toyrun){
  n_load = 10000
} else {
  n_load = -1
}

all_dirs = list.dirs("../Link lives census") # Download at https://www.rigsarkivet.dk/udforsk/link-lives-data/
all_dirs = all_dirs[grep("standardized_sources/census/", all_dirs)]

the_data_std = list()

for(i in all_dirs){
  
  Year = as.numeric(substrRight(i, 4))
  
  files_to_read = data.frame(fname = list.files(i)) %>% 
    mutate(tmp = substrRight(fname, 4)) %>% 
    filter(tmp == ".csv") %>% select(-tmp)
  
  for(j in files_to_read$fname){
    cat("\n\nReading",i,j,"                                           \n")
    fname_ij = paste(sep = "/",i,j)
    suppressWarnings({
      suppressMessages({
        data_ij = read_csv(fname_ij, n_max = 10)
      })
    })
    c_class = rep("c", NCOL(data_ij))
    data_ij = read_csv(
      fname_ij, 
      n_max = n_load,
      col_types = c_class
    )
    data_ij$Year = unique(Year)
    data_ij$fname = fname_ij
    
    data_ij = data_ij %>% mutate_all(as.character)
    
    # names(data_ij) = tolower(names(data_ij))
    # names(data_ij) = sub_scandi(names(data_ij))
    
    the_message = paste(
      sep = "",
      "\n",
      "Found NROW = ", NROW(data_ij)/1000000, " mio.", " and the following columns:\n",
      paste(names(data_ij), collapse = "\n")
    )
    
    # Give id
    data_ij = data_ij %>% 
      mutate(RowID = paste(sep = "", Year,1:n()))
    
    cat(the_message)
    
    the_data_std[[fname_ij]] = data_ij # Save data in location given by filename
  }
}

# ==== Loading raw data ====
all_dirs = list.dirs("../Link lives census") # Download at https://www.rigsarkivet.dk/udforsk/link-lives-data/
all_dirs = all_dirs[grep("transcribed_sources/census", all_dirs)]

the_data_raw = list()

for(i in all_dirs){
  
  Year = as.numeric(substr(i, 29, 32))
  
  files_to_read = data.frame(fname = list.files(i)) %>% 
    mutate(tmp = substrRight(fname, 4)) %>% 
    filter(tmp == ".csv") %>% select(-tmp)
  
  for(j in files_to_read$fname){
    cat("\n\nReading",i,j,"                                           \n")
    fname_ij = paste(sep = "/",i,j)
    suppressWarnings({
      suppressMessages({
        data_ij = read_csv(fname_ij, n_max = 10)
      })
    })
    c_class = rep("c", NCOL(data_ij))
    data_ij = read_csv(
      fname_ij, 
      n_max = n_load,
      col_types = c_class
    )
    data_ij$Year = unique(Year)
    data_ij$fname = fname_ij
    
    data_ij = data_ij %>% mutate_all(as.character)
    
    # names(data_ij) = tolower(names(data_ij))
    # names(data_ij) = sub_scandi(names(data_ij))
    
    the_message = paste(
      sep = "",
      "\n",
      "Found NROW = ", NROW(data_ij)/1000000, " mio.", " and the following columns:\n",
      paste(names(data_ij), collapse = "\n")
    )
    
    # Give id
    data_ij = data_ij %>% 
      mutate(RowID = paste(sep = "", Year,1:n()))
    
    cat(the_message)
    
    the_data_raw[[fname_ij]] = data_ij # Save data in location given by filename
  }
}

# ==== Check simiarity of pa_id ====
# Check whether pa_id is the same across the different sources
# Replace tmp1 and tmp2 by different census years to check similarity
# tmp1 = the_data_raw$`../Link lives census/census_1801_v1.2.1/transcribed_sources/census/1801_20190000.csv`
# tmp2 = the_data_std$`../Link lives census/census_1801_v1.2.1/standardized_sources/census/1801/census.csv`
# 
# tmp1 = tmp1 %>% filter(pa_id %in% 200:299)
# tmp2 = tmp2 %>% filter(pa_id %in% 200:299)
# 
# tmp1 %>% 
#   select(pa_id, navn) %>% 
#   left_join(
#     tmp2 %>% select(pa_id, name_cl),
#     by = "pa_id"
#   ) %>% 
#   mutate(
#     navn = tolower(navn) %>% trimws(),
#     name_cl = trimws(name_cl)
#   ) %>% 
#   mutate(navn == name_cl) %>% View()

# ==== Merging across years and saving ====
lapply(the_data_std, NROW)
merged_data = the_data_std %>% do.call("bind_rows",.)
merged_data_raw = the_data_raw %>% do.call("bind_rows",.)

write_fst(merged_data, "Data/tmp_census.fst", compress = 0) 
write_fst(merged_data_raw, "Data/tmp_census_raw.fst", compress = 0)

