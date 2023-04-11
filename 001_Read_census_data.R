# Reading census data
#
# Date updated:   2023-04-11
# Auhtor:         Christian Vedel 
# Purpose:        This script reads csv-files containing census data and merges them

# ==== Libraries ====
library(tidyverse)
source("000_functions.R")
library(foreach)
library(doParallel)

# ==== Loading standardized data ====
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

# ==== Merging across years ====
lapply(the_data_std, NROW)
merged_data = the_data_std %>% do.call("bind_rows",.)

save(merged_data, file = "Data/tmp_census.Rdata")

