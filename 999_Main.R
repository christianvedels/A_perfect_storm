# Main
#
# Date updated:   2023-04-14
# Auhtor:         Christian Vedel 
# Purpose:        This script runs all the other scripts in the appropiate order


# ==== Custom source ====
source0 = function(x){
  start_time = Sys.time()
  message1 = paste0("\nStarted running ", x, " at ", as.character(start_time))
  cat(message1)
  source(x)
  
  dif_time = Sys.time() - start_time
  run_time = paste(round(dif_time, 3), units(dif_time))
  message2 = paste0(
    "\nFinished running ", x, " at ", as.character(start_time),
    " after ", run_time
  )
  cat(message2)
}


# ==== Source all scripts ====
source0("000_Functions.R")
source0("001_Read_census_data.R")
source0("002_Linking_geo_data.R")
source0("003_Occupations_data.R")
source0("004_Aggregate_demographic_data.R")
source0("005_Limfjord_regions .R")
source0("006_Sound_toll_data.R")
source0("007_Market_access.R")
