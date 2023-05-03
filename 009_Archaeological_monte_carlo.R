# Archaeological Monte Carlo
# Date updated:   2023-04-21
# Auhtor:         Christian Vedel 
#
# Output:         'Arch_panel.csv' containing archaeological observations 

# ==== Library ====
library(tidyverse)
library(foreach)

# ==== Load data ====
the_data = read_csv2("Data/Arch.csv")
geo_data = read.csv2("Data/Geo.csv")

# ==== Descriptive for paper ====
the_data %>% 
  group_by(finding_interpretation_en) %>% 
  count() %>% arrange(-n)

the_data %>% 
  group_by(Category) %>% 
  count() %>% arrange(-n)

# ===== Monte Carlo function ====
# Monte Carlo
# For i in 1:B:
#   1. Sample Year from unif(min, max)

# monteCarlo
# Arguments:
#   Finding_types:  Type of finding e.g. Møntfund
#   capB:           Bootstrap repetitions 
#   resolution:     How often samples should be drawn
monteCarlo = function(Finding_types, capB = 1000, resoultion = 50){
  the_data0 = the_data %>% 
    filter(finding_interpretation_en %in% Finding_types)
  if(NROW(the_data0)==0){stop("No findings for 'Finding_types'")}
  
  count_boot = foreach(b = 1:capB, .combine = "bind_rows") %do% {
    # Sample time interval from that coin
    suppressMessages({
      res = the_data0 %>%
        mutate(
          rYear = round(runif(n(), From_year, To_year)/resoultion)*resoultion
        ) %>%
        group_by(GIS_ID, rYear) %>%
        summarise(
          n = n()
        ) %>%
        mutate(
          activity = as.numeric(n>1)
        )
    })
    
    # Joining with GIS IDs which never showed up
    res = expand.grid(
      GIS_ID = as.character(geo_data$GIS_ID),
      rYear = seq(750, 1500, by = resoultion)
    ) %>%
      left_join(
        res %>% mutate(GIS_ID = as.character(GIS_ID)), by = c("GIS_ID", "rYear")
      ) %>%
      mutate(
        activity = ifelse(is.na(activity), 0, activity),
        n = ifelse(is.na(n), 0, n),
      ) %>%
      mutate(b = b)
    
    return(res)
  }
  
  suppressMessages({
    res = count_boot %>% 
      group_by(rYear, GIS_ID) %>% 
      summarise(
        n = sum(n),
        success = sum(activity),
        trials = capB
      ) %>% 
      mutate(
        rate = success/trials,
        rate_n = n/trials
      )
  })
  
  return(
    list(
      panel = res,
      samples = count_boot
    )
  )
}

# Same as above but based on a normal distribution rather than uniform
monteCarlo_norm = function(Finding_types, capB = 1000, resoultion = 50){
  the_data0 = the_data %>% 
    filter(finding_interpretation_en %in% Finding_types)
  if(NROW(the_data0)==0){stop("No findings for 'Finding_types'")}
  
  count_boot = foreach(b = 1:capB, .combine = "bind_rows") %do% {
    # Sample time interval from that coin
    suppressMessages({
      res = the_data0 %>%
        mutate(
          mean0 = (From_year+To_year)/2,
          sd0 = (To_year - From_year)/1.96
        ) %>% 
        mutate(
          rYear = round(rnorm(n(), mean0, sd0)/resoultion)*resoultion
        ) %>%
        group_by(GIS_ID, rYear) %>%
        summarise(
          n = n()
        ) %>%
        mutate(
          activity = as.numeric(n>1)
        )
    })
    
    # Joining with GIS IDs which never showed up
    res = expand.grid(
      GIS_ID = as.character(geo_data$GIS_ID),
      rYear = seq(750, 1500, by = resoultion)
    ) %>%
      left_join(
        res %>% mutate(GIS_ID = as.character(GIS_ID)), by = c("GIS_ID", "rYear")
      ) %>%
      mutate(
        activity = ifelse(is.na(activity), 0, activity),
        n = ifelse(is.na(n), 0, n),
      ) %>%
      mutate(b = b)
    
    return(res)
  }
  
  suppressMessages({
    res = count_boot %>% 
      group_by(rYear, GIS_ID) %>% 
      summarise(
        n = sum(n),
        success = sum(activity),
        trials = capB
      ) %>% 
      mutate(
        rate = success/trials,
        rate_n = n/trials
      )
  })
  
  return(
    list(
      panel = res,
      samples = count_boot
    )
  )
}


# ==== Run MC ====
# params
set.seed(20)
capB = 1000

site_types_tab = the_data %>% 
  distinct(Category, finding_interpretation_en) %>% 
  bind_rows(
    data.frame(
      Category = "Coin findings", 
      finding_interpretation_en = "Coin findings"
    )
  )

# loop
arch_data = foreach(i = unique(site_types_tab$Category)) %do% {
  cat("\n", i, "\n")
  finding_types_i = site_types_tab %>% 
    filter(Category == i) %>% 
    select(finding_interpretation_en) %>% 
    unlist() %>% unname()
  
  res_i = monteCarlo(Finding_types = finding_types_i, capB = capB)

  res_is = foreach(j = finding_types_i) %do% {
    suppressWarnings({
      j0 = assertive.strings:::strip_non_alphanumeric(j)
    })
    cat(
      "--->", as.character(Sys.time()), j, 
      "                                                      \r"
    )
    res_j = monteCarlo(Finding_types = j, capB = capB)
    return(res_j)
  }
  names(res_is) = finding_types_i
  res_is[[length(res_is)+1]] = res_i
  names(res_is)[length(res_is)] = paste0("Overall_", i)
  
  dir_i = paste0("Data/Tmp_arch_samples")
  if(!dir.exists(dir_i)){
    dir.create(dir_i)
  }
  save(res_is, file = paste0("Data/Tmp_arch_samples/", i,".Rdata") )

  return(res_is)
}

# loop
set.seed(20)
arch_data_norm = foreach(i = unique(site_types_tab$Category)) %do% {
  cat("\n", i, "\n")
  finding_types_i = site_types_tab %>% 
    filter(Category == i) %>% 
    select(finding_interpretation_en) %>% 
    unlist() %>% unname()
  
  res_i = monteCarlo(Finding_types = finding_types_i, capB = capB)
  
  res_is = foreach(j = finding_types_i) %do% {
    suppressWarnings({
      j0 = assertive.strings:::strip_non_alphanumeric(j)
    })
    cat(
      "--->", as.character(Sys.time()), j, 
      "                                                      \r"
    )
    res_j = monteCarlo(Finding_types = j, capB = capB)
    return(res_j)
  }
  names(res_is) = finding_types_i
  res_is[[length(res_is)+1]] = res_i
  names(res_is)[length(res_is)] = paste0("Overall_", i)
  
  dir_i = paste0("Data/Tmp_arch_samples_norm")
  if(!dir.exists(dir_i)){
    dir.create(dir_i)
  }
  save(res_is, file = paste0("Data/Tmp_arch_samples_norm/", i,".Rdata") )
  
  return(res_is)
}

# load("Data/Tmp_arch_samples/Buildings.Rdata")
# res_is$Overall_Buildings
