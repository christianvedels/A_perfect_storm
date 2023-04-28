# Distance to market town
# Date updated:   2023-04-228
# Auhtor:         Christian Vedel 
#
# Output:         Geo.csv
#                 Computes distances to market towns for each parish

# ==== Libraries ====
library(tidyverse)
library(foreach)

# ==== Load data ====
geo_data = read_csv2("Data/Geo.csv", guess_max = 2000)
market_towns = read.csv2("Data/Market_towns.csv", fileEncoding="latin1")

# ==== Convert to decimal lat long ====
# lat long
deg2dec = function(x){
  handlNA = function(x) ifelse(is.na(x), 0, x)
  x = strsplit(x,"[^0-9.-]")
  dec = lapply(x, function(x){x[1]}) %>% 
    unlist() %>% 
    as.numeric() %>% 
    handlNA()
  min = lapply(x, function(x){x[2]}) %>% 
    unlist() %>% 
    as.numeric() %>% 
    handlNA()
  sec = lapply(x, function(x){x[3]}) %>% 
    unlist() %>% 
    as.numeric() %>% 
    handlNA()
  
  min = min + sec/60
  res = dec + min/60
  
  return(res)
}

market_towns = market_towns %>% 
  rowwise() %>%  
  mutate(
    lat = strsplit(Coord, " ")[[1]][1],
    long = strsplit(Coord, " ")[[1]][2]
  ) %>% 
  mutate(
    lat = deg2dec(lat),
    long = deg2dec(long)
  )


# ==== Calculate distance ====
dist_mt = foreach(i = 1:NROW(geo_data), .combine = "bind_rows") %do% {
  dist_i = geosphere::distGeo(
    geo_data[i,] %>% select(long, lat) %>% data.matrix(),
    market_towns %>% select(long, lat) %>% data.matrix()
  )
  data.frame(
    GIS_ID = geo_data$GIS_ID[i],
    Distance_market_town = dist_i[which.min(dist_i)]/1000, # convert to km
    Closest_market_town = market_towns$GIS_ID[which.min(dist_i)]
  )
  
}


# ==== Save data ====
dist_mt %>% write_csv2("Data/Distance_to_market_town.csv")
