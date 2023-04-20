# Market access
# Date updated:   2023-04-19
# Auhtor:         Christian Vedel 
#
# Output:         Geo.csv containing GIS ID and market access improvements by
#                 the channel

# ==== Libraries ====
library(tidyverse)
library(rgdal)
source("000_Functions.R")
library(foreach)
library(ggspatial)

# ==== Load data ====
shape_parish = readOGR("Data/sogne_shape")
market_towns = read.csv2("Data/Market_towns.csv", fileEncoding="latin1")

water_line = readOGR(
  "Data/water-polygons-split-4326"
)

# ==== Functions ====
# marketPotential
marketPotential = function(dist, theta = -1, weights = rep(1, length(x))){
  sum((dist+1)^theta*weights)
}

# report status
report_status = function(i, total, start_time){
  now_time = Sys.time()
  dif_time = round(now_time - start_time, 3)
  the_units = units(dif_time)
  total_time = round(total/i*dif_time, 1)
  finish = as.character(now_time + total_time - dif_time)
  pct = i/total
  pct = paste0(floor(pct*100),"%")
  
  cat(i,
      pct,
      ":: Elapsed:",
      dif_time,
      "of",
      total_time,
      the_units,
      ":: Done at",
      finish,
      "       \r")
}

# Convert to decimal lat long
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

# ==== spdf of parishes ====
parish_spdf = SpatialPointsDataFrame(
  coords = shape_parish@data %>% dplyr::select(long, lat),
  data = shape_parish@data,
  proj4string = shape_parish@proj4string
)

# Cleaning market town data
market_towns = market_towns %>% 
  rowwise() %>%  
  mutate(
    lat = strsplit(Coord, " ")[[1]][1],
    long = strsplit(Coord, " ")[[1]][2]
  ) %>% 
  mutate(
    lat = deg2dec(lat),
    long = deg2dec(long)
  ) %>% 
  mutate(
    Pop1672 = as.numeric(Pop1672),
    Pop1769 = as.numeric(Pop1769),
    Pop1787 = as.numeric(Pop1787),
    Pop1801 = as.numeric(Pop1801)
  ) %>% 
  ungroup() %>% 
  mutate(tmp = long - mean(long)) %>% 
  rowwise() %>% 
  mutate(
    Y_km = degrees_to_km(lat, long)[1],
    X_km = degrees_to_km(lat, long)[2]
  )

# ==== Market towns spdf ====
dist_mat_market_towns = foreach(
  From = market_towns$Market_townID, 
  .combine = "bind_rows"
) %do% {
  From = market_towns %>% 
    filter(Market_townID == From)
  
  res = market_towns %>% 
    rowwise() %>% 
    mutate(
      Dist_km = sqrt((X_km - From$X_km)^2 + (Y_km - From$Y_km)^2)
    ) %>% 
    mutate(
      From = From$Market_townID
    ) %>%
    dplyr::select(
      From, Market_townID, Dist_km
    ) %>% 
    rename(
      To = Market_townID
    )
  
  return(res)
}

# Exclude neighbor market towns
market_towns = market_towns %>% 
  filter(!(Market_townID %in% c("n2014", "n105")))

market_towns_spdf = SpatialPointsDataFrame(
  coords = market_towns %>% dplyr::select(long, lat),
  data = market_towns,
  proj4string = CRS("+proj=longlat +zone=32 +ellps=GRS80")
)

market_towns_spdf@data = market_towns_spdf@data %>% 
  mutate(
    Before = Coastal == 1 & Privilege_start < 1825 & Privilege_end > 1825
  ) %>% 
  mutate(
    After = Before | Limfjord_coast == "west"
  )


# ==== >> DK Water / land grid ====
# DK grid 1km x 1km
# https://rdrr.io/cran/gdistance/f/vignettes/Overview.Rmd
# Make grid with land / water

# Cut water line
b = matrix(c(8, 54, 16, 58),nrow=2)

water_dk = Clip_it(water_line, b)
plot(water_dk)

# Transform to same proj4string
water_dk = spTransform(water_dk, shape1@proj4string)  

the_bbox = bbox(water_dk)

res = 500 # meters
resDK = res
long_min = the_bbox[1,1]
long_max = the_bbox[1,2]
lat_min = the_bbox[2,1]
lat_max = the_bbox[2,2]

the_gridDK = expand.grid(
  long = seq(from = long_min, to = long_max, by = res),
  lat = seq(from = lat_min, to = lat_max, by = res)
) %>%
  mutate(cellID = 1:n()) %>%
  filter(
    long > 4000000,
    lat > 3400000
  ) %>%
  filter(
    lat < 3875000,
    long < 4700000
  ) %>%
  ungroup()

the_gridDK = SpatialPointsDataFrame(
  coords = the_gridDK %>% dplyr::select(long, lat),
  data = the_gridDK,
  proj4string = shape1@proj4string
)

gc()
the_gridDK_land = the_gridDK %over% water_dk

land = ifelse(!is.na(the_gridDK_land), "water", "land")

the_gridDK@data = the_gridDK@data %>%
  mutate(land = land)

the_gridDK@data %>%
  ggplot(aes(long, lat, col = land)) +
  geom_point() +
  coord_fixed()










# Agger tange
# Added manually in approximate location
the_gridDK@data = the_gridDK@data %>%
  mutate(
    Agger_Isthmus = lat < 3737500 & lat > 3732000 &
      long > 4212000 & long < 4214000
  )


the_gridDK@data %>%
  filter(
    lat > 3700000 & lat < 3750000 &
      long > 4200000 & long < 4240000
  ) %>%
  ggplot(aes(long, lat, col = paste(land, Agger_Isthmus))) +
  geom_point() +
  coord_fixed()

# Encoding the 'Iron coast' - 1000 m. belt on the west coast
the_gridDK@data = the_gridDK@data %>%
  group_by(lat) %>%
  mutate(
    Iron_coast = cellID %in% cellID[which(land == "land")[1:2]]
  ) %>%
  group_by(long) %>%
  mutate(
    Iron_coast = Iron_coast |
      cellID %in% cellID[rev(which(land == "land"))[1:2]] &
      long < 4300000 & long > 4225000
  ) %>%
  ungroup()

the_gridDK@data %>%
  filter(
    lat > 3400000 & lat < 3900000 &
      long > 4200000 & long < 4700000
  ) %>%
  ggplot(aes(long, lat, col = paste(land, Iron_coast))) +
  geom_point() +
  coord_fixed()

save(the_gridDK, file = "Landwater_gridDK.Rdata")
load("Landwater_gridDK.Rdata")


