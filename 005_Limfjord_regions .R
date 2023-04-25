# Limfjord regions 
# Date updated:   2023-04-21
# Auhtor:         Christian Vedel 
#
# Output:         Geo.csv containing GIS ID and their Limfjord regions


# ==== Libraries ====
library(tidyverse)
source("000_Functions.R")
library(sf)
library(rgeos)
library(rgdal)
library(ggspatial)

# ==== Load data ====
# Downloaded 2023-04-19 from:
# - https://osmdata.openstreetmap.de/data/water-polygons.html
water_line = readOGR(
  "Data/water-polygons-split-4326"
)

shape_parish = readOGR("Data/sogne_shape") # From www.digdag.dk

limfjord = readOGR("Data/limfjorden/Limfjorden.shp")

# ==== Cleaning shape files ====

# Cut water line
b = matrix(c(8, 54, 16, 58),nrow=2)

water_dk = Clip_it(water_line, b)
plot(water_dk)

# Transform to same proj4string
water_dk = water_dk %>% spTransform("+proj=longlat +zone=32 +ellps=GRS80")
limfjord = limfjord %>% spTransform("+proj=longlat +zone=32 +ellps=GRS80")

# Construct spdf from parish shape
sogne_spdf = SpatialPointsDataFrame(
  coords = shape_parish@data %>% select(long, lat),
  data = shape_parish@data,
  proj4string = shape_parish@proj4string
)

# ==== Calculate distances ====
# The following takes ~18 hours to run. This motivates the tmp file
# # 1. Calculate dist to ocean
# # 2. Calculate dist to limfjord
# 
# parish_to_limfjord = geosphere::dist2Line(sogne_spdf, limfjord)
# parish_to_ocean = geosphere::dist2Line(sogne_spdf, water_dk)
# 
# save(parish_to_limfjord, parish_to_ocean, file = "Data/tmp_limfj_distances0.Rdata")
load("Data/tmp_limfj_distances0.Rdata")

parish_to_limfjord = data.frame(parish_to_limfjord) %>% 
  # mutate(STEDNR = sogne_spdf$STEDNR) %>% 
  rename(
    distance_lim = distance,
    lon_lim = lon,
    lat_lim = lat
  )

parish_to_ocean = parish_to_ocean %>% data.frame() %>% 
  rename(
    distance_oce = distance,
    lon_oce = lon,
    lat_oce = lat
  ) %>% 
  select(-ID)

shape_parish@data = shape_parish@data %>% 
  bind_cols(parish_to_limfjord) %>% 
  bind_cols(parish_to_ocean) %>% 
  mutate(
    limfjord_closest = ifelse(distance_oce>=(distance_lim-200), "Yes", "No")
  )


# ==== Plots sannity check ====
p1 = ggplot() + 
  layer_spatial(
    data = shape_parish,
    aes(fill = distance_oce)
  ) +
  ggtitle("Distance to the ocean")

p2 = ggplot() + 
  layer_spatial(
    data = shape_parish,
    aes(fill = log(distance_lim))
  ) +
  ggtitle("Distance to the Limfjord")

p3 = ggplot() + 
  layer_spatial(
    data = shape_parish,
    aes(fill = limfjord_closest)
  ) +
  ggtitle("Limfjord closest")

# Missing. Looked up on https://digdag.dk/
# - Harbooer

shape_parish@data %>% # Looked up in data
  filter(SOGN %in% c("Harbooer"))
# GIS_ID: 1668

# Making correction
shape_parish@data = shape_parish@data %>% 
  mutate(
    limfjord_closest = ifelse(GIS_ID == "1668", "Yes", limfjord_closest)
  )

# ==== Divide into east/west/middle ====
dist2line0 = function(x){
  x1 = c(57.044185, 9.186837)
  x2 = c(56.958951, 9.275585)
  # Convert to km
  x1 = degrees_to_km(x1[2], x1[1])
  x2 = degrees_to_km(x2[2], x2[1])
  x = degrees_to_km(x[2], x[1])

  a = (x2[1]-x1[1])*(x1[2]-x[2])-(x1[1]-x[1])*(x2[2]-x1[2])
  b = sqrt((x2[1]-x1[1])^2+(x2[2]-x1[2]))
  res = a/b
  res
}

shape_parish@data = shape_parish@data %>% 
  rowwise() %>% 
  mutate(
    dist_east_west = dist2line0(c(lat, long))
  )

# Check on plot 
p1 = ggplot() + 
  layer_spatial(
    data = shape_parish,
    aes(fill = dist_east_west>0)
  ) + 
  ggtitle("Distance East West")

p1 = ggplot() + 
  layer_spatial(
    data = shape_parish,
    aes(fill = dist_east_west)
  ) + 
  ggtitle("Distance East West")

# Regions
buffer = 20 # Middle zone size in km
shape_parish@data = shape_parish@data %>%
  mutate(
    limfjord_placement = ifelse(limfjord_closest == "Yes", "Limfjord", "not")
  ) %>% 
  rowwise() %>%
  mutate(
    east_west = case_when(
      dist_east_west > buffer ~ "west",
      dist_east_west < -buffer ~ "east",
      TRUE ~ "middle"
    )
  ) %>%
  mutate(
    limfjord_placement = ifelse(limfjord_placement == "not", "not", east_west)
  ) %>%
  ungroup()

p1 = ggplot() + 
  layer_spatial(
    data = shape_parish,
    aes(fill = limfjord_placement)
  ) + 
  ggtitle("limfjord_placement")


geo_data = shape_parish@data

geo_data = geo_data %>% 
  select(AMT, HERRED, SOGN, GIS_ID, long, lat, limfjord_placement, distance_oce, distance_lim) %>% 
  rename(
    County = AMT,
    Hundred = HERRED,
    Parish = SOGN
  ) %>% 
  arrange(County)

# ==== Subgroups ====
geo_data = geo_data %>%
  mutate(
    main = TRUE,
    coastal = distance_oce < 5000,
    non_limfjord_control = distance_lim > 100000 | limfjord_placement != "not",
    wo_kbh = !County %in% c("Koebenhavn",
                            "Frederiksborg")
  )

# ==== Save result ====
write_csv2(geo_data, "Data/Geo.csv")
