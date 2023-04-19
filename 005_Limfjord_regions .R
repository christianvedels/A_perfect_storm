# Limfjord regions 
# Date updated:   2023-04-19
# Auhtor:         Christian Vedel 
#
# Output:         Geo.Rdata containing GIS ID and their Limfjord regions


# ==== Libraries ====
library(tidyverse)
source("000_Functions.R")
library(sf)
library(rgeos)
library(rgdal)

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
# The following takes a while to run. This motivates the tmp file
# # 1. Calculate dist to ocean
# # 2. Calculate dist to limfjord
# 
# sogne_to_limfjord = geosphere::dist2Line(sogne_spdf, limfjord)
# sogne_to_ocean = geosphere::dist2Line(sogne_spdf, water_dk)
# 
# save(sogne_to_limfjord, sogne_to_ocean, file = "Data/tmp_limfj_distances.Rdata")
load("Data/tmp_limfj_distances.Rdata")


