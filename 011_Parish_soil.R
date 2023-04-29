# Soil types of parishes
# Date updated:   2023-04-29
# Auhtor:         Christian Vedel 
#
# Output:         'Parish_soil.csv'

# This script is based on the following prompt to chatGPT:
# "I have two shape files:
#   shape_parishes
# shape_soil
# 
# Shape parishes contains the borders of parishes. Shape soil contain patches of different soil types.
# I want to use an R script to check the how much of each soil type is in each parish. "


# I repeatedly fed the error and used its response until the present script was generated
# this was then adapted slightly.


# ==== Libraries ====
library(tidyverse)
library(sf)
library(ggspatial)

# ==== Read in shapefiles ====
shape_parishes = st_read("Data/sogne_shape", check_ring_dir = TRUE)
shape_soil = st_read("Data/Jordart_200000_Shape/jordart_200000.shp", check_ring_dir = TRUE)


# ==== Find area of each soil type in each parish ====
# Confirm that the CRS (coordinate reference system) is the same for both shapefiles
if (st_crs(shape_parishes) != st_crs(shape_soil)) {
  shape_soil = st_transform(shape_soil, st_crs(shape_parishes))
}

# Transform the data to a suitable projected coordinate system for Denmark
shape_parishes = st_transform(shape_parishes, "+proj=utm +zone=32 +datum=WGS84")
shape_soil = st_transform(shape_soil, "+proj=utm +zone=32 +datum=WGS84")

# Use the st_buffer function to create a small buffer around each geometry
shape_parishes_buff = st_buffer(shape_parishes, dist = 0.0001)
shape_soil_buff = st_buffer(shape_soil, dist = 0.0001)

# Use the st_intersection function to find the intersection of the two shapefiles
parish_soil_intersection = st_intersection(shape_parishes_buff, shape_soil_buff)

# Use the st_area function to calculate the area of each soil type within each parish
parish_soil_area = st_area(parish_soil_intersection)

area_parish = data.frame(
  GIS_ID = shape_parishes_buff$GIS_ID,
  AREA_PARISH = st_area(shape_parishes_buff)
)

# Convert to a data frame and add columns for the parish name and soil type
parish_soil_area_df = data.frame(
  GIS_ID = parish_soil_intersection$GIS_ID,
  SOIL_TYPE = parish_soil_intersection$TSYM, 
  AREA = parish_soil_area
) %>% 
  group_by(GIS_ID, SOIL_TYPE) %>% 
  summarise(AREA = sum(AREA)) %>% 
  left_join(area_parish, by = "GIS_ID")

# Calculate pct and check for sanity
parish_soil = parish_soil_area_df %>% 
  mutate(
    AREA = as.numeric(AREA),
    AREA_PARISH = as.numeric(AREA_PARISH)
  ) %>% 
  mutate(pct = AREA / AREA_PARISH)

parish_soil %>% ggplot(aes(pct)) + geom_histogram()

parish_soil %>% group_by(GIS_ID) %>% 
  summarise(pct = sum(pct)) %>%
  ggplot(aes(pct)) + geom_histogram()

parish_soil = parish_soil %>% 
  rename(area_parish = AREA_PARISH) %>% 
  select(GIS_ID, SOIL_TYPE, area_parish, pct)

# Make full grid: Each GIS_ID and soil type represented. NA is 0.
parish_soil = expand.grid(
  GIS_ID = unique(parish_soil$GIS_ID) ,
  SOIL_TYPE = unique(parish_soil$SOIL_TYPE) 
) %>% 
  left_join(parish_soil %>% select(-area_parish), by = c("GIS_ID", "SOIL_TYPE")) %>% 
  mutate(
    pct = ifelse(is.na(pct), 0, pct)
  ) %>% 
  select(GIS_ID, SOIL_TYPE, pct) %>% 
  left_join(area_parish, by = "GIS_ID") %>% 
  rename(area_parish = AREA_PARISH) %>% 
  mutate(area_parish = as.numeric(area_parish))

tmp = shape_parishes %>% 
  left_join(parish_soil %>% filter(SOIL_TYPE == "ML"), by = "GIS_ID")

ggplot() + 
  layer_spatial(
    data = tmp, 
    aes(
      fill = pct
    )
  )

# ==== Save results ====
parish_soil %>% write_csv2("Data/Parish_soil.csv")
