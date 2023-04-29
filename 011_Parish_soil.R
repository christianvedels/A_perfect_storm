# Soil types of parishes
# Date updated:   2023-04-29
# Auhtor:         Christian Vedel 
#
# Output:         'Parish_soil.csv'


# ==== Libraries ====
library(tidyverse)
library(rgdal)

# ==== Soil type for parishes ====

# # Soil type shares
# soil = readOGR("Data/Jordart_200000_Shape/jordart_200000.shp")
# soil = soil %>%
#   spTransform("+proj=longlat +zone=32 +ellps=GRS80")
# 
# # Strategy
# # 1. Create grid
# # 2. Sample from grid
# # 3. Use grid to estimate values in shape data
shape_parishes = readOGR("Data/sogne_shape")
# shape_parishes = shape_parishes %>% 
#   spTransform("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")
# the_bbox = bbox(shape_parishes)
# 
# res = 100 # meters
# 
# long_min = the_bbox[1,1]
# long_max = the_bbox[1,2]
# lat_min = the_bbox[2,1]
# lat_max = the_bbox[2,2]
# 
# the_grid = expand.grid(
#   long = seq(from = long_min, to = long_max, by = res),
#   lat = seq(from = lat_min, to = lat_max, by = res)
# )
# 
# the_grid = SpatialPoints(
#   coords = the_grid,
#   proj4string = shape_parishes@proj4string
# )
# 
# the_grid = rgeos::gIntersection(the_grid, shape_parishes, checkValidity = 2)
# 
# the_grid = the_grid %>%
#   spTransform(soil@proj4string)
# 
# attr(the_grid@coords, "dimnames")[[1]] = 1:NROW(the_grid@coords)
# soil_grid = the_grid %over% soil
# soil_grid = the_grid@coords %>%
#   data.frame() %>%
#   bind_cols(soil_grid) %>%
#   rename(
#     long = x,
#     lat = y
#   )
# save(soil_grid, file = "Data/Tmp_soil_grid.Rdata")
load("Data/Tmp_soil_grid.Rdata")

soil_grid_spdf = SpatialPointsDataFrame(
  coords = soil_grid %>% select(long, lat),
  proj4string = shape_parishes@proj4string,
  data = soil_grid
)
parish_soils = soil_grid_spdf %over% shape_parishes
parish_soils = bind_cols(parish_soils %>% select(-long, -lat), soil_grid_spdf@data)

parish_mean_soils = parish_soils %>%
  group_by(GIS_ID, TSYM) %>%
  count() %>%
  group_by(GIS_ID) %>%
  mutate(pct = n/sum(n))

parish_mean_soils = expand.grid(
  TSYM = unique(soil@data$TSYM),
  GIS_ID = unique(parish_soils$GIS_ID)
) %>%
  left_join(
    parish_mean_soils, by = c("GIS_ID","TSYM")
  ) %>%
  mutate(
    n = ifelse(is.na(n), 0, n),
    pct = ifelse(is.na(pct), 0, pct)
  )

parish_mean_soils %>%
  group_by(TSYM) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(pct = n/sum(n)) %>% arrange(-pct)

sub_scandi = function(x){
  scandi_letters = c("Æ",
                     "æ",
                     "Ø",
                     "ø",
                     "Å",
                     "å")

  replacement = c("Ae",
                  "ae",
                  "Oe",
                  "oe",
                  "Aa",
                  "aa")

  for(i in 1:length(scandi_letters)){
    x = gsub(
      scandi_letters[i],
      replacement[i],
      x
    )
  }

  return(x)

}

sub_scandi_mis = function(x){
  scandi_letters = c(
    "Ã¸",
    "Ã¥",
    "Ã¦",
    "Ã˜",
    "Ã…",
    "Ã†"
  )

  replacement = c(
    "ø",
    "å",
    "æ",
    "Ø",
    "Å",
    "Ø"
  )

  for(i in 1:length(scandi_letters)){
    x = gsub(
      scandi_letters[i],
      replacement[i],
      x
    )
  }

  return(x)
}

soil_pct = parish_mean_soils %>%
  mutate(TSYM = sub_scandi_mis(TSYM)) %>%
  mutate(TSYM = sub_scandi(TSYM)) %>%
  rename(Soil_type = TSYM) %>%
  pivot_wider(
    names_from = Soil_type,
    id_cols = "GIS_ID",
    values_from = pct,
    names_prefix = "Soil_pct_"
  )

soil_count = parish_mean_soils %>%
  mutate(TSYM = sub_scandi_mis(TSYM)) %>%
  mutate(TSYM = sub_scandi(TSYM)) %>%
  rename(Soil_type = TSYM) %>%
  pivot_wider(
    names_from = Soil_type,
    id_cols = "GIS_ID",
    values_from = n,
    names_prefix = "Soil_count_"
  )

points_in_parish = parish_mean_soils %>%
  group_by(GIS_ID) %>%
  summarise(
    Area = sum(n)
  )

parish_soil = points_in_parish %>%
  full_join(soil_pct, by = "GIS_ID") %>%
  full_join(soil_count, by = "GIS_ID")

save(parish_soil, file = "Parish_soil.Rdata")
