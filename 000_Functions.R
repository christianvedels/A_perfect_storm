# Functions
#
# Date updated:   2023-04-11
# Auhtor:         Christian Vedel 
# Purpose:        Functions used in the rest of the project

# ==== substrRight =====
# substr but from right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# ==== sub_scandi ====
# this substitutes scandinavian letters
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

# ==== sub_scandi_mis ====
# This subsitutes wrongly read 
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
    "Æ"
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



# ==== Clip_it ====
# Clip a shape file by a bounding box
# shp:  Shape 
# bb:   Bounding box (e.g. matrix(c(8, 54, 16, 58), nrow=2))

Clip_it = function(shp, bb){
  require(rgeos)
  if(class(bb)[1] == "matrix") b_poly <- as(raster::extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(raster::extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}


# ==== degrees_to_km() ====
# Converts lat long grid til km grid
# Based on circle approximation of earth
#
# Args:
# lat:  Decimal degrees lattitude
# long: Decimal degrees longitude
# 

degrees_to_km = function(lat, long) {
  mean_earth_radius =  6371.0088
  to_radians = 1 / 360 * 2 * pi
  
  # Lat
  earth_circumference = mean_earth_radius * 2 * pi
  lat_km = earth_circumference * lat / 360
  
  # Long
  radius_at_lat = mean_earth_radius * cos(lat * to_radians)
  circumference_at_lat = 2 * pi * radius_at_lat
  long_km = long / 360 * circumference_at_lat
  
  return(c(lat_km, long_km))
  
}