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

# ==== construct_panel() ====
# This takes archaeological samples from the data from 009_Archaeological_monte_carlo.R
# and turns them into a panel
construct_panel = function(arch_samples){
  suppressMessages({
    arch_samples %>%
      # Add distinct ID for repeated GIS_IDs
      group_by(GIS_ID, rYear) %>% 
      summarise(
        activity = mean(activity)
      )
  })
}

# ==== arch_sampler() ====
arch_sampler = function(arch_samples, capB = 1000){
  require(foreach)
  
  # Check if geo data is loaded. Otherwise load it
  if(!"geo_data" %in% ls()){
    geo_data = read_csv2("Data/Geo.csv", guess_max = 2000)
  }
  
  Uniques_GIS_IDs = geo_data$GIS_ID
  max_b = arch_samples$b %>% max()
  
  start_t = Sys.time()
  
  panels = foreach(b = 1:capB) %do% {
    # Sample parishes
    GIS_ID_b = sample(Uniques_GIS_IDs, size = length(Uniques_GIS_IDs), replace = TRUE)
    b_b = sample(seq(1, max_b), size = max_b, replace = TRUE)
    
    # Construct samples file from sampled parishes
    res = expand.grid(
      GIS_ID = GIS_ID_b,
      b = b_b
    ) %>% 
      distinct(GIS_ID, b) %>% 
      left_join(arch_samples, by = c("GIS_ID", "b")) %>%
      # Turn this into a panel like the main reg panel
      construct_panel()
    
    
    # Report status to console
    time_t = Sys.time()
    delta_t = time_t - start_t
    per_step = delta_t/b
    total_t = capB * per_step
    remaining_t = total_t - delta_t
    
    message0 = paste0(
      "\n",
      as.character(time_t),
      ": b = ", b,
      " ellapsed time: ", round(delta_t, 3), " ", units(delta_t),
      " remaining: ", round(remaining_t, 3), " ", units(remaining_t),
      " of ", round(total_t, 3), " ", units(total_t)
      
    )
    
    cat(message0)
    
    return(res)
  }
  
  return(panels)
}


