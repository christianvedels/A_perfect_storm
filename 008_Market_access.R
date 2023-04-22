# Market access
# Date updated:   2023-04-20
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
library(raster)
library(gdistance)

# ==== Load data ====
shape_parish = readOGR("Data/sogne_shape")

water_line = readOGR(
  "Data/water-polygons-split-4326"
)

# Sound toll
sound_toll = read.csv2("data/LocalSoundToll.csv")

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

# MAfunction
# Parameters:
# theta:  Distance elasticity. Defaults to 1 
# alpha:  Relative cost of land travel (Defaults to 10). Alternative values
#         are computationally costly.
# portW:  Input the weight of each port. 

MAfunction = function(theta = -1, alpha = 10, portW){
  
  addr_alpha = which(attr(costmats,"alpha")==alpha)
  cost_distPort = costmats[[addr_alpha]]
  
  # Estimate MP
  start_time = Sys.time()
  res = foreach(i = 1:NROW(parish_spdf@data), .combine = "bind_rows") %do% {
    GIS_ID_i = parish_spdf@data$GIS_ID[i]
    
    dists_i = sound_toll_port_spdf@data %>%
      bind_cols(CostDist = cost_distPort[i,]) %>% 
      # Avoid exploding values close to ports
      mutate(
        CostDist = ifelse(CostDist < 1, 1, CostDist) 
      )
    
    MP = dists_i %>%
      ungroup() %>%
      summarise(MPbefore = marketPotential(CostDist, weights = portW, theta = theta)) %>% unlist()
    
    if(i %% 10 == 0){
      report_status(i, total = NROW(parish_spdf@data), start_time = start_time)
    }
    
    res = data.frame(GIS_ID = GIS_ID_i, MP = MP)
    return(res)
  }
  
  return(res)
}

# Estimate
Calculate_and_plot = function(theta, alpha){
  cat("\n1. Before\n")
  MA_before = MAfunction(portW = portWs$before, theta = theta, alpha = alpha) %>% 
    rename(MA_before = MP)
  cat("\n2. After\n")
  MA_after = MAfunction(portW = portWs$after, theta = theta, alpha = alpha) %>% 
    rename(MA_after = MP)
  
  MAs = MA_before %>% 
    left_join(MA_after, by = "GIS_ID") %>% 
    mutate(
      MA_after_before = MA_after / MA_before
    )
  
  parish_spdf@data = parish_spdf@data %>%
    left_join(MAs, by = "GIS_ID")
  
  shape_parish@data = shape_parish@data %>%
    left_join(MAs, by = "GIS_ID")
  
  p1 = parish_spdf@data %>%
    ggplot() +
    layer_spatial(
      data = shape_parish,
      aes(fill = log(MA_after/MA_before)),
      col = "grey"
    ) +
    scale_fill_distiller(palette = "Spectral") +
    geom_point(
      data = sound_toll_port_limfj_spdf@data,
      aes(longitude, latitude),
      inherit.aes = FALSE
    ) + 
    scale_color_distiller(palette = "RdYlBu") +
    theme_bw()
  
  print(p1)
  
  fname = paste0("Plots/MA plots/alpha",alpha,"_theta",abs(theta),".png")
  ggsave(fname, plot = p1, width = 10, height = 7)
  
  return(MAs)
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

# ==== spdf of sound toll ports ====
sound_toll = sound_toll %>% 
  filter(port != "Limfjorden") %>% 
  rename(
    N_in = n_to,
    N_out = n_from
  ) %>% 
  group_by(port) %>% 
  mutate(
    max_traffic = max(trafic)
  ) %>% 
  filter(max_traffic > 1) %>% # Filter off micelanous ports (max 1 ship in a year)
  ungroup() %>% 
  filter(Year >= 1750 & Year <= 1855) %>% 
  mutate(After = as.numeric(Year >= 1834)) %>% 
  # mutate(limfjord_placement = ifelse(port == "Viborg", "west", limfjord_placement)) %>% 
  mutate(
    limfjord_placement = ifelse(
      limfjord_placement == "no", 
      "reference", 
      limfjord_placement
    )
  ) %>% 
  mutate(
    limfjord_placement = relevel(factor(limfjord_placement), ref = "reference")
  ) %>% 
  rename(
    latitude = decLatitude,
    longitude = decLongitude
  ) %>% 
  mutate(
    byPort = ifelse(limfjord_placement == "reference", "reference", port)
  ) %>% 
  mutate(
    byPort = relevel(factor(byPort), ref = "reference")
  )

sound_toll_ports = sound_toll %>% distinct(port, longitude, latitude, limfjord_placement, byPort)
sound_toll_port_spdf = SpatialPointsDataFrame(
  coords = sound_toll_ports %>% dplyr::select(longitude, latitude),
  data = sound_toll_ports,
  proj4string = CRS("+proj=longlat +zone=32 +ellps=GRS80")
)

# ==== Limfjord ports ====
sound_toll_limfjord = sound_toll %>%
  distinct(latitude, longitude, byPort, port, limfjord_placement) %>%
  filter(limfjord_placement != "reference") %>%
  filter(limfjord_placement != "east") %>%
  ungroup()

sound_toll_port_limfj_spdf = SpatialPointsDataFrame(
  coords = sound_toll_limfjord %>% dplyr::select(longitude, latitude),
  data = sound_toll_limfjord,
  proj4string = CRS("+proj=longlat +zone=32 +ellps=GRS80")
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

# Exclude neighbour market towns
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


# ==== DK Water / land grid ====
# # DK grid 0.5km x 0.5km
# # https://rdrr.io/cran/gdistance/f/vignettes/Overview.Rmd
# # Make grid with land / water
# 
# # Cut water line
# b = matrix(c(8, 54, 16, 58),nrow=2)
# 
# water_dk = Clip_it(water_line, b)
# plot(water_dk)
# 
# # Transform to same proj4string
# water_dk = spTransform(water_dk, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")  
# 
# the_bbox = bbox(water_dk)
# 
# res = 500 # meters
# resDK = res
# long_min = the_bbox[1,1]
# long_max = the_bbox[1,2]
# lat_min = the_bbox[2,1]
# lat_max = the_bbox[2,2]
# 
# the_gridDK = expand.grid(
#   long = seq(from = long_min, to = long_max, by = res),
#   lat = seq(from = lat_min, to = lat_max, by = res)
# ) %>%
#   mutate(cellID = 1:n()) %>%
#   filter(
#     long > 4189843,
#     lat > 3400000
#   ) %>%
#   filter(
#     lat < 3875000,
#     long < 4700000
#   ) %>%
#   ungroup()
# 
# the_gridDK = SpatialPointsDataFrame(
#   coords = the_gridDK %>% dplyr::select(long, lat),
#   data = the_gridDK,
#   proj4string = water_dk@proj4string
# )
# 
# gc()
# the_gridDK_land = the_gridDK %over% water_dk
# 
# land = ifelse(!is.na(the_gridDK_land), "water", "land")
# 
# the_gridDK@data = the_gridDK@data %>%
#   mutate(land = land)
# 
# the_gridDK@data = the_gridDK@data %>% 
#   mutate(land = ifelse(long < 4109843, "water", land))
# 
# the_gridDK@data %>%
#   ggplot(aes(long, lat, col = land)) +
#   geom_point() +
#   coord_fixed()
# 
# 
# # Agger isthmus
# # Added manually in approximate location
# the_gridDK@data = the_gridDK@data %>%
#   mutate(
#     Agger_Isthmus = lat < 3737500 & lat > 3732000 &
#       long > 4212000 & long < 4214000
#   )
# 
# 
# the_gridDK@data %>%
#   filter(
#     lat > 3700000 & lat < 3750000 &
#       long > 4200000 & long < 4240000
#   ) %>%
#   ggplot(aes(long, lat, col = paste(land, Agger_Isthmus))) +
#   geom_point() +
#   coord_fixed()
# 
# # Convert to degrees
# the_gridDK = the_gridDK %>% spTransform("+proj=longlat +zone=32 +ellps=GRS80")
# the_gridDK@data = the_gridDK@data %>%
#   rename(long_m = long, lat_m = lat) %>%
#   bind_cols(the_gridDK@coords %>% data.frame())
# 
# # Løgstør
# loegstoer = matrix(c(56.935819, 9.198828, 57.000624, 9.290843), ncol = 2) %>%
#   t() %>%
#   data.frame() %>%
#   rename(lat = X1, long = X2)
# 
# the_gridDK@data = the_gridDK@data %>%
#   mutate(
#     loegstoer = ifelse(
#       lat < max(loegstoer$lat) & lat > min(loegstoer$lat) &
#         long < max(loegstoer$long) & long > min(loegstoer$long),
#       "yes",
#       "no"
#     )
#   )
# 
# the_gridDK@data %>%
#   filter(
#     long > 9 & long < 9.5
#   ) %>%
#   filter(
#     lat > 56.5 & lat < 57.25
#   ) %>%
#   mutate(
#     tmp = paste(land, loegstoer)
#   ) %>%
#   ggplot(aes(long, lat, col = tmp)) +
#   geom_point() +
#   coord_fixed()
# 
# save(the_gridDK, file = "Data/Tmp_landwater_gridDK.Rdata")
load("Data/Tmp_landwater_gridDK.Rdata")

# ==== Computing transition matricies and cost distances ====
alphas = c(1, 5, 10, 20, 50)
# # alpha = 10 is the default parameter
# # this correpsonds to land travel being 10 times more expensive than sea travel
# # Transition mats
# transmatsDK = foreach(alpha = alphas) %do% {
#   cat("\nalpha =", alpha, "||", as.character(Sys.time()))
#   beta = alpha
# 
#   the_gridDK@data = the_gridDK@data %>%
#     mutate(
#       cost = case_when(
#         land == "land" ~ alpha,
#         loegstoer == "yes" & land == "water" ~ beta,
#         TRUE ~ 1
#       )
#     )
# 
#   r0 = matrix( # Fiddling around with transformations to make it line up
#     the_gridDK$cost,
#     nrow = length(unique(the_gridDK$long_m))
#   ) %>%
#     Thermimage::rotate90.matrix()
#   r = raster(
#     r0,
#     # ncol = length(unique(the_gridDK$long_m)), nrows = length(unique(the_gridDK$lat_m)),
#     xmn = the_gridDK@bbox[1,1],
#     xmx = the_gridDK@bbox[1,2],
#     ymn = the_gridDK@bbox[2,1],
#     ymx = the_gridDK@bbox[2,2],
#     crs = the_gridDK@proj4string
#   )
# 
# 
#   tmp = the_gridDK
#   tmp@data = tmp@data %>% dplyr::select(cost)
#   grid_raster = raster::rasterize(tmp, r, 'cost', fun = min)
#   trMat = transition(r, transitionFunction = mean, directions = 8)
# 
#   plot(raster(trMat))
#   return(trMat)
# }
# 
# save(transmatsDK, file = "Data/Tmp_transmatsDK.Rdata")
load("Data/Tmp_transmatsDK.Rdata")

# # Cost distances
# costmats = foreach(i = 1:length(transmatsDK)) %do% {
#   cat("\ni =", i, "||", as.character(Sys.time()))
#   cost_distPort = costDistance(transmatsDK[[i]], parish_spdf, sound_toll_port_spdf)
#   cost_distPort
# }
# attr(costmats,"alpha") = alphas
# save(costmats, file = "Data/Tmp_costmats.Rdata")
load("Data/Tmp_costmats.Rdata")

# ==== Creating portWs ====
portWs = sound_toll %>% # Localised impact
  filter(Year == 1800) %>%  
  dplyr::select(port, byPort, limfjord_placement) %>% 
  mutate(
    before = ifelse(limfjord_placement %in% c("west", "middle"), 0, 1)
  ) %>% 
  mutate(
    after = 1
  )

tmp = sound_toll_port_spdf@data %>% 
  mutate(order_tmp = 1:n()) %>% 
  dplyr::select(order_tmp, port)

portWs = portWs %>% 
  left_join(tmp, by = "port") %>% 
  arrange(order_tmp)

# ==== Compute MAs ====
res = foreach(theta = -2^(0:4), .combine = "bind_rows") %do% {
  foreach(alpha = alphas, .combine = "bind_rows") %do% {
    cat("\nalpha =",alpha,"theta =", theta, "||||", as.character(Sys.time()))
    res_i = Calculate_and_plot(theta, alpha)
    res_i %>% mutate(
      theta = theta,
      alpha = alpha
    )
  }
}


res %>% 
  write.csv2("Data/MA_estimates.csv", row.names = FALSE)
