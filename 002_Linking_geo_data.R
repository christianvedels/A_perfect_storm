# Linking geo data
#
# Date updated:   2023-04-11
# Auhtor:         Christian Vedel 
# Purpose:        Geo data cleaning

# ==== Libraries and preparation ====
library(tidyverse)
source("000_functions.R")
library(sf)

# ==== Shape files ====
shape_old = readOGR("../../Conflict, Religion, and Development/WP1 - Religious conflict in Denmark/Maps fra Nina/Sogne_shape/Sogn1820Final_Adjust.shp")
shape_digdag = readOGR("../../Conflict, Religion, and Development/WP1 - Religious conflict in Denmark/Public map data/KIRKELIG_SHAPE_UTM32-EUREF89/Sogn.shp")
shape_digdag = shape_digdag %>% subset(fra < "1820-01-01" & til > "1820-01-01")