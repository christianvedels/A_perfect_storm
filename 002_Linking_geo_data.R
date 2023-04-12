# Linking geo data
#
# Date updated:   2023-04-11
# Auhtor:         Christian Vedel 
# Purpose:        Geo data cleaning

# ==== Libraries and preparation ====
library(tidyverse)
source("000_functions.R")
library(rgdal)

# ==== Read data ====
load("Data/tmp_census.Rdata")

# ==== Shape files ====
shape_parish = readOGR("Data/sogne_shape") # From www.digdag.dk

# ==== Extract data to make manual key ====
# merged_data %>% # Tmp 1 for making key
#   distinct(event_county, event_district, event_parish) %>% 
#   mutate_all(.funs = sub_scandi) %>% 
#   write_csv2("Tmp1.csv")
# 
# shape_parish@data %>% # Tmp 2 for making key
#   select(AMT, HERRED, SOGN, GIS_ID, lat, long) %>% 
#   mutate_all(.funs = sub_scandi) %>% 
#   mutate(
#     string = paste(AMT, SOGN, GIS_ID, sep = ", ")
#   ) %>% 
#   write_csv2("Tmp2.csv")

# Read the finished key
key = read_csv2("Data/Key_census_to_shape.csv")

# ==== GIS ID to census data ====
# 'GIS_ID' is the name of the unique ids in the shape file
key %>% 
  rowwise() %>% 
  mutate(
    GIS_ID = strsplit(GIS_ID, ", ")[[1]][3]
  )

stop("Lemvig is missing in LL data")


key = key %>%
  select(Amt, Herred, Sogn, GIS_ID) %>% 
  rowwise() %>% 
  mutate(GIS_ID = gsub("(.*),.*", "\\1", GIS_ID))

match_space %>% 
  mutate(GIS_ID = as.character(GIS_ID)) %>% 
  anti_join(key, by = "GIS_ID") #%>% View()


# key stats
n_for_missing = merged_data %>%
  group_by(Year, County, Hundred, Placename) %>%
  count() %>%
  rename(
    Sogn = Placename,
    Amt = County,
    Herred = Hundred
  ) %>%
  arrange(Amt, Herred) %>%
  semi_join(
    key %>% filter(is.na(GIS_ID))
  )

# add key to merged_data 
n1 = NROW(merged_data)
tmp = merged_data %>% 
  left_join(key, c("County"="Amt","Placename"="Sogn","Hundred"="Herred"))

n2 = NROW(tmp)
if(n1!=n2) stop("Something went wrong in key joining")
merged_data = tmp
rm(tmp)
