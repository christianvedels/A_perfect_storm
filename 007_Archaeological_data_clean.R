# Archaeological data
# Date updated:   2023-04-21
# Auhtor:         Christian Vedel 
#
# Output:         'Arch.csv' containing archaeological observations 

# ==== Libraries =====
library(tidyverse)
library(rgdal)
library(foreach)
source("000_Functions.R")

# ==== Load data ====
the_data = read.csv("Data/Archeological finds/anlaeg_all_4326.csv", fileEncoding = "latin1")
shape = readOGR("Data/Archeological finds/anlaeg_all_4326_shp")
shape_parishes = readOGR("Data/sogne_shape/sogne.shp")
geo_data = read.csv2("Data/Geo.csv")

# ==== Clean data ====
shape = shape %>% spTransform("+proj=longlat +zone=32 +ellps=GRS80")
coords = shape@coords %>% data.frame()
names(coords) = c("Long", "Lat")

the_data = the_data %>% 
  bind_cols(coords)

# Filter off very uncertain datings
the_data %>% 
  group_by(
    datering
  ) %>% 
  summarise(
    mean_range = mean(til_aar - fra_aar)
  ) %>% 
  arrange(mean_range)

used_historical_periods = c(
  "Efterreformatorisk tid",
  "Nyere tid",
  "Vikingetid",
  "Middelalder",
  "Historisk Tid"
)

the_data = the_data %>% filter(datering %in% used_historical_periods)

# Filter data
the_data = the_data %>%
  filter(
    til_aar <= 1500 | fra_aar >= 750
  )

# Datafication 
parish_years_empty = expand.grid(
  Year = seq(from = 750, to = 1500, by = 50),
  GIS_ID = unique(shape_parishes$GIS_ID)
)

all_in = function(x1, x2, y1, y2){
  test1 = x1>=y1
  test2 = x2<=y2
  return(test1&test2)
}

tmp = function(x){
  if(length(x)==1){
    return(x)
  } else if(length(x)==0){
    return(NA)
  } else {
    stop("Too long")
  }
}

the_data_spdf =
  SpatialPointsDataFrame(
    coords = data.matrix(the_data %>% select(Long, Lat)),
    data = the_data,
    proj4string = CRS("+proj=longlat +zone=32 +ellps=GRS80 +no_defs")
  )

parishes = the_data_spdf %over% shape_parishes
parishes = parishes %>% select(GIS_ID)
the_data = bind_cols(the_data, parishes)

the_data = parish_years_empty %>% 
  left_join(the_data, by = "GIS_ID")

the_data = the_data %>% 
  filter(fra_aar<Year & til_aar > Year)

# Select vars which are used 
the_data = the_data %>% 
  select(
    Year, GIS_ID, anlaegsbetydning, fra_aar, til_aar
  ) %>% 
  rename(
    finding_interpretation = anlaegsbetydning
  )

# Replace scandi letters
the_data = the_data %>% 
  mutate(
    finding_interpretation = sub_scandi(finding_interpretation)
  )

# ==== Define categories ====
site_types = list(
  Indicators_of_economic_activity = c(
    "Coin findings" = "Moentfund",
    "Brick" = "Tegl",
    "Iron" = "Jern",
    "Metal" = "Metal",
    "Water mill" = "Vandmoelle",
    "Mill dam" = "Moelledaemning",
    "Bakery oven" = "Bageovn",
    "Furnace (unspecified)" = "Ovnanlaeg (uspecificeret)",
    "Windmill" = "Vindmoelle",
    "Pottery" = "Keramik",
    "Glas" = "Glas",
    "Trade" = "Handel",
    "Textile" = "Tekstil"
  ),
  Buildings = c(
    "House (possibly with a stable)" = "Hus (evt. med stald)",
    "Settlement, unspecified subgroup" = "Bosaettelse, uspec undergruppe",
    "Well" = "Broend",
    "Livestock enclosure" = "Dyrefold",
    "Farmstead" = "Gaard",
    "Pit-house" = "Grubehus",
    "Hedge/fence" = "Hegn/gaerde",
    "Stable building" = "Staldbygning",
    "Posthole with unknown function" = "Stolpehul m. uvis funktion",
    "Ditch (boundary)" = "Groeft (skel)",
    "Boundary (unspecified)" = "Skel (uspecificeret)",
    "Boundary stone (property)" = "Skelsten (ejendom)",
    "Garden" = "Have",
    "Barn" = "Lade",
    "Boundary (property)" = "Skel (ejendom)",
    "Main building" = "Hovedbygning"
  ),
  State_formation = c(
    "Thing (assembly site)" = "Tingsted",
    "Execution site" = "Rettersted",
    "Hospital" = "Hospital",
    "Official residence" = "Embedsbolig",
    "Boundary stone (public administration)" = "Skelsten (offentlig administration)",
    "Boundary (public administration)" = "Skel (offentlig administration)"
  ),
  Religious = c(
    "Church" = "Kirke",
    "Chapel" = "Kapel",
    "Monastery complex" = "Klosteranlaeg",
    "Burial chapel" = "Gravkapel",
    "Cross" = "Kors",
    "Church barn" = "Kirkelade",
    "Church stable" = "Kirkestald"
  ),
  Defensive = c(
    "Castle/motte-and-bailey" = "Borg/Voldsted", 
    "Moat" = "Voldgrav", 
    "Rampart" = "Forsvarsvold", 
    "Fortification" = "Befaestning", 
    "Military, unspecified subgroup" = "Militaervaesen, uspec undergruppe", 
    "Redoubt" = "Skanse", 
    "Trench" = "Skyttegrav"
  ),
  Distress = c(
    "Hoard finds" = "Depotfund"
  )
)


site_types_tab = foreach(i = 1:length(site_types), .combine = "bind_rows") %do% {
  name_i = names(site_types)[i]
  data.frame(
    Category = name_i,
    finding_interpretation = site_types[[i]],
    finding_interpretation_en = names(site_types[[i]])
  )
}

the_data = the_data %>% 
  left_join(site_types_tab, by = "finding_interpretation") %>%
  mutate(
    Category = ifelse(is.na(Category),
                      "Not categorized", Category
                      ),
    finding_interpretation_en = ifelse(is.na(finding_interpretation_en), 
                                       "Not categorized", finding_interpretation_en
                                       )
  )

the_data = the_data %>% 
  rename(
    From_year = fra_aar,
    To_year = til_aar
  )

# ==== Save data ====
the_data %>% 
  write_csv2("Data/Arch.csv")

