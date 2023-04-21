# Archaeological data
# Date updated:   2023-04-21
# Auhtor:         Christian Vedel 
#
# Output:         'Arch.csv' containing archaeological observations 

# ==== Libraries =====
library(tidyverse)
library(rgeos)

# ==== Load data ====
the_data = read.csv("Data/Archeological finds/anlaeg_all_4326.csv")
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

the_data = the_data %>% filter(datering %in% used_datering)

# Filter data
the_data = the_data %>%
  # filter(anlaegsbetydning == "M\xf8ntfund") %>% 
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
the_data = bind_cols(the_data, parishes)

the_data = parish_years_empty %>% 
  left_join(the_data, by = "GIS_ID")

the_data = the_data %>% 
  filter(fra_aar<Year & til_aar > Year)

the_data_sum = the_data %>% 
  group_by(Year, limfjord_placement, anlaegsbetydning) %>% 
  summarise(
    prob = n()/n_sogne_limfj,
    lprob = log(n()/n_sogne_limfj)
  )

the_data_sum0 = the_data %>% 
  group_by(Year, limfjord_placement) %>% 
  summarise(
    prob = n()/n_sogne_limfj,
    lprob = log(n()/n_sogne_limfj)
  )

the_data_spdf@data %>% NROW()


# p1 = the_data_sum %>%
#   ggplot(aes(Year, log(prob), col = limfjord_placement)) +
#   geom_line() +
#   # facet_wrap(~anlaegsbetydning, scales = "free_y") +
#   facet_wrap(~anlaegsbetydning) +
#   theme_bw() +
#   geom_vline(xintercept = c(1086,1200), lty = 2)
# 
# ggsave("Plots/Arch.png", width = 45, height = 30, plot = p1, limitsize = FALSE)
# 
p1 = the_data_sum0 %>%
  ggplot(aes(Year, prob, col = limfjord_placement)) +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = c(1086,1200), lty = 2)

p1
ggsave("Arch0.png", width = 10, height = 8, plot = p1)

the_data %>% 
  group_by(anlaegsbetydning, limfjord_placement) %>% 
  summarise(
    n = n(),
    fra_aar = paste0(fra_aar[1:5], collapse = "; "),
    til_aar = paste0(til_aar[1:5], collapse = "; "),
    url = paste0(url[1:5], collapse = "; ")
  ) %>% 
  filter(limfjord_placement=="west") %>% 
  arrange(limfjord_placement, -n)# %>% View()