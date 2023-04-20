# Sound toll
# Date updated:   2023-04-20
# Auhtor:         Christian Vedel 
#
# ==== Libraries ====
source("000_functions.R")
library(tidyverse)

# ==== Load data ====
places_standard = read.csv(
  "Data/Sound toll database/Data/Placenames/places_standard.csv",
  encoding = "UTF-8"
)

places_source = read.csv(
  "Data/Sound toll database/Data/Placenames/places_source.csv",
  encoding = "UTF-8"
)

places_w_spelling = places_standard %>% 
  left_join(places_source, by = c("Kode"="soundcoding"))

passages_16341857 = read.csv(
  "Data/Sound toll database/Data/y1634-1857/doorvaarten.csv",
  encoding = "UTF-8"
)

cargo_16341857 = read.csv(
  "Data/Sound toll database/Data/y1634-1857/ladingen.csv",
  encoding = "UTF-8"
)

tmp1 = passages_16341857 %>% 
  select(
    id_doorvaart,
    jaar, 
    maand,
    schipper_plaatsnaam
  ) %>% 
  rename(
    journey_id = id_doorvaart,
    Year = jaar,
    Month = maand,
    skipper_origin = schipper_plaatsnaam
  )

tmp2 = cargo_16341857 %>% 
  distinct(
    id_doorvaart,
    van, 
    naar
  ) %>% 
  rename(
    journey_id = id_doorvaart,
    From = van,
    To = naar
  )

journeys = tmp1 %>% 
  left_join(tmp2, by = "journey_id")

NROW(journeys)

set.seed(20)
tmp3 = places_w_spelling %>% 
  select(
    Kode, place, Stednavn, Modern_Country, Province
  ) %>% 
  rename(
    place_id = Kode,
    place_standardized = Stednavn
  ) %>% 
  # select(place, place_id) %>% 
  distinct() %>%
  sample_frac() %>% 
  group_by(place) %>% 
  summarise(
    place_id = place_id[1], # Some are not unique
    place_standardized = place_standardized[1],
    Modern_Country = Modern_Country[1],
    Region = Province[1]
  ) %>% 
  mutate( # Mors is 'Nykøbing Mors' it is mislabelled in this data
    place_standardized = ifelse(place_standardized == "Mors", "Nykøbing Mors", place_standardized)
  )


journeys = journeys %>% 
  left_join(tmp3, by = c("From"="place")) %>% 
  rename(
    place_id_from = place_id, 
    place_standardized_from = place_standardized,
    Modern_Country_from = Modern_Country,
    Region_from = Region
  ) %>% 
  left_join(tmp3, by = c("To"="place")) %>% 
  rename(
    place_id_to = place_id, 
    place_standardized_to = place_standardized,
    Modern_Country_to = Modern_Country,
    Region_to = Region
  )

NROW(journeys)
journeys %>% 
  group_by(journey_id) %>% 
  count() %>% 
  filter(n>1)

# ==== count journeys ====
count_from = journeys %>% 
  filter(Modern_Country_from == "Denmark") %>% 
  # filter(Region_from %in% c("North Jutland", "Central Jutland", "South Denmark")) %>%
  group_by(Year, place_standardized_from) %>% 
  count() %>% 
  rename(n_from = n)

count_to = journeys %>% 
  filter(Modern_Country_to == "Denmark") %>% 
  # filter(Region_to %in% c("North Jutland", "Central Jutland", "South Denmark")) %>%
  group_by(Year, place_standardized_to) %>% 
  count() %>% 
  rename(n_to = n)

journeys_count = count_from %>% 
  full_join(count_to, by = c("Year", "place_standardized_from" = "place_standardized_to")) %>% 
  rename(port = place_standardized_from)

west_limfjord = c(
  'Lemvig',
  'Thisted',
  'Skive',
  'Harboøre',
  'Nykøbing Mors',
  'Agger',
  'Limfjorden',
  'Struer'
)

east_limfjord = c(
  'Nibe',
  'Hals',
  'Aalborg'
)

middle_limfjord = c('Løgstør')

journeys_count = journeys_count %>% 
  mutate(
    limfjord_placement = case_when(
      port %in% west_limfjord ~ "west",
      port %in% east_limfjord ~ "east",
      port %in% middle_limfjord ~ "middle",
      TRUE ~ "no"
    )
  ) %>% 
  mutate(
    n_from = ifelse(is.na(n_from), 0, n_from)
  ) %>% 
  mutate(
    n_to = ifelse(is.na(n_to), 0, n_to)
  )


journeys_count0 = expand.grid(
  Year = unique(journeys_count$Year),
  port = unique(journeys_count$port)
) %>%
  mutate(
    limfjord_placement = case_when(
      port %in% west_limfjord ~ "west",
      port %in% east_limfjord ~ "east",
      port %in% middle_limfjord ~ "middle",
      TRUE ~ "no"
    )
  ) %>% 
  left_join(
    journeys_count, by = c("Year", "port", "limfjord_placement")
  ) %>% 
  mutate(
    n_from = ifelse(is.na(n_from), 0, n_from),
    n_to = ifelse(is.na(n_to), 0, n_to)
  ) %>% 
  mutate(trafic = n_from + n_to)

p1 = journeys_count0 %>% 
  filter(Year < 1857) %>% 
  filter(Year > 1750) %>%
  group_by(Year, limfjord_placement) %>% 
  summarise(
    trafic = sum(trafic)
  ) %>% 
  ggplot(aes(Year, log(trafic+1), col = limfjord_placement)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  geom_vline(xintercept = c(1825,1834), lty = 2) +
  # facet_wrap(~limfjord_placement) +
  labs(
    title = "Elsinore sound toll: Ships to/from port relative to Limfjord",
    y = "log(count)"
  )


# ==== Save results ====
places_standard_dk = places_standard %>% 
  filter(Modern_Country == "Denmark")

journeys_count0 %>% 
  left_join(places_standard_dk, by = c("port" = "Stednavn")) %>% 
  write.csv2("Data/LocalSoundToll.csv", row.names = FALSE)
