# Misc data checks
#
# Date updated:   2023-04-28
# Auhtor:         Christian Vedel 
# Purpose:        Misc lookups in data to inform analysis

# ==== Libraries ====
library(tidyverse)
library(fst)

# ==== Load data ====
merged_data = read_fst("Data/tmp_census.fst") 
occ_key = read_csv2("Data/Top100_HISCO.csv", col_types = "c")

# ==== Tmp checks for the analysis ====
# # Check Nykoebing - one of the affected market towns
# merged_data %>% 
#   filter(GIS_ID == "691") %>% 
#   select(Year, pa_id, name, sex, age, Occupation, Household_position, hisco1, en_hisco_text1, hisco2, en_hisco_text2) %>% 
#   write.csv2("Data/Tmp_nykoebing.csv", fileEncoding="latin1")

# Check growing professions 1880-1901
geo_data = read_csv2("Data/Geo.csv")
west_limfj = geo_data %>% filter(limfjord_placement == "west")
reference = geo_data %>% filter(limfjord_placement == "not")

sum0 = function(x) sum(x, na.rm = TRUE)

# In west Limfjord
obs1801 = merged_data %>% 
  filter(Year == 1801) %>% 
  filter(GIS_ID %in% west_limfj$GIS_ID) %>% 
  pivot_longer(hisco1:hisco5, names_to = "misc") %>% 
  select(name, value) %>% group_by(value) %>% 
  count()

obs1860 = merged_data %>% 
  filter(Year == 1860) %>% 
  filter(GIS_ID %in% west_limfj$GIS_ID) %>% 
  pivot_longer(hisco1:hisco5, names_to = "misc") %>% 
  select(name, value) %>% group_by(value) %>% 
  count()

obs1880 = merged_data %>% 
  filter(Year == 1880) %>% 
  filter(GIS_ID %in% west_limfj$GIS_ID) %>% 
  pivot_longer(hisco1:hisco5, names_to = "misc") %>% 
  select(name, value) %>% group_by(value) %>% 
  count()

obs1901 = merged_data %>% 
  filter(Year == 1901) %>% 
  filter(GIS_ID %in% west_limfj$GIS_ID) %>% 
  pivot_longer(hisco1:hisco5, names_to = "misc") %>% 
  select(name, value) %>% group_by(value) %>% 
  count()

dif_west_late = obs1880 %>% 
  left_join(obs1901, by = "value", suffix = c("_1880", "_1901")) %>%
  mutate(value = substr(value, 1, 6)) %>% 
  group_by(value) %>% 
  summarise(
    n_1880 = sum0(n_1880),
    n_1901 = sum0(n_1901)
  ) %>% 
  mutate(growth = n_1901 / n_1880) %>% 
  arrange(-growth)


dif_west = obs1801 %>% 
  left_join(obs1860, by = "value", suffix = c("_1801", "_1860")) %>%
  mutate(value = substr(value, 1, 6)) %>% 
  group_by(value) %>% 
  summarise(
    n_1801 = sum0(n_1801),
    n_1860 = sum0(n_1860)
  ) %>% 
  mutate(growth = n_1860 / n_1801) %>% 
  arrange(-growth)

# Outside Limfjord
obs1801 = merged_data %>% 
  filter(Year == 1801) %>% 
  filter(GIS_ID %in% reference$GIS_ID) %>% 
  pivot_longer(hisco1:hisco5, names_to = "misc") %>% 
  select(name, value) %>% group_by(value) %>% 
  count()

obs1860 = merged_data %>% 
  filter(Year == 1860) %>% 
  filter(GIS_ID %in% reference$GIS_ID) %>% 
  pivot_longer(hisco1:hisco5, names_to = "misc") %>% 
  select(name, value) %>% group_by(value) %>% 
  count()

obs1880 = merged_data %>% 
  filter(Year == 1880) %>% 
  filter(GIS_ID %in% reference$GIS_ID) %>% 
  pivot_longer(hisco1:hisco5, names_to = "misc") %>% 
  select(name, value) %>% group_by(value) %>% 
  count()

obs1901 = merged_data %>% 
  filter(Year == 1901) %>% 
  filter(GIS_ID %in% reference$GIS_ID) %>% 
  pivot_longer(hisco1:hisco5, names_to = "misc") %>% 
  select(name, value) %>% group_by(value) %>% 
  count()

dif_reference_late = obs1880 %>% 
  left_join(obs1901, by = "value", suffix = c("_1880", "_1901")) %>%
  mutate(value = substr(value, 1, 6)) %>% 
  group_by(value) %>% 
  summarise(
    n_1880 = sum0(n_1880),
    n_1901 = sum0(n_1901)
  ) %>% 
  mutate(growth = n_1901 / n_1880) %>% 
  arrange(-growth)


dif_reference = obs1801 %>% 
  left_join(obs1860, by = "value", suffix = c("_1801", "_1860")) %>%
  mutate(value = substr(value, 1, 6)) %>% 
  group_by(value) %>% 
  summarise(
    n_1801 = sum0(n_1801),
    n_1860 = sum0(n_1860)
  ) %>% 
  mutate(growth = n_1860 / n_1801) %>% 
  arrange(-growth)


dif_west %>% 
  semi_join(
    occ_key %>% filter(Category == "manufacturing"),
    by = c("value"="hisco")
  ) %>% 
  left_join(dif_reference, by = "value") %>% 
  mutate(did_growth = growth.x - growth.y) %>% 
  arrange(-did_growth) %>% 
  mutate(value = as.numeric(value)) %>% 
  left_join(
    hisco::hisco %>% distinct(hisco, en_hisco_text), by = c("value"="hisco")
  )


dif_west_late %>% 
  semi_join(
    occ_key %>% filter(Category == "manufacturing"),
    by = c("value"="hisco")
  ) %>% 
  left_join(dif_reference_late, by = "value") %>% 
  mutate(did_growth = growth.x - growth.y) %>% 
  arrange(-did_growth) %>% 
  mutate(value = as.numeric(value)) %>% 
  left_join(
    hisco::hisco %>% distinct(hisco, en_hisco_text), by = c("value"="hisco")
  )

# Metal processors grew more in west Limfjord than elsewhere


