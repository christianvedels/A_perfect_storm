# Pop mechanism
# Date updated:   2023-06-12
# Auhtor:         Christian Vedel 
#
# Purpose:        Construct the map showing the event        
# Output:     

# ==== Libraries ====
library(tidyverse)
library(fixest)
library(sandwich)
library(foreach)
library(ggridges)

# ==== Load data ====
reg_pop = read_csv2("Data/Pop_reg.csv", guess_max = 2000)

# ==== Factors ====
reg_pop = reg_pop %>% 
  mutate(
    Year = relevel(factor(Year), ref = "1801")
  )

# ==== Mechanism occupation ====
# Breach --> Fishing
fish = feols(
  log(Fishing + 1) ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)
plot_mod(
  fish, "fish", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

fish0 = fish

fish = feols(
  Fish ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>% 
    mutate(Fish = Fishing > 0),
  cluster = ~ GIS_ID
)
plot_mod(
  fish, "fish_extensive", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

fish = feols(
  log(Fishing) ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>% 
    filter(coastal),
  cluster = ~ GIS_ID
)
plot_mod(
  fish, "fish_intensive", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

fish = feols(
  Share ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>% 
    mutate(Share = Fishing / Pop),
  cluster = ~ GIS_ID
)
plot_mod(
  fish, "fish_share", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

# Breach --> Manufacturing
manu = feols(
  log(Manufacturing + 1) ~ Year*Affected,
  data = reg_pop %>% mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)
plot_mod(
  manu, "manu", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

manu0 = manu

manu = feols(
  Manu ~ Year*Affected,
  data = reg_pop %>% mutate(Affected = delta_lMA_theta_1_alpha_10) %>% mutate(Manu = Manufacturing>0),
  cluster = ~ GIS_ID
)
plot_mod(
  manu, "manu_extensive", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

manu = feols(
  log(Manufacturing) ~ Year*Affected,
  data = reg_pop %>% mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)
plot_mod(
  manu, "manu_intensive", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)


manu = feols(
  Share ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>% 
    mutate(Share = Manufacturing / Pop),
  cluster = ~ GIS_ID
)
plot_mod(
  manu, "manu_share", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)


# APE
Fish_avg_midpoint = reg_pop %>% 
  filter(Year %in% c(1801, 1901), limfjord_placement == "west") %>% 
  summarise(Fishing = mean(Fishing)) %>% unlist()

Manu_avg_midpoint = reg_pop %>% 
  filter(Year %in% c(1801, 1901), limfjord_placement == "west") %>% 
  summarise(Manufacturing = mean(Manufacturing)) %>% unlist()

APE_fish = Fish_avg_midpoint * fish0$coefficients["Year1901:Affected"]
APE_manu = Manu_avg_midpoint * manu0$coefficients["Year1901:Affected"]


# All occupations
reg_pop = reg_pop %>% 
  rowwise() %>% 
  mutate(
    all_occ = 
      hisco_1st_digit0 +
      hisco_1st_digit1 +
      hisco_1st_digit2 +
      hisco_1st_digit3 +
      hisco_1st_digit4 +
      hisco_1st_digit5 +
      hisco_1st_digit6 +
      hisco_1st_digit7 +
      hisco_1st_digit8 +
      hisco_1st_digit9
  )

# Breach --> Fishing
occ = feols(
  log(all_occ + 1) ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)
plot_mod(
  occ, "all_occupations", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

occ = feols(
  Occ ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>% 
    mutate(Occ = all_occ > 0),
  cluster = ~ GIS_ID
)
plot_mod(
  occ, "all_occupations_intensive", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

occ = feols(
  log(all_occ) ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>% 
    mutate(all_occ = all_occ / Pop) %>% 
    filter(coastal),
  cluster = ~ GIS_ID
)
plot_mod(
  occ, "all_occupations_extensive", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

occ = feols(
  Share ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>% 
    mutate(Share = all_occ / Pop),
  cluster = ~ GIS_ID
)
plot_mod(
  occ, "all_occupations_share", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

# ==== Mechanism internal migration ====
# Breach --> Migration
migr = feols(
  log(Born_different_county+1) ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>% 
    filter(as.numeric(as.character(Year)) >= 1845) %>% 
    mutate(Year = relevel(Year, ref = "1845")),
  cluster = ~ GIS_ID
)
plot_mod(
  migr, "born_different", ylab = "Parameter estimate", 
  vadj = 0.15, the_col = "#2c5c34", ref_year = 1845, corner_text = "Control group: Less Market Access improvement"
)

migr = feols(
  Share ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>% 
    filter(as.numeric(as.character(Year)) >= 1845) %>% 
    mutate(Year = relevel(Year, ref = "1845")) %>% 
    mutate(Share = Born_different_county / Pop),
  cluster = ~ GIS_ID
)
plot_mod(
  migr, "born_different_share", ylab = "Parameter estimate", 
  vadj = -0.25, the_col = "#2c5c34", ref_year = 1845, corner_text = "Control group: Less Market Access improvement"
)


# ==== Effect by age group and gender ====
# Young children per woman
fertility = feols(
  Child_women_ratio ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)
plot_mod(
  fertility, "fertility", ylab = "Parameter estimate", 
  vadj = 0.15, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

migr$coefficients["Year1901:Affected"]
fertility$coefficients["Year1901:Affected"]

# Young men
mf = feols(
  mf_ratio_15_24 ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% 
    mutate(Affected = limfjord_placement_west) %>% 
    mutate(mf_ratio_15_24 = Age_25_34_m / Age_25_34_f),
  cluster = ~ GIS_ID
)
plot_mod(
  mf, "young_male", ylab = "Parameter estimate", 
  vadj = 0.15, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

# Elderly
mf = feols(
  log(Age_65_125) ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)
plot_mod(
  mf, "elderly", ylab = "Parameter estimate", 
  vadj = 0.15, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

# ==== Male female ratios in age groups 
tmp_f = reg_pop %>% 
  select(Year, GIS_ID, limfjord_placement, Age_0_1_f:Age_65_125_f) %>% 
  pivot_longer(Age_0_1_f:Age_65_125_f) %>% 
  mutate(name = gsub("_f","", name))
tmp_m = reg_pop %>% 
  select(Year, GIS_ID, limfjord_placement, Age_0_1_m:Age_65_125_m) %>% 
  pivot_longer(Age_0_1_m:Age_65_125_m) %>% 
  mutate(name = gsub("_m","", name))

data1 = tmp_f %>% 
  left_join(
    tmp_m, by = c("GIS_ID", "Year", "name", "limfjord_placement"),
    suffix = c("_f", "_m")
  ) %>% 
  filter(name != "Age_0_1") 

res_g = foreach(g = unique(data1$name), .combine = "bind_rows") %do% {
  data_g = data1 %>% 
    filter(name == g) %>% 
    fastDummies::dummy_cols("limfjord_placement") %>% 
    mutate(
      mf_ratio = (value_m + 1)/(value_f + 1)
    )
  
  mod_g = feols(
    log(mf_ratio) ~ log(value_f) + Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
    data = data_g %>% 
      mutate(Affected = limfjord_placement_west),
    cluster = ~ GIS_ID
  )
  
  plot_mod(mod_g, return_data = TRUE) %>% 
    mutate(group = g)
}

p1 = res_g %>% 
  mutate(
    group = sub("_", " ", group)
  ) %>% 
  mutate(
    group = sub("_", " to ", group)
  ) %>%
  mutate(
    group = factor(
      group,
      levels = c(
        "Age 1 to 4", 
        "Age 5 to 14",
        "Age 15 to 24",
        "Age 25 to 34",
        "Age 35 to 44",
        "Age 45 to 54",
        "Age 55 to 64",
        "Age 65 to 125"
      )
    )
  ) %>% 
  ggplot(aes(Year, Estimate, col = group)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = Lower, ymax = Upper)) + 
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  facet_wrap(~group) +
  geom_vline(xintercept = c(1825, 1833.5), lty = 2) +
  scale_x_continuous(breaks = seq(1790, 1900, by = 10)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +
  theme(legend.position = "bottom",
        plot.caption = element_text(vjust = 20)) + 
  labs(
    y = "Parameter estimate",
    caption = "Control: Non-Limfjord parishes"
  ) + 
  geom_hline(yintercept = 0) + 
  theme(
    plot.caption = element_text(size = 7, face = "italic", vjust = 0.2),
  )


fname0 = paste0("Plots/Regression_plots/", "Age_group_mf_ratios", ".png")
ggsave(fname0,  plot = p1, width = 14, height = 16, units = "cm")

