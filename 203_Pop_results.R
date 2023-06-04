# Pop results
# Date updated:   2023-04-26
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

# ==== Read data ====
reg_pop = read_csv2("Data/Popdata.csv", guess_max = 2000)
geo_data = read_csv2("Data/Geo.csv", guess_max = 2000)
market_access = read_csv2("Data/MA_estimates.csv", guess_max = 2000) 
market_towns = read_csv2("Data/Market_towns.csv")
dist_mt = read_csv2("Data/Distance_to_market_town.csv", guess_max = 2000)

# ==== Functions ====
plot_mod = function(
    mod, 
    fname, 
    ref_year = 1801, 
    Parishes = 0, 
    ylab = "",
    vadj = 0.15,
    the_col = "#b33d3d",
    corner_text = "Control: Non-Limfjord parishes",
    return_data = FALSE
){
  capN = mod$nobs
  
  data0 = mod$coeftable %>%
    data.frame() %>%
    mutate(Var = rownames(.)) %>%
    remove_rownames() %>%
    mutate(
      Year = substr(Var, 5, 8) %>% as.numeric()
    ) %>%
    filter(
      grepl("Affected",Var)
    ) %>%
    filter(
      !grepl("log",Var)
    ) %>%
    mutate(
      Upper = Estimate + 1.96*Std..Error,
      Lower = Estimate - 1.96*Std..Error
    ) %>%
    drop_na(Year) %>% 
    mutate(
      Year = as.numeric(Year)
    ) %>%
    bind_rows(
      expand.grid(
        Estimate = 0,
        Year = ref_year
      )
    )
  
  if(return_data){
    return(data0)
  }
  
  p1 = data0 %>%
    ggplot(aes(Year, Estimate)) +
    geom_point(col = the_col) +
    geom_errorbar(aes(ymax = Upper, ymin = Lower), col = the_col) +
    geom_hline(yintercept = 0, lty = 2) +
    theme_bw() +
    geom_vline(xintercept = c(1825, 1833.5), lty = 2) +
    annotate(geom = "text", x= 1825, label = "1825 breach", y = vadj, angle=90, vjust = 1.2) +
    annotate(geom = "text", x= 1833.5, label = "1834 navigable", y = vadj, angle=90, vjust = 1.2) +
    scale_x_continuous(breaks = seq(1790, 1900, by = 10)) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    ) +
    theme(legend.position = "bottom",
          plot.caption = element_text(vjust = 20)) + 
    labs(
      y = ylab,
      caption = corner_text
    ) + 
    theme(
      plot.caption = element_text(size = 7, face = "italic", vjust = 0.2),
      # axis.title.x = element_text(hjust = 0)
    )
  
  fname0 = paste0("Plots/Regression_plots/", fname, ".png")
  ggsave(fname0,  plot = p1, width = 10, height = 8, units = "cm")
  
  return(p1)
}

# ==== Small dataficiations ====
# Reshaping MA estimates
market_access = market_access %>% 
  mutate(
    delta_lMA = log(MA_after_before)
  ) %>% 
  select(GIS_ID, delta_lMA, theta, alpha) %>% 
  pivot_wider(
    names_from = c(theta, alpha),
    values_from = delta_lMA,
    names_glue = "delta_lMA_theta_{-theta}_alpha_{alpha}"
  )

# Adding geo and MA to samples
reg_pop = reg_pop %>%
  left_join(geo_data, by = "GIS_ID") %>%
  left_join(market_access, by = "GIS_ID") %>%
  mutate(Year = relevel(factor(Year), ref = "1801")) %>%
  fastDummies::dummy_cols("limfjord_placement") %>% 
  filter(consistent == 1) %>% 
  drop_na(GIS_ID)

# Adding market town dummmy
mt = market_towns %>% 
  distinct(GIS_ID) %>% 
  mutate(Market_town = 1)

reg_pop = reg_pop %>% 
  left_join(mt, by = "GIS_ID") %>% 
  mutate(
    Market_town = ifelse(is.na(Market_town), 0, Market_town)
  )

# Adding distance to market town and indicator of within 5 km of market town
reg_pop = reg_pop %>% 
  left_join(dist_mt, by = "GIS_ID") %>% 
  mutate(
    Within_5km_of_mt = Distance_market_town < 5
  )

# Adding child women ratio
reg_pop = reg_pop %>% 
  mutate(Child_women_ratio = (Age_1_4) / (Age_15_24_f + Age_25_34_f + Age_35_44_f)) %>% 
  mutate(
    Child_women_ratio = ifelse(
      is.finite(Child_women_ratio), Child_women_ratio, NA
    )
  )

# ==== Summary table ====
reg_pop %>% 
  mutate(
    Born_different_county = ifelse(Year %in% c(1787, 1801, 1834, 1840), NA, Born_different_county)
  ) %>% 
  select(
    Pop,
    limfjord_placement_west,
    delta_lMA_theta_1_alpha_10,
    Fishing,
    Manufacturing,
    Born_different_county,
    Child_women_ratio
  ) %>% 
  psych::describe(quant = c(0.25, 0.75)) %>% 
  mutate_all(round, 2) %>% 
  select(n, mean, sd, min, Q0.25, median, Q0.75, max) %>% 
  knitr::kable("latex", booktabs = TRUE, align = "c")

n1 = reg_pop %>% 
  distinct(Year) %>% NROW()

n2 = reg_pop %>% 
  distinct(GIS_ID) %>% NROW()

n1*n2

# ==== Balancing plot ====
# Function to center values at 'not'
center_not = function(x, limfjord_position){
  x = x - x[limfjord_position == "not"]
}

plot_stats = reg_pop %>% 
  # filter(GIS_ID %in% matched_gis_ids$GIS_ID) %>%
  filter(Year %in% c(1787, 1801)) %>% 
  filter(limfjord_placement %in% c("not", "west")) %>% 
  select(
    Year, limfjord_placement, Pop, Age_mean, Fishing, Manufacturing, Child_women_ratio
  ) %>% 
  filter(Fishing > 0) %>% 
  mutate(
    lPop = log(Pop),
    lFish = log(Fishing + 1),
    lManu = log(Manufacturing + 1)
  ) %>% 
  select(-Pop, -Fishing, -Manufacturing) %>% 
  group_by(Year, limfjord_placement) %>% 
  pivot_longer(
    Age_mean:lManu,
    names_to = "Variable"
  ) %>% 
  group_by(Year, Variable) %>% 
  mutate(value = value - mean(value, na.rm = TRUE)) %>% 
  mutate(
    value_std = value / sd(value, na.rm = TRUE)
  ) %>% 
  mutate(Year = relevel(Year, ref = "1787")) %>% 
  mutate(
    Variable = case_when(
      Variable == "lPop" ~ "log(Population)",
      Variable == "lManu" ~ "log(Manufacturing + 1)",
      Variable == "lFish" ~ "log(Fishing + 1)*",
      Variable == "Child_women_ratio" ~ "Young children per woman",
      Variable == "Age_mean" ~ "Mean age in parish"
    )
  ) %>% 
  mutate(
    Affected = ifelse(limfjord_placement == "west", "West Limfjord", "Reference")
  )

p1 = plot_stats %>% 
  ggplot(aes(value_std, y = Variable, lty = Affected)) + 
  geom_density_ridges(alpha = 0, scale = 0.8) + 
  facet_wrap(~Year) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  labs(x = "Standardised values")

p1
ggsave("Plots/Balancing_plot.png",  plot = p1, width = 16, height = 10, units = "cm")

# Fishing == 0 is sorted away. Check if dist is similar for this
reg_pop %>% 
  filter(Year %in% c(1787, 1801)) %>% 
  group_by(limfjord_placement, Year) %>% 
  summarise(
    mean(Fishing==0)
  )

# ==== Regressions ====
mod1 = feols(
  log(Pop) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% mutate(Affected = limfjord_placement_west),
  cluster = ~ GIS_ID
)

p1_dummy = plot_mod(mod1, "pop_dummy", ylab = "Parameter estimate", corner_text = "Control group: Non-Limfjord parishes")

p1_dummy

mod2 = feols(
  log(Pop) ~ Year*Affected,
  data = reg_pop %>% mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)

p2_ma = plot_mod(mod2, "pop_MA", ylab = "Parameter estimate", vadj = 1, corner_text = "Control group: Less Market Access improvement")

p2_ma

# Regression table
mods = list(
  mod1,
  mod2
)

mods %>% 
  etable(tex = TRUE)

# ==== APE ====
Pop_avg_midpoint = reg_pop %>% 
  filter(Year %in% c(1801, 1901), limfjord_placement == "west") %>% 
  summarise(Pop = mean(Pop)) %>% unlist()

APE1 = Pop_avg_midpoint * mod1$coefficients["Year1901:Affected"]
APE2 = Pop_avg_midpoint * mod2$coefficients["Year1901:Affected"]

# ==== Multiverse ====
# Multiverse dummy
sub_groups = expand.grid(
  coastal = unique(reg_pop$coastal),
  wo_kbh = unique(reg_pop$wo_kbh),
  non_limfjord_control = unique(reg_pop$non_limfjord_control),
  Within_5km_of_mt = unique(reg_pop$Within_5km_of_mt)
)

mult_dummy = foreach(g = 1:NROW(sub_groups), .combine = "bind_rows") %do% {
  groups_g = sub_groups[g,]
  reg_pop_g = reg_pop
  
  group = ""
  
  if(groups_g$coastal){
    reg_pop_g = reg_pop_g %>% 
      filter(coastal)
    
    if(group == ""){
      group = "A"
    } else {
      group = paste(group, "A", sep = ", ")
    }
    
  }
  
  if(groups_g$wo_kbh){
    reg_pop_g = reg_pop_g %>% 
      filter(wo_kbh)
    
    if(group == ""){
      group = "B"
    } else {
      group = paste(group, "B", sep = ", ")
    }
  }
  
  if(groups_g$non_limfjord_control){
    reg_pop_g = reg_pop_g %>% 
      filter(non_limfjord_control)
    
    if(group == ""){
      group = "C"
    } else {
      group = paste(group, "C", sep = ", ")
    }
  }
  
  if(groups_g$Within_5km_of_mt){
    reg_pop_g = reg_pop_g %>% 
      filter(non_limfjord_control)
    
    if(group == ""){
      group = "D"
    } else {
      group = paste(group, "D", sep = ", ")
    }
  }
  
  mod_g = feols(
    log(Pop) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
    data = reg_pop_g %>% mutate(Affected = limfjord_placement_west),
    cluster = ~ GIS_ID
  )
  
  # Extract data
  plot_mod(mod_g, return_data = TRUE) %>% mutate(group = group)
}

default = mult_dummy %>% 
  filter(group == "") %>% 
  filter(Year == 1901) %>% 
  select(Estimate) %>% unlist()

p1 = mult_dummy %>% 
  filter(Year == 1901) %>%
  arrange(Estimate) %>% 
  mutate(
    group = ifelse(group == "", "Default", group),
    the_col = ifelse(group == "Default", "Yes", "No")
  ) %>% 
  mutate(group = factor(group, levels = group)) %>% 
  ggplot(aes(Estimate, group)) +
  geom_point() + 
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, col = the_col)) + 
  geom_vline(xintercept = default, lty = 2) + 
  xlim(0, NA) + 
  theme_bw() + 
  scale_color_manual(
    values = c("No" = "black", "Yes" = "#b33d3d")
  ) + 
  labs(
    y = "",
    x = "Parameter estimate 1901"
  ) + 
  theme(legend.position = "none")

p1
fname0 = paste0("Plots/Regression_plots/", "Multiverse_dummy", ".png")
ggsave(fname0,  plot = p1, width = 10, height = 8, units = "cm")

# Multiverse market access
sub_groups = expand.grid(
  coastal = unique(reg_pop$coastal),
  wo_kbh = unique(reg_pop$wo_kbh),
  non_limfjord_control = unique(reg_pop$non_limfjord_control),
  Within_5km_of_mt = unique(reg_pop$Within_5km_of_mt)
)

mult_MA = foreach(g = 1:NROW(sub_groups), .combine = "bind_rows") %do% {
  groups_g = sub_groups[g,]
  reg_pop_g = reg_pop
  
  group = ""
  
  if(groups_g$coastal){
    reg_pop_g = reg_pop_g %>% 
      filter(coastal)
    
    if(group == ""){
      group = "A"
    } else {
      group = paste(group, "A", sep = ", ")
    }
    
  }
  
  if(groups_g$wo_kbh){
    reg_pop_g = reg_pop_g %>% 
      filter(wo_kbh)
    
    if(group == ""){
      group = "B"
    } else {
      group = paste(group, "B", sep = ", ")
    }
  }
  
  if(groups_g$non_limfjord_control){
    reg_pop_g = reg_pop_g %>% 
      filter(non_limfjord_control)
    
    if(group == ""){
      group = "C"
    } else {
      group = paste(group, "C", sep = ", ")
    }
  }
  
  if(groups_g$Within_5km_of_mt){
    reg_pop_g = reg_pop_g %>% 
      filter(non_limfjord_control)
    
    if(group == ""){
      group = "D"
    } else {
      group = paste(group, "D", sep = ", ")
    }
  }
  
  mod_g = feols(
    log(Pop) ~ Year*Affected,
    data = reg_pop_g %>% mutate(Affected = delta_lMA_theta_1_alpha_10),
    cluster = ~ GIS_ID
  )
  
  # Extract data
  plot_mod(mod_g, return_data = TRUE) %>% mutate(group = group)
}

default = mult_MA %>% 
  filter(group == "") %>% 
  filter(Year == 1901) %>% 
  select(Estimate) %>% unlist()

p1 = mult_MA %>% 
  filter(Year == 1901) %>%
  arrange(Estimate) %>% 
  mutate(
    group = ifelse(group == "", "Default", group),
    the_col = ifelse(group == "Default", "Yes", "No")
  ) %>% 
  mutate(group = factor(group, levels = group)) %>% 
  ggplot(aes(Estimate, group)) +
  geom_point() + 
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, col = the_col)) + 
  geom_vline(xintercept = default, lty = 2) + 
  xlim(0, NA) + 
  theme_bw() + 
  scale_color_manual(
    values = c("No" = "black", "Yes" = "#b33d3d")
  ) + 
  labs(
    y = "",
    x = "Parameter estimate 1901"
  ) + 
  theme(legend.position = "none")

p1
fname0 = paste0("Plots/Regression_plots/", "Multiverse_MA", ".png")
ggsave(fname0,  plot = p1, width = 10, height = 8, units = "cm")


# Multiverse market access parameters
sub_groups = reg_pop %>% select(delta_lMA_theta_1_alpha_1:delta_lMA_theta_16_alpha_50)

mult_MA2 = foreach(g = 1:NCOL(sub_groups), .combine = "bind_rows") %do% {
  MA_g = sub_groups[,g] %>% unlist() %>% unname()
  # Standardize
  MA_g = (MA_g - mean(MA_g, na.rm = TRUE))/sd(MA_g, na.rm = TRUE)
  
  group = names(sub_groups)[g]
  theta = strsplit(group, "_")[[1]][4]
  alpha = strsplit(group, "_")[[1]][6]
  
  mod_g = feols(
    log(Pop) ~ Year*Affected,
    data = reg_pop %>% mutate(Affected = MA_g),
    cluster = ~ GIS_ID
  )
  
  # Extract data
  plot_mod(mod_g, return_data = TRUE) %>% 
    mutate(
      theta = theta,
      alpha = alpha
    )
}

default = mult_MA2 %>% 
  filter(theta == 1, alpha == 10) %>% 
  filter(Year == 1901) %>% 
  select(Estimate) %>% unlist()

p1 = mult_MA2 %>% 
  filter(Year == 1901) %>%
  arrange(Estimate) %>% 
  mutate(
    the_col = ifelse(theta == 1 & alpha == 10, "Yes", "No")
  ) %>% 
  mutate(
    Alt = paste0("(", "α = ", alpha,", θ = ", theta, ")")
  ) %>% 
  mutate(
    Alt = factor(Alt, levels = unique(Alt))
  ) %>% 
  ggplot(aes(Estimate, Alt)) +
  geom_point() + 
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, col = the_col)) + 
  geom_vline(xintercept = default, lty = 2) + 
  theme_bw() + 
  scale_color_manual(
    values = c("No" = "black", "Yes" = "#b33d3d")
  ) + 
  labs(
    y = "",
    x = "Parameter estimate 1901"
  ) + 
  theme(legend.position = "none")

p1
fname0 = paste0("Plots/Regression_plots/", "Multiverse_MA_param", ".png")
ggsave(fname0,  plot = p1, width = 10, height = 8, units = "cm")

# ==== Multiverse 1787 ====
# Make plots for 1787 to check for pretrends

# Multiverse dummy
default = mult_dummy %>% 
  filter(group == "") %>% 
  filter(Year == 1787) %>% 
  select(Estimate) %>% unlist()

p1 = mult_dummy %>% 
  filter(Year == 1787) %>%
  arrange(Estimate) %>% 
  mutate(
    group = ifelse(group == "", "Default", group),
    the_col = ifelse(group == "Default", "Yes", "No")
  ) %>% 
  mutate(group = factor(group, levels = group)) %>% 
  ggplot(aes(Estimate, group)) +
  geom_point() + 
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, col = the_col)) + 
  geom_vline(xintercept = default, lty = 2) + 
  geom_vline(xintercept = 0) + 
  theme_bw() + 
  scale_color_manual(
    values = c("No" = "black", "Yes" = "#b33d3d")
  ) + 
  labs(
    y = "",
    x = "Parameter estimate 1787"
  ) + 
  theme(legend.position = "none")

p1
fname0 = paste0("Plots/Regression_plots/", "Multiverse_dummy_1787", ".png")
ggsave(fname0,  plot = p1, width = 10, height = 8, units = "cm")

# Multiverse market access
default = mult_MA %>% 
  filter(group == "") %>% 
  filter(Year == 1787) %>% 
  select(Estimate) %>% unlist()

p1 = mult_MA %>% 
  filter(Year == 1787) %>%
  arrange(Estimate) %>% 
  mutate(
    group = ifelse(group == "", "Default", group),
    the_col = ifelse(group == "Default", "Yes", "No")
  ) %>% 
  mutate(group = factor(group, levels = group)) %>% 
  ggplot(aes(Estimate, group)) +
  geom_point() + 
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, col = the_col)) + 
  geom_vline(xintercept = default, lty = 2) + 
  geom_vline(xintercept = 0) + 
  theme_bw() + 
  scale_color_manual(
    values = c("No" = "black", "Yes" = "#b33d3d")
  ) + 
  labs(
    y = "",
    x = "Parameter estimate 1787"
  ) + 
  theme(legend.position = "none")

p1
fname0 = paste0("Plots/Regression_plots/", "Multiverse_MA_1787", ".png")
ggsave(fname0,  plot = p1, width = 10, height = 8, units = "cm")


# Multiverse market access parameters
default = mult_MA2 %>% 
  filter(theta == 1, alpha == 10) %>% 
  filter(Year == 1787) %>% 
  select(Estimate) %>% unlist()

p1 = mult_MA2 %>% 
  filter(Year == 1787) %>%
  arrange(Estimate) %>% 
  mutate(
    the_col = ifelse(theta == 1 & alpha == 10, "Yes", "No")
  ) %>% 
  mutate(
    Alt = paste0("(", "α = ", alpha,", θ = ", theta, ")")
  ) %>% 
  mutate(
    Alt = factor(Alt, levels = unique(Alt))
  ) %>% 
  ggplot(aes(Estimate, Alt)) +
  geom_point() + 
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, col = the_col)) + 
  geom_vline(xintercept = default, lty = 2) + 
  geom_vline(xintercept = 0) + 
  theme_bw() + 
  scale_color_manual(
    values = c("No" = "black", "Yes" = "#b33d3d")
  ) + 
  labs(
    y = "",
    x = "Parameter estimate 1787"
  ) + 
  theme(legend.position = "none")

p1
fname0 = paste0("Plots/Regression_plots/", "Multiverse_MA_param_1787", ".png")
ggsave(fname0,  plot = p1, width = 10, height = 8, units = "cm")

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


# ==== Doubly Robust DID ====
# This is included because of doubly robust estimator
library(did)

set.seed(20)
reg_pop0 = reg_pop %>% 
  mutate(lPop = log(Pop)) %>% 
  mutate(
    Year_num = as.numeric(as.character(Year)),
    GIS_ID_num = as.numeric(factor(GIS_ID)),
    Treat_year = ifelse(limfjord_placement_west==1, 1834, 0)
  ) %>% 
  mutate(
    lManu = log(Manufacturing + 1),
    lFish = log(Fishing + 1)
  )

# No covariates
out1 = att_gt(
  yname = "lPop",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  data = reg_pop0,
)

ggdid(out1)

# Covariates
out2 = att_gt(
  yname = "lPop",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  data = reg_pop0,
  xformla = ~ lManu + lFish + Child_women_ratio  + Age_0_1 + Age_1_4 + Age_5_14 + Age_15_24 + Age_25_34 + Age_35_44 + Age_45_54 + Age_55_64 + Age_65_125
)

ggdid(out2)

# Pretreatment outcome also as covariate
out3 = att_gt(
  yname = "lPop",
  tname = "Year_num",
  gname = "Treat_year",
  idname = "GIS_ID_num",
  data = reg_pop0,
  xformla = ~ lPop + lManu + lFish + Child_women_ratio  + Age_0_1 + Age_1_4 + Age_5_14 + Age_15_24 + Age_25_34 + Age_35_44 + Age_45_54 + Age_55_64 + Age_65_125
)

ggdid(out3)

summary(out1); summary(out2); summary(out3)

