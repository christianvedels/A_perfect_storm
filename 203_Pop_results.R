# Pop results
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
      Variable == "Child_women_ratio" ~ "Child women ratio",
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
  labs(x = "Standardized values")

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

