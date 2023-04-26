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

# ==== Read data ====
reg_pop = read_csv2("Data/Popdata.csv", guess_max = 2000)
geo_data = read_csv2("Data/Geo.csv", guess_max = 2000)
market_access = read_csv2("Data/MA_estimates.csv", guess_max = 2000) 

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

# ==== Regressions ====
mod1 = feols(
  log(Pop) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% mutate(Affected = limfjord_placement_west),
  cluster = ~ GIS_ID
)

p1_dummy = plot_mod(mod1, "pop_dummy", ylab = "Parameter estimate")

p1_dummy

mod2 = feols(
  log(Pop) ~ Year*Affected,
  data = reg_pop %>% mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)

p2_ma = plot_mod(mod2, "pop_MA", ylab = "Parameter estimate")

p2_ma



# ==== Mechanism occupation ====
# Breach --> Fishing
fish = feols(
  log(Fishing+1) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% 
    mutate(Affected = limfjord_placement_west) %>% 
    filter(wo_kbh),
  cluster = ~ GIS_ID
)
plot_mod(
  fish, "fish", ylab = "Parameter estimate", vadj = 0.15, the_col = "#2c5c34"
)

# Breach --> Manufacturing
manu = feols(
  log(Manufacturing+1) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% mutate(Affected = limfjord_placement_west),
  cluster = ~ GIS_ID
)
plot_mod(
  manu, "manu", ylab = "Parameter estimate", vadj = 0.15, the_col = "#2c5c34"
)

# ==== Mechanism internal migration ====
# Breach --> Migration
migr = feols(
  log(Born_different_county) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% 
    mutate(Affected = limfjord_placement_west) %>% 
    filter(as.numeric(as.character(Year)) >= 1845) %>% 
    mutate(Year = relevel(Year, ref = "1845")),
  cluster = ~ GIS_ID
)
plot_mod(
  migr, "born_different", ylab = "Parameter estimate", 
  vadj = 0.15, the_col = "#2c5c34", ref_year = 1845
)



# ==== Effect by age group and gender ====
# Young children per woman
fertility = feols(
  log(Small_children_per_woman) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% 
    mutate(Affected = limfjord_placement_west) %>% 
    mutate(Small_children_per_woman = (Age_1_4) / Pop_f),
  cluster = ~ GIS_ID
)
plot_mod(
  fertility, "fertility", ylab = "Parameter estimate", 
  vadj = 0.15, the_col = "#2c5c34"
)

# Young men
mf = feols(
  log(mf_ratio_15_24) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% 
    mutate(Affected = limfjord_placement_west) %>% 
    mutate(mf_ratio_15_24 = Age_25_34_m / Age_25_34_f),
  cluster = ~ GIS_ID
)
plot_mod(
  mf, "young_male", ylab = "Parameter estimate", 
  vadj = 0.15, the_col = "#2c5c34"
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
