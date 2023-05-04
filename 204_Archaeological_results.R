# Archaeological results
# Date updated:   2023-04-22
# Auhtor:         Christian Vedel 
#
# Purpose:        Archaeological results

# ==== Libraries ====
library(tidyverse)
source("000_Functions.R")
library(fixest)
library(foreach)

# ==== Bootstrapped data frames ====
# The following files can be recreated with 009_Archaeological_monte_carlo.R
# But this takes a while. They can also be donwloaded here:
# https://www.dropbox.com/scl/fo/nxuv09eraovysu8bge9nu/h?dl=0&rlkey=a1d1lzxe04fzx6xa3fe0xvgq4
#
# load("Data/Tmp_arch_samples/Buildings.Rdata")
# samples_buildings = arch_sampler(arch_samples = res_is$Overall_Buildings$samples)
# 
# load("Data/Tmp_arch_samples/Coin findings.Rdata")
# samples_coins = arch_sampler(arch_samples = res_is$`Overall_Coin findings`$samples)
# 
# save(samples_buildings, samples_coins, file = "Data/Tmp_reg_data_arch_samples.Rdata")
load("Data/Tmp_reg_data_arch_samples.Rdata")

# ==== Load data =====
coins = read_csv2("Data/Reg_arch_coins.csv", guess_max = 2000)
buildings = read_csv2("Data/Reg_arch_buildings.csv", guess_max = 2000)
geo_data = read_csv2("Data/Geo.csv", guess_max = 2000)
market_access = read_csv2("Data/MA_estimates.csv", guess_max = 2000) 
arch_raw = read_csv2("Data/Arch.csv", guess_max = 2000)
matched_parishes = read_csv2("Data/Matched_parishes.csv")

# ==== Default colors ====
# Regions:
regions_col = c(
  reference = "black",
  east = "#273a8f",
  middle = "#2c5c34",
  west = "#b33d3d"
)

# ==== Functions ====

# plot_mod_arch()
# Function that produces a plot of regressions coefficients
plot_mod_arch = function(
    mod, 
    fname, 
    ref_year = 1801, 
    Parishes = 0, 
    ylab = "",
    vadj = 0.15,
    the_col = "#b33d3d",
    corner_text = "Control: Non-Limfjord parishes"
){
  capN = mod$nobs
  
  p1 = mod$coeftable %>%
    data.frame() %>%
    mutate(Var = rownames(.)) %>%
    remove_rownames() %>%
    mutate(
      Year = gsub(":Affected|Year","", Var) %>% as.numeric()
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
    ) %>%
    ggplot(aes(Year, Estimate)) +
    geom_point(col = the_col) +
    geom_errorbar(aes(ymax = Upper, ymin = Lower), col = the_col) +
    geom_hline(yintercept = 0, lty = 2) +
    theme_bw() +
    geom_vline(xintercept = c(1086, 1200), lty = 2) +
    scale_x_continuous(breaks = seq(750, 1500, by = 50)) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5)
    ) +
    theme(legend.position = "bottom",
          plot.caption = element_text(vjust = 20))  + 
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

# ==== Small dataficiation ====
coins = coins %>% 
  fastDummies::dummy_cols("limfjord_placement") %>% 
  mutate(
    Year = relevel(factor(rYear), ref = "1000")
  )

buildings = buildings %>% 
  fastDummies::dummy_cols("limfjord_placement") %>% 
  mutate(
    Year = relevel(factor(rYear), ref = "1000")
  )

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
samples_coins = lapply(samples_coins, function(x){
  x %>%
    left_join(geo_data, by = "GIS_ID") %>%
    left_join(market_access, by = "GIS_ID") %>%
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>%
    mutate(
      Year = relevel(factor(rYear), ref = "1000")
    ) %>%
    fastDummies::dummy_cols("limfjord_placement")
})

# Adding geo and MA to samples
samples_buildings = lapply(samples_buildings, function(x){
  x %>%
    left_join(geo_data, by = "GIS_ID") %>%
    left_join(market_access, by = "GIS_ID") %>%
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>%
    mutate(
      Year = relevel(factor(rYear), ref = "1000")
    ) %>%
    fastDummies::dummy_cols("limfjord_placement")
})

# ==== Clean raw data for descriptive plot ====
# Arch raw
arch_raw = arch_raw %>% 
  left_join(geo_data, by = "GIS_ID")

arch_raw1 = foreach(y = seq(750, 1500, by = 50), .combine = "bind_rows") %do% {
  res_i = arch_raw %>% 
    filter(finding_interpretation_en %in% c("Coin findings")) %>% 
    # Filter arch_raw by the year range
    filter(y >= From_year) %>% 
    filter(y <= To_year) %>% 
    group_by(GIS_ID) %>% 
    count() %>% 
    mutate(Year = y)
  
  # Report status to console
  cat(y, "         \r")
  return(res_i)
}

arch_raw1 = expand.grid(
  Year = seq(750, 1500, by = 50),
  GIS_ID = geo_data$GIS_ID
) %>% 
  left_join(arch_raw1, by = c("GIS_ID", "Year")) %>% 
  mutate(
    n = ifelse(is.na(n), 0, n)
  ) %>% 
  left_join(geo_data, by = "GIS_ID")

# ==== Descriptive with raw data ====

p1 = arch_raw1 %>% 
  mutate(
    limfjord_placement = ifelse(limfjord_placement == "not", "reference", limfjord_placement)
  ) %>% 
  group_by(Year, limfjord_placement) %>% 
  # filter(limfjord_placement != "middle") %>%
  ggplot(aes(Year, log(n+1), col = limfjord_placement)) + 
  geom_smooth(se = FALSE, lty = 5, size = 1) +
  theme_bw() +
  geom_vline(xintercept = c(1086, 1200), lty = 2) +
  scale_x_continuous(breaks = seq(750, 1500, by = 50)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) +  
  scale_color_manual(
    values = regions_col
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  labs(
    col = "Location in Limfjord:",
    y = "log(findings + 1)"
  )
  
p1
ggsave("Plots/Arch_descriptive.png", plot = p1, width = 8, height = 5)
  
# ==== Regressions full samples ====
# MA approach
mod1 = feols(
  activity ~ Year*Affected,
  data = coins %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10)
)

set.seed(20)
vcov1 = vcov_function_boot(
  activity ~ Year*Affected, 
  samples = samples_coins, 
  capB = 1000, 
  affected = "delta_lMA_theta_1_alpha_10"
)

mod1 = summary(
  mod1,
  vcov = vcov1$vcov
  )

plot_mod_arch(mod1, "arch_MA_coins", ref_year = 1000, the_col = regions_col["west"])

# Plot of coefs in 1350
p2 = vcov1$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50, fill = regions_col["west"]) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod1$coefficients[names(mod1$coefficients)=="Year1350:Affected"],
    lty = 2
  ) + 
  labs(
    x = "Parameter estimate: Affected x Year 1350"
  )

p2
fname0 = paste0("Plots/Regression_plots/", "arch_MA_coins_boot", ".png")
ggsave(fname0,  plot = p2, width = 10, height = 8, units = "cm")



# Dummy approach
mod2 = feols(
  activity ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = coins %>% mutate(Affected = limfjord_placement_west)
)

set.seed(20)
vcov2 = vcov_function_boot(
  activity ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east, 
  samples = samples_coins, 
  capB = 1000, 
  affected = "limfjord_placement_west"
)

mod2 = summary(
  mod2,
  vcov = vcov2$vcov
)

plot_mod_arch(mod2, "arch_dummy_coins", ref_year = 1000, the_col = regions_col["west"])

# Plot of coefs in 1350
p2 = vcov2$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50, fill = regions_col["west"]) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod2$coefficients[names(mod2$coefficients)=="Year1350:Affected"],
    lty = 2
  ) + 
  labs(
    x = "Parameter estimate: Affected x Year 1350"
  )

p2
fname0 = paste0("Plots/Regression_plots/", "arch_dummy_coins_boot", ".png")
ggsave(fname0,  plot = p2, width = 10, height = 8, units = "cm")


# Buildings
# MA approach
mod3 = feols(
  activity ~ Year*Affected,
  data = buildings %>% mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)

set.seed(20)
vcov3 = vcov_function_boot(
  activity ~ Year*Affected, 
  samples = samples_buildings, 
  capB = 1000, 
  affected = "delta_lMA_theta_1_alpha_10"
)

mod3 = summary(
  mod3,
  vcov = vcov3$vcov
)

plot_mod_arch(mod3, "arch_MA_buildings", ref_year = 1000, the_col = regions_col["middle"])

# Plot of coefs in 1350
p2 = vcov3$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50, fill = regions_col["middle"]) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod3$coefficients[names(mod3$coefficients)=="Year1350:Affected"],
    lty = 2
  ) + 
  labs(
    x = "Parameter estimate: Affected x Year 1350"
  )

p2
fname0 = paste0("Plots/Regression_plots/", "arch_MA_buildings_boot", ".png")
ggsave(fname0,  plot = p2, width = 10, height = 8, units = "cm")

# Dummy approach
mod4 = feols(
  activity ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = buildings %>% mutate(Affected = limfjord_placement_west),
  cluster = ~ GIS_ID
)

set.seed(20)
vcov4 = vcov_function_boot(
  activity ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east, 
  samples = samples_buildings, 
  capB = 1000, 
  affected = "limfjord_placement_west"
)

mod4 = summary(
  mod4,
  vcov = vcov4$vcov
)

plot_mod_arch(mod4, "arch_dummy_buildings", ref_year = 1000, the_col = regions_col["middle"])

# Plot of coefs in 1350
p2 = vcov4$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50, fill = regions_col["middle"]) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod4$coefficients[names(mod4$coefficients)=="Year1350:Affected"],
    lty = 2
  ) + 
  labs(
    x = "Parameter estimate: Affected x Year 1350"
  )

p2
fname0 = paste0("Plots/Regression_plots/", "arch_dummy_buildings_boot", ".png")
ggsave(fname0,  plot = p2, width = 10, height = 8, units = "cm")


# ==== Regressions matched sample ====
samples_buildings_matched = lapply(samples_buildings, function(x) {
  x %>% 
    filter(GIS_ID %in% matched_parishes$GIS_ID)
})
samples_coins_matched = lapply(samples_coins, function(x) {
  x %>% 
    filter(GIS_ID %in% matched_parishes$GIS_ID)
})

# MA approach
mod5 = feols(
  activity ~ Year*Affected,
  data = coins %>% 
    filter(GIS_ID %in% matched_parishes$GIS_ID) %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10)
)

set.seed(20)
vcov5 = vcov_function_boot(
  activity ~ Year*Affected, 
  samples = samples_coins_matched, 
  capB = 1000, 
  affected = "delta_lMA_theta_1_alpha_10",
)

mod5 = summary(
  mod5,
  vcov = vcov5$vcov
)

plot_mod_arch(mod5, "arch_MA_coins_matched", ref_year = 1000, the_col = "#DE7500")

# Plot of coefs in 1350
vcov5$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod5$coefficients[names(mod1$coefficients)=="Year1350:Affected"],
    lty = 2
  )


# Dummy approach
mod6 = feols(
  activity ~ Year*Affected,
  data = coins %>% 
    filter(GIS_ID %in% matched_parishes$GIS_ID) %>%  
    mutate(Affected = limfjord_placement_west)
)

set.seed(20)
vcov6 = vcov_function_boot(
  activity ~ Year*Affected, 
  samples = samples_coins_matched, 
  capB = 1000, 
  affected = "limfjord_placement_west"
)

mod6 = summary(
  mod6,
  vcov = vcov6$vcov
)

plot_mod_arch(mod6, "arch_dummy_coins_matched", ref_year = 1000, the_col = "#DE7500")

# Plot of coefs in 1350
vcov6$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod6$coefficients[names(mod6$coefficients)=="Year1350:Affected"],
    lty = 2
  )


# Buildings
# MA approach
mod7 = feols(
  activity ~ Year*Affected,
  data = buildings %>% 
    filter(GIS_ID %in% matched_parishes$GIS_ID) %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)

set.seed(20)
vcov7 = vcov_function_boot(
  activity ~ Year*Affected, 
  samples = samples_buildings_matched, 
  capB = 1000, 
  affected = "delta_lMA_theta_1_alpha_10"
)

mod7 = summary(
  mod7,
  vcov = vcov7$vcov
)

plot_mod_arch(mod7, "arch_MA_buildings_matched", ref_year = 1000, the_col = "#273a8f")

# Plot of coefs in 1350
vcov7$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod7$coefficients[names(mod7$coefficients)=="Year1350:Affected"],
    lty = 2
  )

# Dummy approach
mod8 = feols(
  activity ~ Year*Affected,
  data = buildings %>% 
    filter(GIS_ID %in% matched_parishes$GIS_ID) %>% 
    mutate(Affected = limfjord_placement_west),
  cluster = ~ GIS_ID
)

set.seed(20)
vcov8 = vcov_function_boot(
  activity ~ Year*Affected, 
  samples = samples_buildings_matched, 
  capB = 1000, 
  affected = "limfjord_placement_west"
)

mod8 = summary(
  mod8,
  vcov = vcov8$vcov
)

plot_mod_arch(mod8, "arch_dummy_buildings_matched", ref_year = 1000, the_col = "#273a8f")

# Plot of coefs in 1350
vcov8$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod8$coefficients[names(mod8$coefficients)=="Year1350:Affected"],
    lty = 2
  )

# ==== Make regression table ====
mods = list(
  mod1,
  mod2,
  mod3,
  mod4,
  mod5,
  mod6,
  mod7,
  mod8
)

mods %>% 
  etable(tex = TRUE)


# To show all parameters in the appendix
mods = list(
  mod1,
  mod2,
  mod3,
  mod4
)

mods %>% 
  etable(tex = TRUE)

mods = list(
  mod5,
  mod6,
  mod7,
  mod8
)

mods %>% 
  etable(tex = TRUE)



# ==== Regressions based on data from the normal distribution ====
# The following files can be recreated with 009_Archaeological_monte_carlo.R
# But this takes a while. They can also be donwloaded here:
# https://www.dropbox.com/s/62yxdax9eaqedu9/Tmp_reg_data_arch_samples_norm.Rdata?dl=0
# 
# load("Data/Tmp_arch_samples_norm/Buildings.Rdata")
# samples_buildings = arch_sampler(arch_samples = res_is$Overall_Buildings$samples)
# 
# load("Data/Tmp_arch_samples_norm/Coin findings.Rdata")
# samples_coins = arch_sampler(arch_samples = res_is$`Overall_Coin findings`$samples)
# 
# save(samples_buildings, samples_coins, file = "Data/Tmp_reg_data_arch_samples_norm.Rdata")
load("Data/Tmp_reg_data_arch_samples_norm.Rdata")

coins = read_csv2("Data/Reg_arch_coins_norm.csv", guess_max = 2000) %>% 
  fastDummies::dummy_cols("limfjord_placement") %>% 
  mutate(
    Year = relevel(factor(rYear), ref = "1000")
  )
buildings = read_csv2("Data/Reg_arch_buildings_norm.csv", guess_max = 2000) %>% 
  fastDummies::dummy_cols("limfjord_placement") %>% 
  mutate(
    Year = relevel(factor(rYear), ref = "1000")
  )

# Adding geo and MA to samples
samples_coins = lapply(samples_coins, function(x){
  x %>%
    left_join(geo_data, by = "GIS_ID") %>%
    left_join(market_access, by = "GIS_ID") %>%
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>%
    mutate(
      Year = relevel(factor(rYear), ref = "1000")
    ) %>%
    fastDummies::dummy_cols("limfjord_placement")
})

# Adding geo and MA to samples
samples_buildings = lapply(samples_buildings, function(x){
  x %>%
    left_join(geo_data, by = "GIS_ID") %>%
    left_join(market_access, by = "GIS_ID") %>%
    mutate(Affected = delta_lMA_theta_1_alpha_10) %>%
    mutate(
      Year = relevel(factor(rYear), ref = "1000")
    ) %>%
    fastDummies::dummy_cols("limfjord_placement")
})

# ==== Regressions norm ====
# MA approach
mod1 = feols(
  activity ~ Year*Affected,
  data = coins %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10)
)

set.seed(20)
vcov1 = vcov_function_boot(
  activity ~ Year*Affected, 
  samples = samples_coins, 
  capB = 1000, 
  affected = "delta_lMA_theta_1_alpha_10"
)

mod1 = summary(
  mod1,
  vcov = vcov1$vcov
)

plot_mod_arch(mod1, "arch_MA_coins_norm", ref_year = 1000, the_col = regions_col["west"])

# Plot of coefs in 1350
p2 = vcov1$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50, fill = regions_col["west"]) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod1$coefficients[names(mod1$coefficients)=="Year1350:Affected"],
    lty = 2
  ) + 
  labs(
    x = "Parameter estimate: Affected x Year 1350"
  )

p2
fname0 = paste0("Plots/Regression_plots/", "arch_MA_coins_boot_norm", ".png")
ggsave(fname0,  plot = p2, width = 10, height = 8, units = "cm")


# Dummy approach
mod2 = feols(
  activity ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = coins %>% mutate(Affected = limfjord_placement_west)
)

set.seed(20)
vcov2 = vcov_function_boot(
  activity ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east, 
  samples = samples_coins, 
  capB = 1000, 
  affected = "limfjord_placement_west"
)

mod2 = summary(
  mod2,
  vcov = vcov2$vcov
)

plot_mod_arch(mod2, "arch_dummy_coins_norm", ref_year = 1000, the_col = regions_col["west"])

# Plot of coefs in 1350
p2 = vcov2$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50, fill = regions_col["west"]) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod2$coefficients[names(mod2$coefficients)=="Year1350:Affected"],
    lty = 2
  ) + 
  labs(
    x = "Parameter estimate: Affected x Year 1350"
  )

p2
fname0 = paste0("Plots/Regression_plots/", "arch_dummy_coins_boot_norm", ".png")
ggsave(fname0,  plot = p2, width = 10, height = 8, units = "cm")

# Buildings
# MA approach
mod3 = feols(
  activity ~ Year*Affected,
  data = buildings %>% mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)

set.seed(20)
vcov3 = vcov_function_boot(
  activity ~ Year*Affected, 
  samples = samples_buildings, 
  capB = 1000, 
  affected = "delta_lMA_theta_1_alpha_10"
)

mod3 = summary(
  mod3,
  vcov = vcov3$vcov
)

plot_mod_arch(mod3, "arch_MA_buildings_norm", ref_year = 1000, the_col = regions_col["middle"])

# Plot of coefs in 1350
p2 = vcov3$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50, fill = regions_col["middle"]) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod3$coefficients[names(mod3$coefficients)=="Year1350:Affected"],
    lty = 2
  ) + 
  labs(
    x = "Parameter estimate: Affected x Year 1350"
  )

p2
fname0 = paste0("Plots/Regression_plots/", "arch_MA_buildings_boot_norm", ".png")
ggsave(fname0,  plot = p2, width = 10, height = 8, units = "cm")

# Dummy approach
mod4 = feols(
  activity ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = buildings %>% mutate(Affected = limfjord_placement_west),
  cluster = ~ GIS_ID
)

set.seed(20)
vcov4 = vcov_function_boot(
  activity ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east, 
  samples = samples_buildings, 
  capB = 1000, 
  affected = "limfjord_placement_west"
)

mod4 = summary(
  mod4,
  vcov = vcov4$vcov
)

plot_mod_arch(mod4, "arch_dummy_buildings_norm", ref_year = 1000, the_col = regions_col["middle"])

# Plot of coefs in 1350
p2 = vcov4$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50, fill = regions_col["middle"]) + 
  theme_bw() + 
  geom_vline(xintercept = 0) + 
  geom_vline(
    xintercept = mod4$coefficients[names(mod4$coefficients)=="Year1350:Affected"],
    lty = 2
  ) + 
  labs(
    x = "Parameter estimate: Affected x Year 1350"
  )

p2
fname0 = paste0("Plots/Regression_plots/", "arch_dummy_buildings_boot_norm", ".png")
ggsave(fname0,  plot = p2, width = 10, height = 8, units = "cm")
