# Archaeological results
# Date updated:   2023-04-22
# Auhtor:         Christian Vedel 
#
# Purpose:        Archaeological results

# ==== Libraries ====
library(tidyverse)
source("000_Functions.R")
library(fixest)

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

# ==== Regressions ====

# MA approach
mod1 = feols(
  activity ~ Year*Affected,
  data = coins %>% mutate(Affected = delta_lMA_theta_1_alpha_10)
)

set.seed(20)
vcov1 = vcov_funciton_boot(
  activity ~ Year*Affected, 
  samples = samples_coins, 
  capB = 1000, 
  affected = "delta_lMA_theta_1_alpha_10"
)

mod1 = summary(
  mod1,
  vcov = vcov1$vcov
  )

plot_mod_arch(mod1, "arch_MA_coins", ref_year = 1000, the_col = "#DE7500")

# Plot of coefs in 1350
vcov1$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50) + 
  xlim(c(-0.3, 0)) + 
  theme_bw() + 
  geom_vline(xintercept = 0, lty = 2) + 
  geom_vline(
    xintercept = mod1$coefficients[names(mod1$coefficients)=="Year1350:Affected"]
  )



# Dummy approach
mod1 = feols(
  activity ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = coins %>% mutate(Affected = limfjord_placement_west)
)

set.seed(20)
vcov1 = vcov_funciton_boot(
  activity ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east, 
  samples = samples_coins, 
  capB = 1000, 
  affected = "limfjord_placement_west"
)

mod1 = summary(
  mod1,
  vcov = vcov1$vcov
)

plot_mod_arch(mod1, "arch_dummy_coins", ref_year = 1000, the_col = "#DE7500")

# Plot of coefs in 1350
vcov1$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50) + 
  xlim(c(-0.05, 0)) + 
  theme_bw() + 
  geom_vline(xintercept = 0, lty = 2) + 
  geom_vline(
    xintercept = mod1$coefficients[names(mod1$coefficients)=="Year1350:Affected"]
  )




# N 
# MA approach
mod1 = feols(
  log(n+1) ~ Year*Affected,
  data = coins %>% mutate(Affected = delta_lMA_theta_1_alpha_10)
)

set.seed(20)
vcov1 = vcov_funciton_boot(
  log(n+1) ~ Year*Affected, 
  samples = samples_coins, 
  capB = 1000, 
  affected = "delta_lMA_theta_1_alpha_10"
)

mod1 = summary(
  mod1,
  vcov = vcov1$vcov
)

plot_mod_arch(mod1, "arch_MA_coins", ref_year = 1000, the_col = "#DE7500")

# Plot of coefs in 1350
vcov1$beta_samples %>% 
  data.frame() %>% 
  ggplot(aes(Year1350.Affected)) + 
  geom_histogram(bins = 50) + 
  # xlim(c(-0.3, 0)) + 
  theme_bw() + 
  geom_vline(xintercept = 0, lty = 2) + 
  geom_vline(
    xintercept = mod1$coefficients[names(mod1$coefficients)=="Year1350:Affected"]
  )










mod1 = feols(
  activity ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = buildings %>% mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)

plot_mod_arch(mod1, "arch_dummy_buildings", ref_year = 1000, the_col = "#DE7500")
