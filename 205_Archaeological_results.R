# Archaeological results
# Date updated:   2023-04-22
# Auhtor:         Christian Vedel 
#
# Purpose:        Archaeological results

# ==== Libraries ====
library(tidyverse)
source("000_Functions.R")

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



coins %>%  
  filter(limfjord_placement %in% c("west", "not")) %>% 
  ggplot(aes(
    x = rYear, y = activity, col = limfjord_placement
  )) + 
  # geom_point(alpha = 0.1) + 
  geom_smooth()

buildings %>%  
  filter(limfjord_placement %in% c("west", "not")) %>% 
  ggplot(aes(
    x = rYear, y = activity, col = limfjord_placement
  )) + 
  # geom_point(alpha = 0.1) + 
  geom_smooth()





