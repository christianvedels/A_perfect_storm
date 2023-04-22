# Archaeological results
# Date updated:   2023-04-22
# Auhtor:         Christian Vedel 
#
# Purpose:        Archaeological results

# ==== Libraries ====
library(tidyverse)
source("000_Functions.R")


# ==== Load data =====
coins = read_csv2("Data/Reg_arch_coins.csv")
buildings = read_csv2("Data/Reg_arch_buildings.csv")



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


# ==== Bootstrapped data frames ====
# The following files can be recreated with 009_Archaeological_monte_carlo.R
# But this takes a while. They can also be donwloaded here: 
# https://www.dropbox.com/scl/fo/nxuv09eraovysu8bge9nu/h?dl=0&rlkey=a1d1lzxe04fzx6xa3fe0xvgq4

load("Data/Tmp_arch_samples/Buildings.Rdata")
samples_buildings = arch_sampler(res_is$Overall_Buildings$samples)

load("Data/Tmp_arch_samples/Coin findings.Rdata")
coins = res_is
rm(res_is)

