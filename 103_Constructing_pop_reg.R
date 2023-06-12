# Pop reg data
# Date updated:   2023-06-12
# Auhtor:         Christian Vedel 
#
# Purpose:        Constructs the regression data used in the analysis        
# Output:         Pop_reg.csv

# ==== Libraries ====
library(tidyverse)
library(foreach)

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


# ==== Save data ====
write_csv2(reg_pop, "Data/Pop_reg.csv")
