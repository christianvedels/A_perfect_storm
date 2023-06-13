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
source("000_Functions.R")

# ==== Load data ====
reg_pop = read_csv2("Data/Pop_reg.csv", guess_max = 2000)

# ==== Factors ====
reg_pop = reg_pop %>% 
  mutate(
    Year = relevel(factor(Year), ref = "1801")
  )


# ==== Effect of channel on occupational structure ====

# Occupation
occ_effects_1901 = foreach(i = 1:7, .combine = "bind_rows") %do% {
  occ_i = reg_pop %>% 
    select(contains(paste0("hisco_1st_digit", i))) %>% 
    select(-contains("_f"), -contains("_m")) %>% unlist()
  
  # If hisco first digit is 1 then it should contain both 0 and 1
  if(i == 1){
    occ_0 = reg_pop %>% 
      select(contains(paste0("hisco_1st_digit", i-1))) %>% 
      select(-contains("_f"), -contains("_m")) %>% unlist()
    
    occ_i = occ_0 + occ_i
  }
  
  # If hisco first digit is 7 then it should contain both 7, 8 and 9
  if(i == 7){
    occ_8 = reg_pop %>% 
      select(contains(paste0("hisco_1st_digit", i+1))) %>% 
      select(-contains("_f"), -contains("_m")) %>% unlist()
    
    occ_9 = reg_pop %>% 
      select(contains(paste0("hisco_1st_digit", i+2))) %>% 
      select(-contains("_f"), -contains("_m")) %>% unlist()
    
    occ_i = occ_8 + occ_9 + occ_i
  }
  
  reg_pop$occ_i = occ_i
  
  exposed_pop = reg_pop %>% 
    filter(limfjord_placement_west == 1) %>% 
    summarise(
      mean(occ_i, na.rm = TRUE)
    ) %>% unlist()
  
  average_parish_size = reg_pop %>% 
    filter(limfjord_placement_west == 1) %>% 
    summarise(
      mean(Pop, na.rm = TRUE)
    ) %>% unlist()
  
  # log(x+1)
  mod_i_MA = feols(
    log(occ_i + 1) ~ Year*Affected,
    data = reg_pop %>% 
      mutate(Affected = delta_lMA_theta_1_alpha_10),
    cluster = ~ GIS_ID
  )
  
  mod_i_Dummy = feols(
    log(occ_i + 1) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
    data = reg_pop %>% 
      mutate(Affected = limfjord_placement_west),
    cluster = ~ GIS_ID
  )
  
  # Saving plots
  MA_logx1 = plot_mod( # MA approach
    mod_i_MA, 
    paste0("log_x1_HISCO_",i,"_MA"),
    corner_text = "Control group: Less Market Access improvement",
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Occupations/",
    return_data_and_plot = TRUE
  ) %>% 
    filter(Year == 1901)
  
  Dummy_logx1 = plot_mod( # Dummy approach
    mod_i_Dummy, 
    paste0("log_x1_HISCO_",i,"_Dummy"),
    corner_text = "Control: Non-Limfjord parishes",
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Occupations/",
    return_data_and_plot = TRUE
  ) %>% 
    filter(Year == 1901)
  
  # extensive
  mod_i_MA = feols(
    occ_i>0 ~ Year*Affected,
    data = reg_pop %>% 
      mutate(Affected = delta_lMA_theta_1_alpha_10),
    cluster = ~ GIS_ID
  )
  
  mod_i_Dummy = feols(
    occ_i>0 ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
    data = reg_pop %>% 
      mutate(Affected = limfjord_placement_west),
    cluster = ~ GIS_ID
  )
  
  # Saving plots
  MA_ext = plot_mod( # MA approach
    mod_i_MA, 
    paste0("extensive_HISCO_",i,"_MA"),
    corner_text = "Control group: Less Market Access improvement",
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Occupations/",
    return_data_and_plot = TRUE
  ) %>% 
    filter(Year == 1901)
  
  Dummy_ext = plot_mod( # Dummy approach
    mod_i_Dummy, 
    paste0("extensive_HISCO_",i,"_Dummy"),
    corner_text = "Control: Non-Limfjord parishes",
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Occupations/",
    return_data_and_plot = TRUE
  ) %>% 
    filter(Year == 1901)
  
  # intensive
  with_occ_consist = reg_pop %>% # IDs which consistently have >0
    filter(occ_i>0) %>% 
    group_by(GIS_ID) %>% 
    count() %>% 
    ungroup() %>% 
    filter(
      n == max(n) # Only those observed in all years with the above filter
    )
  
  exposed_pop_int = reg_pop %>% 
    filter(GIS_ID %in% with_occ_consist$GIS_ID) %>% 
    filter(limfjord_placement_west == 1) %>% 
    summarise(
      mean(occ_i, na.rm = TRUE)
    ) %>% unlist()
  
  average_parish_size_int = reg_pop %>% 
    filter(GIS_ID %in% with_occ_consist$GIS_ID) %>% 
    filter(limfjord_placement_west == 1) %>% 
    summarise(
      mean(Pop, na.rm = TRUE)
    ) %>% unlist()
  
  mod_i_MA = feols(
    log(occ_i) ~ Year*Affected,
    data = reg_pop %>% 
      mutate(Affected = delta_lMA_theta_1_alpha_10) %>% 
      filter(GIS_ID %in% with_occ_consist$GIS_ID),
    cluster = ~ GIS_ID
  )
  
  mod_i_Dummy = feols(
    log(occ_i) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
    data = reg_pop %>% 
      mutate(Affected = limfjord_placement_west) %>% 
      filter(GIS_ID %in% with_occ_consist$GIS_ID),
    cluster = ~ GIS_ID
  )
  
  # Saving plots
  MA_int = plot_mod( # MA approach
    mod_i_MA, 
    paste0("intensive_HISCO_",i,"_MA"),
    corner_text = "Control group: Less Market Access improvement",
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Occupations/",
    return_data_and_plot = TRUE
  ) %>% 
    filter(Year == 1901)
  
  Dummy_int = plot_mod( # Dummy approach
    mod_i_Dummy, 
    paste0("intensive_HISCO_",i,"_Dummy"),
    corner_text = "Control: Non-Limfjord parishes",
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Occupations/",
    return_data_and_plot = TRUE
  ) %>% 
    filter(Year == 1901)
  
  # asinh()
  mod_i_MA = feols(
    asinh(occ_i) ~ Year*Affected,
    data = reg_pop %>% 
      mutate(Affected = delta_lMA_theta_1_alpha_10),
    cluster = ~ GIS_ID
  )
  
  mod_i_Dummy = feols(
    asinh(occ_i) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
    data = reg_pop %>% 
      mutate(Affected = limfjord_placement_west),
    cluster = ~ GIS_ID
  )
  
  # Saving plots
  MA_asinh = plot_mod( # MA approach
    mod_i_MA, 
    paste0("asinh_HISCO_",i,"_MA"),
    corner_text = "Control group: Less Market Access improvement",
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Occupations/",
    return_data_and_plot = TRUE
  ) %>% 
    filter(Year == 1901)
  
  Dummy_asinh = plot_mod( # Dummy approach
    mod_i_Dummy, 
    paste0("asinh_HISCO_",i,"_Dummy"),
    corner_text = "Control: Non-Limfjord parishes",
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Occupations/",
    return_data_and_plot = TRUE
  ) %>% 
    filter(Year == 1901)
  
  data.frame(
    Exposed_pop = c(rep(exposed_pop, 4), rep(exposed_pop_int, 2), rep(exposed_pop, 2)),
    average_parish_size = c(rep(average_parish_size, 4), rep(average_parish_size_int, 2), rep(average_parish_size, 2)),
    Affected = rep(c("MA", "Dummy"), 4),
    Approach = c("log(x+1)", "log(x+1)", "Extensive", "Extensive", "Intensive", "Intensive", "asinh(x)", "asinh(x)"),
    Estimate = c(
      MA_logx1$Estimate, 
      Dummy_logx1$Estimate, 
      MA_ext$Estimate, 
      Dummy_ext$Estimate,
      MA_int$Estimate,
      Dummy_int$Estimate,
      MA_asinh$Estimate,
      Dummy_asinh$Estimate
    ),
    Std = c(
      MA_logx1$Std..Error, 
      Dummy_logx1$Std..Error, 
      MA_ext$Std..Error, 
      Dummy_ext$Std..Error,
      MA_int$Std..Error,
      Dummy_int$Std..Error,
      MA_asinh$Std..Error,
      Dummy_asinh$Std..Error
    ),
    Pval = c(
      MA_logx1$Pr...t.., 
      Dummy_logx1$Pr...t..,
      MA_ext$Pr...t.., 
      Dummy_ext$Pr...t..,
      MA_int$Pr...t..,
      Dummy_int$Pr...t..,
      MA_asinh$Pr...t..,
      Dummy_asinh$Pr...t..
    ),
    Pretrend_est = c(
      MA_logx1$Pretrend_est, 
      Dummy_logx1$Pretrend_est, 
      MA_ext$Pretrend_est, 
      Dummy_ext$Pretrend_est,
      MA_int$Pretrend_est,
      Dummy_int$Pretrend_est,
      MA_asinh$Pretrend_est,
      Dummy_asinh$Pretrend_est
    ),
    Pretrend_pval = c(
      MA_logx1$Pretrend_pval, 
      Dummy_logx1$Pretrend_pval, 
      MA_ext$Pretrend_pval, 
      Dummy_ext$Pretrend_pval,
      MA_int$Pretrend_pval,
      Dummy_int$Pretrend_pval,
      MA_asinh$Pretrend_pval,
      Dummy_asinh$Pretrend_pval
    ),
    hisco = i,
    n_parishes = c(rep(1589, 4), rep(NROW(with_occ_consist), 2), rep(1589, 2))
  )
}

# Key with description
key_desc = data.frame(
  hisco = c(1:7),
  description = c( # From: https://historyofwork.iisg.nl/major.php
    "0/1: Professional, technical and\nrelated workers",
    "2: Administrative and managerial\nworkers",
    "3: Clerical and related workers",
    "4: Sales workers",
    "5: Service workers",
    "6: Agricultural, animal husbandry\n and forestry workers, fishermen\nand hunters",
    "7/8/9: Production and related\nworkers, transportequipment operators\nand labourers"
  )
)

# What is a meaningfull effect?
# For the dummy approach: 0.05, MA approach 0.5
occ_effects_1901 %>% 
  filter(
    ifelse(
      Affected == "MA", abs(Estimate)>0.25, abs(Estimate)>0.05
    )
  ) %>% 
  left_join(key_desc, by = "hisco")
# There is possibly a meaningful effect to all occupations (but not on all margins)

meaningfull_effects = data.frame(
  Affected = rep(c("MA", "Dummy")),
  Effect_size_upper = c(0.05, 0.25),
  Effect_size_lower = -c(0.05, 0.25)
)

# Bonferoni correction
occ_effects_1901 = occ_effects_1901 %>% 
  mutate(
    Pval_holm = p.adjust(Pval, method = "holm")
  ) %>%
  mutate(
    stars = case_when(
      Pval_holm < 0.01 ~ "***",
      Pval_holm < 0.05 ~ "**",
      Pval_holm < 0.1 ~ "*",
      TRUE ~ ""
    )
  )

# APE
occ_effects_1901 = occ_effects_1901 %>% 
  mutate(
    Upper = Estimate + 1.96*Std,
    Lower = Estimate - 1.96*Std
  ) %>% 
  mutate(
    APE = Exposed_pop*Estimate,
    APE_upper = Exposed_pop*Upper,
    APE_lower = Exposed_pop*Lower
  ) %>% 
  mutate(
    APE_pct = APE/average_parish_size,
    APE_pct_lower = APE_lower/average_parish_size,
    APE_pct_upper = APE_upper/average_parish_size
  )

# Make plot
nudge = 0.05
p1 = occ_effects_1901 %>% 
  mutate(
    Approach = case_when(
      Approach == "Extensive" ~ paste0("1: ", Approach),
      Approach == "Intensive" ~ paste0("2: ", Approach),
      Approach == "log(x+1)" ~ paste0("3: ", Approach),
      Approach == "asinh(x)" ~ paste0("4: ", Approach)
    )
  ) %>% 
  filter(
    n_parishes > 100
  ) %>%
  left_join(key_desc, by = "hisco") %>% 
  left_join(meaningfull_effects, by = "Affected") %>% 
  group_by(hisco) %>% 
  mutate(
    Effect_size_upper = Effect_size_upper*mean(exposed_pop)/mean(average_parish_size),  
    Effect_size_lower = Effect_size_lower*mean(exposed_pop)/mean(average_parish_size)
  ) %>% 
  ungroup() %>% 
  mutate(
    intensive_text = ifelse(Approach == "2: Intensive", n_parishes, "")
  ) %>% 
  mutate(
    intensive_text = ifelse(Pretrend_pval<0.05, paste0(intensive_text,"*"), intensive_text)
  ) %>% 
  mutate(
    Approach = factor(Approach, levels = c("1: Extensive", "2: Intensive", "3: log(x+1)", "4: asinh(x)"))
  ) %>% 
  ggplot(aes(Approach, APE_pct, col = Affected)) + 
  geom_point(position = position_nudge(x = c(nudge, -nudge))) + 
  geom_errorbar(aes(ymin = APE_pct_lower, ymax = APE_pct_upper), position = position_nudge(x = c(nudge, -nudge))) +
  facet_wrap(~description, scales = "free_y") + 
  geom_hline(yintercept = 0) + 
  geom_text(
    aes(label = intensive_text),
    position = position_nudge(
      x = c(4*nudge, -4*nudge)
    )
    ) + 
  theme_bw() + 
  scale_color_manual(values = c(Dummy = "#2c5c34", MA = "#b33d3d")) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) + 
  geom_hline(aes(yintercept = Effect_size_upper, col = Affected), lty = 2) +
  geom_hline(aes(yintercept = Effect_size_lower, col = Affected), lty = 2) +
  labs(
    x = "",
    y = "APE share"
  )

p1
ggsave("Plots/Mechanism/All_occupations.png", plot = p1, width = 2*10, height = 2*8, units = "cm")

# Table of all
table0 = occ_effects_1901 %>% 
  mutate(
    Approach = case_when(
      Approach == "Extensive" ~ paste0("1: ", Approach),
      Approach == "Intensive" ~ paste0("2: ", Approach),
      Approach == "log(x+1)" ~ paste0("3: ", Approach),
      Approach == "asinh(x)" ~ paste0("4: ", Approach)
    )
  ) %>% 
  arrange(-abs(Estimate)) %>% 
  left_join(key_desc, by = "hisco") %>% 
  arrange(hisco) %>% 
  mutate(
    Pretrend_stars = case_when(
      Pretrend_pval < 0.01 ~ "***",
      Pretrend_pval < 0.05 ~ "**",
      Pretrend_pval < 0.1 ~ "*",
      TRUE ~ ""
    )
  ) %>% 
  mutate_all(Round0) %>%
  mutate(
    Estimate = paste0(Estimate, stars, " (", Std, ")"),
    Pretrend_pval = paste0(Pretrend_pval, Pretrend_stars)
  ) %>% 
  select(hisco, Affected, Approach, Estimate, Pretrend_pval, APE, APE_pct, n_parishes)

table0 %>% # For appendix
  arrange(hisco) %>% 
  knitr::kable("latex", booktabs = TRUE, align = "c")


# ==== What is inside '6' and '7/8/9'? ====
reg_pop %>% 
  select(hisco_2nd_digit60:hisco_2nd_digit99) %>% 
  summarise_all(sum0)

# ==== Was it fishing or other types of agriculture? ====
reg_pop = reg_pop %>% 
  mutate(
    Agri_not_fish = hisco_1st_digit6 - Fishing
  )

fish = feols(
  log(Fishing + 1) ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)
plot_mod(
  fish, "fish_MA", dir0 = "Plots/Mechanism/", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

# What part of agriculture?
fish = feols(
  log(Agri_not_fish + 1) ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)
plot_mod(
  fish, "agri_not_fish_MA", dir0 = "Plots/Mechanism/", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

fish = feols(
  log(Fishing + 1) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% 
    mutate(Affected = limfjord_placement_west),
  cluster = ~ GIS_ID
)
plot_mod(
  fish, "fish_dummy", dir0 = "Plots/Mechanism/", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

# What part of agriculture?
fish = feols(
  log(Agri_not_fish + 1) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% 
    mutate(Affected = limfjord_placement_west),
  cluster = ~ GIS_ID
)
plot_mod(
  fish, "agri_not_fish_dummy", dir0 = "Plots/Mechanism/", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34", corner_text = "Control group: Less Market Access improvement"
)

# ==== Effect of channel on demographics ====

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

