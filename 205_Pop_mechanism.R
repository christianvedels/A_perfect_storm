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
    filter(Year == 1901) %>% 
    summarise(
      mean(occ_i, na.rm = TRUE)
    ) %>% unlist()
  
  average_parish_size = reg_pop %>%
    filter(Year == 1901) %>% 
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
    filter(Year == 1901) %>% 
    filter(GIS_ID %in% with_occ_consist$GIS_ID) %>% 
    filter(limfjord_placement_west == 1) %>% 
    summarise(
      mean(occ_i, na.rm = TRUE)
    ) %>% unlist()
  
  average_parish_size_int = reg_pop %>%
    filter(Year == 1901) %>% 
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
    "0/1: Professional, technical",
    "2: Admin. and managerial",
    "3: Clerical and related",
    "4: Sales workers",
    "5: Service workers",
    "6: Agricultural",
    "7/8/9: Production and related"
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
  Effect_size_upper = c(1.591198*0.05, 0.236446*0.05),
  Effect_size_lower = -c(1.591198*0.05, 0.236446*0.05)
)

# Bonferoni correction
bonf_crit = abs(qnorm(0.025/NROW(occ_effects_1901)))

occ_effects_1901 = occ_effects_1901 %>% 
  mutate(
    Pval_bonf = p.adjust(Pval, method = "bonferroni")
  ) %>%
  mutate(
    stars = case_when(
      Pval_bonf < 0.01 ~ "***",
      Pval_bonf < 0.05 ~ "**",
      Pval_bonf < 0.1 ~ "*",
      TRUE ~ ""
    )
  ) %>% 
  mutate(
    Upper = Estimate + bonf_crit*Std,
    Lower = Estimate - bonf_crit*Std
  ) %>% 
  ungroup()

# APE
occ_effects_1901 = occ_effects_1901 %>% 
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
  ungroup() %>% 
  mutate(
    intensive_text = ifelse(Approach == "2: Intensive", n_parishes, "")
  ) %>% 
  # mutate(
  #   intensive_text = ifelse(Pretrend_pval<0.05, paste0(intensive_text,"*"), intensive_text)
  # ) %>% 
  mutate(
    Approach = factor(Approach, levels = c("1: Extensive", "2: Intensive", "3: log(x+1)", "4: asinh(x)"))
  ) %>% 
  ggplot(aes(Approach, APE_pct, col = Affected)) + 
  geom_point(position = position_nudge(x = c(nudge, -nudge))) + 
  geom_errorbar(aes(ymin = APE_pct_lower, ymax = APE_pct_upper), position = position_nudge(x = c(nudge, -nudge))) +
  facet_wrap(~description) + 
  geom_hline(yintercept = 0) + 
  geom_text(
    aes(label = intensive_text),
    position = position_nudge(
      x = c(4*nudge, -4*nudge),
      y = c(2*nudge, -2*nudge)
    )
    ) + 
  theme_bw() + 
  scale_color_manual(values = c(Dummy = "#2c5c34", MA = "#b33d3d")) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) + 
  # geom_hline(aes(yintercept = Effect_size_upper, col = Affected), lty = 2) +
  # geom_hline(aes(yintercept = Effect_size_lower, col = Affected), lty = 2) +
  labs(
    x = "",
    y = "APE share"
  ) +
  theme(legend.position = "bottom")

p1
ggsave("Plots/Mechanism/All_occupations.png", plot = p1, width = 1.9*8, height = 1.9*9, units = "cm")

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
  select(hisco, Affected, Approach, Estimate, n_parishes) %>% 
  arrange(hisco) %>% 
  knitr::kable("latex", booktabs = TRUE, align = "c")

# Look up parameters
table0 %>% 
  filter(Approach == "2: Intensive") %>% 
  filter(hisco %in% c(6, 7))

# How many in each category in 1787/1801?
reg_pop %>% 
  filter(Year %in% c(1801, 1901)) %>% 
  filter(limfjord_placement_west == 1) %>% 
  pivot_longer(hisco_1st_digit6:hisco_1st_digit9) %>% 
  group_by(Year, name) %>% 
  summarise(
    n = sum(value),
    Pop = sum(Pop)
  ) %>% 
  mutate(
    pct = n/Pop
  )

# ==== What is inside '6' and '7/8/9'? ====
# Extract only parameter estimates p-values are meaningless with so many estimates

tmp = reg_pop %>% 
  select(hisco_3rd_digit611:hisco_3rd_digit649, hisco_2nd_digit71:hisco_2nd_digit99) %>%
  # Tested 3rd digit for all, and did not learn anything new:
  # select(hisco_3rd_digit611:hisco_3rd_digit649, hisco_3rd_digit712:hisco_3rd_digit999) %>%
  data.frame() %>%
  select(where(~ any(sum_special(.) != 0)))

effects_6_to_9 = foreach(j = 1:NCOL(tmp), .errorhandling = "stop", .combine = "bind_rows") %do% {
  reg_pop$occ_j = tmp[,j]
  if(var(reg_pop$occ_j, na.rm = TRUE) == 0){
    return(NA)
  }
  
  exposed_pop = reg_pop %>% 
    filter(Year == 1901) %>% 
    filter(limfjord_placement_west == 1) %>% 
    summarise(
      mean(occ_j, na.rm = TRUE)
    ) %>% unlist()
  
  average_parish_size = reg_pop %>% 
    filter(Year == 1901) %>% 
    filter(limfjord_placement_west == 1) %>% 
    summarise(
      mean(hisco_1st_digit6 + hisco_1st_digit7 + hisco_1st_digit8 + hisco_1st_digit9, na.rm = TRUE)
    ) %>% unlist()
  
  # log(x+1)
  mod_i_MA = feols(
    log(occ_j + 1) ~ Year*Affected,
    data = reg_pop %>% 
      mutate(Affected = delta_lMA_theta_1_alpha_10),
    cluster = ~ GIS_ID
  )
  
  mod_i_Dummy = feols(
    log(occ_j + 1) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
    data = reg_pop %>% 
      mutate(Affected = limfjord_placement_west),
    cluster = ~ GIS_ID
  )
  
  # Extract estimate in 1901
  MA_logx1 = plot_mod( # MA approach
    mod_i_MA, 
    the_col = "#2c5c34",
    return_data = TRUE
  ) %>% 
    filter(Year == 1901)
  
  Dummy_logx1 = plot_mod( # Dummy approach
    mod_i_Dummy, 
    the_col = "#2c5c34",
    return_data = TRUE
  ) %>% 
    filter(Year == 1901)
  
  # extensive
  mod_i_MA = feols(
    occ_j>0 ~ Year*Affected,
    data = reg_pop %>% 
      mutate(Affected = delta_lMA_theta_1_alpha_10),
    cluster = ~ GIS_ID
  )
  
  mod_i_Dummy = feols(
    occ_j>0 ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
    data = reg_pop %>% 
      mutate(Affected = limfjord_placement_west),
    cluster = ~ GIS_ID
  )
  
  # Extract estimate in 1901
  MA_ext = plot_mod( # MA approach
    mod_i_MA, 
    the_col = "#2c5c34",
    return_data = TRUE
  ) %>% 
    filter(Year == 1901)
  
  Dummy_ext = plot_mod( # Dummy approach
    mod_i_Dummy, 
    the_col = "#2c5c34",
    return_data = TRUE
  ) %>% 
    filter(Year == 1901)
  
  # intensive
  with_occ_consist = reg_pop %>% # IDs which consistently have >0
    filter(occ_j>0) %>% 
    group_by(GIS_ID) %>% 
    count() %>% 
    ungroup() %>% 
    filter(
      n == max(n) # Only those observed in all years with the above filter
    )
  
  if(NROW(with_occ_consist)<25){ # If few parishes it is likely to break
    exposed_pop_int = NA
    average_parish_size_int = NA
    MA_int = data.frame(Estimate = NA, Std..Error = NA)
    Dummy_int = data.frame(Estimate = NA, Std..Error = NA)
  } else {
    exposed_pop_int = reg_pop %>%
      filter(Year == 1901) %>% 
      filter(GIS_ID %in% with_occ_consist$GIS_ID) %>% 
      filter(limfjord_placement_west == 1) %>% 
      summarise(
        mean(occ_j, na.rm = TRUE)
      ) %>% unlist()
    
    average_parish_size_int = reg_pop %>% 
      filter(Year == 1901) %>% 
      filter(GIS_ID %in% with_occ_consist$GIS_ID) %>% 
      filter(limfjord_placement_west == 1) %>% 
      summarise(
        mean(hisco_1st_digit6 + hisco_1st_digit7 + hisco_1st_digit8 + hisco_1st_digit9, na.rm = TRUE)
      ) %>% unlist()
    
    mod_i_MA = feols(
      log(occ_j) ~ Year*Affected,
      data = reg_pop %>% 
        mutate(Affected = delta_lMA_theta_1_alpha_10) %>% 
        filter(GIS_ID %in% with_occ_consist$GIS_ID),
      cluster = ~ GIS_ID
    )
    
    mod_i_Dummy = feols(
      log(occ_j) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
      data = reg_pop %>% 
        mutate(Affected = limfjord_placement_west) %>% 
        filter(GIS_ID %in% with_occ_consist$GIS_ID),
      cluster = ~ GIS_ID
    )
    
    # Extract estimate in 1901
    MA_int = plot_mod( # MA approach
      mod_i_MA, 
      the_col = "#2c5c34",
      return_data = TRUE
    ) %>% 
      filter(Year == 1901)
    
    Dummy_int = plot_mod( # MA approach
      mod_i_Dummy, 
      the_col = "#2c5c34",
      return_data = TRUE
    ) %>% 
      filter(Year == 1901)
  }
  
  # Test if any west Limfjord
  test = reg_pop %>% 
    filter(GIS_ID %in% with_occ_consist$GIS_ID) %>% 
    filter(limfjord_placement_west == 1) %>% NROW()
  if(test==0 | NROW(Dummy_int) == 0){
    exposed_pop_int = NA
    average_parish_size_int = NA
    MA_int = data.frame(Estimate = NA, Std..Error = NA)
    Dummy_int = data.frame(Estimate = NA, Std..Error = NA)
  }
  
  # asinh()
  mod_i_MA = feols(
    asinh(occ_j) ~ Year*Affected,
    data = reg_pop %>% 
      mutate(Affected = delta_lMA_theta_1_alpha_10),
    cluster = ~ GIS_ID
  )
  
  mod_i_Dummy = feols(
    asinh(occ_j) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
    data = reg_pop %>% 
      mutate(Affected = limfjord_placement_west),
    cluster = ~ GIS_ID
  )
  
  # Saving plots
  MA_asinh = plot_mod( # MA approach
    mod_i_MA, 
    the_col = "#2c5c34",
    return_data = TRUE
  ) %>% 
    filter(Year == 1901)
  
  Dummy_asinh = plot_mod( # MA approach
    mod_i_Dummy, 
    the_col = "#2c5c34",
    return_data = TRUE
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
    hisco = gsub("t", "", substrRight(names(tmp)[j], 3)),
    n_parishes = c(rep(1589, 4), rep(NROW(with_occ_consist), 2), rep(1589, 2))
  )
}

# Descriptions
description = data.frame(
  hisco = c(611, 621:629, 631:632, 641, 649, 71:79, 80:89, 90:99) %>% as.character(),
  description = c( # Copied from https://historyofwork.iisg.nl/major.php
    # Starting with 6
    "611: General Farmers",
    
    "621: General Farm Workers",
    "622: Field Crop and Vegetable Farm Workers",
    "623: Orchard, Vineyard and Related Tree and Shrub Crop Workers",
    "624: Livestock Workers",
    "625: Dairy Farm Workers",
    "626: Poultry Farm Workers",
    "627: Nursery Workers and Gardeners",
    "628: Farm Machinery Operators",
    "629: Agricultural and Animal Husbandry Workers Not Elsewhere Classified",
    
    "631: Loggers",
    "632: Forestry Workers (except Logging)",
    
    "641: Fishermen",
    "649: Fishermen, Hunters and Related Workers Not Elsewhere Classified",
    
    # Starting with 7
    "71: Miners, Quarrymen, Well-Drillers And Related Workers",
    "72: Metal Processors",
    "73: Wood Preparation Workers And Paper Makers",
    "74: Chemical Processors And Related Workers",
    "75: Spinners, Weavers, Knitters, Dyers And Related Workers",
    "76: Tanners, Fellmongers And Pelt Dressers",
    "77: Food And Beverage Processors",
    "78: Tobacco Preparers And Tobacco Product Makers",
    "79: Tailors, Dressmakers, Sewers, Upholsterers And Related Workers",
    
    # Starting with 8
    "80: Shoemakers And Leather Goods Makers",
    "81: Cabinetmakers And Related Woodworkers",
    "82: Stone Cutters And Carvers",
    "83: Blacksmiths, Toolmakers And Machine-Tool Operators",
    "84: Machinery Fitters, Machine Assemblers And Precision-Instrument Makers (Except Electrical)",
    "85: Electrical Fitters And Related Electrical And Electronics Workers",
    "86: Broadcasting And Sound-Equipment Operators And Cinema Projectionists",
    "87: Plumbers, Welders, Sheet-Metal, And Structural Metal Preparers And Erectors",
    "88: Jewellers And Precious Metal Workers",
    "89: Glass Formers, Potters And Related Workers",
    
    # Starting with 9
    "90: Rubber And Plastics Product Makers",
    "91: Paper And Paperboard Products Makers",
    "92: Printers And Related Workers",
    "93: Painters",
    "94: Production And Related Workers Not Elsewhere Classified",
    "95: Bricklayers, Carpenters And Other Construction Workers",
    "96: Stationary Engine And Related Equipment Operators",
    "97: Material Handling And Related Equipment Operators, Dockers And Freight Handlers",
    "98: Transport Equipment Operators",
    "99: Workers Not Elsewhere Classified"
  )
)


# Table
table0 = effects_6_to_9 %>% 
  left_join(description, by = "hisco") %>% 
  mutate(
    description = ifelse(hisco == "ng", "Fishing", description)
  ) %>% 
  # remove fihsing
  filter(hisco != "ng") %>%
  mutate(
    Approach = case_when(
      Approach == "Extensive" ~ paste0("1: ", Approach),
      Approach == "Intensive" ~ paste0("2: ", Approach),
      Approach == "log(x+1)" ~ paste0("3: ", Approach),
      Approach == "asinh(x)" ~ paste0("4: ", Approach)
    )
  ) %>% 
  mutate(
    APE = Exposed_pop*Estimate
  ) %>% 
  mutate(
    APE_pct = APE/average_parish_size
  ) %>% 
  filter(
    n_parishes > 100
  ) %>%
  mutate(
    `HISCO first digit` = ifelse(
      hisco < 70, 
      "6: Agricultural, animal husbandry\n and forestry workers, fishermen\nand hunters",
      "7/8/9: Production and related\nworkers, transportequipment operators\nand labourers"
    )
  )


# Plot effects
nudge_factor = 1.5

for(aff in c("Dummy", "MA")){
  for(app in c("1: Extensive", "2: Intensive", "3: log(x+1)", "4: asinh(x)")){
    table_i = table0 %>% 
      filter(
        Affected == aff
      ) %>% 
      filter(
        Approach == app
      ) %>% 
      arrange(`HISCO first digit`, Estimate) %>% 
      mutate(
        description = cut_strings(description, 30)
      ) %>% 
      mutate(
        description = forcats::fct_inorder(description),
        hisco = forcats::fct_inorder(hisco)
      ) %>% 
      ungroup() %>% 
      drop_na(Estimate)
    
    lim_i = c(
      table_i$Estimate %>% min(na.rm = TRUE)*nudge_factor,
      table_i$Estimate %>% max(na.rm = TRUE)*nudge_factor
    )
    
    p1 = table_i %>% 
      ggplot(aes(Estimate, hisco, col = `HISCO first digit`)) + 
      geom_point(shape = 4) + 
      geom_text(
        aes(label = description),
        angle = 0,
        size = 3,
        nudge_y = 0.5
      ) +
      geom_vline(xintercept = 0) + 
      theme_bw() + 
      scale_color_manual(values = c(
        "6: Agricultural, animal husbandry\n and forestry workers, fishermen\nand hunters" = "#2c5c34", 
        "7/8/9: Production and related\nworkers, transportequipment operators\nand labourers" = "#b33d3d"
      )) + 
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5)
      ) + 
      labs(
        x = paste("Parameter estimate\n", substr(app, 4, nchar(app))),
        y = "HISCO"
      ) + 
      geom_segment(
        aes(x = 0, xend = Estimate, y = hisco, yend = hisco)
      ) +
      xlim(lim_i) +
      expand_limits(y = c(0, length(levels(table_i$hisco))+1)) +
      theme(
        legend.position = "bottom"
      ) + 
      labs(
        col = "Major category:"
      )
      
    # print(p1)
    ggsave(paste0("Plots/Mechanism/Detailed6789/", aff, "_", substr(app, 4, 6), ".png"), plot = p1, width = 2*10, height = 2*8, units = "cm")
  }
}

# ==== Fishing ====
fish = feols(
  log(Fishing + 1) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% 
    mutate(Affected = limfjord_placement_west),
  cluster = ~ GIS_ID
)
plot_mod(
  fish, "fish_dummy", dir0 = "Plots/Mechanism/", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34"
)

fish = feols(
  log(Fishing + 1) ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)
plot_mod(
  fish, "fish_MA", dir0 = "Plots/Mechanism/", ylab = "Parameter estimate", vadj = 0, the_col = "#b33d3d", corner_text = "Control group: Less Market Access improvement"
)


# ==== Spinning ====
mod1 = feols(
  log(hisco_2nd_digit75 + 1) ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% 
    mutate(Affected = limfjord_placement_west),
  cluster = ~ GIS_ID
)
plot_mod(
  mod1, "spinning_dummy", dir0 = "Plots/Mechanism/", ylab = "Parameter estimate", vadj = 0, the_col = "#2c5c34",
)

spinning = feols(
  log(hisco_2nd_digit75 + 1) ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)
plot_mod(
  spinning, "spinning_MA", dir0 = "Plots/Mechanism/", ylab = "Parameter estimate", vadj = 0, the_col = "#b33d3d", corner_text = "Control group: Less Market Access improvement"
)

# ==== Simple fertility and migration ====
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
  migr, "born_different_share_MA", 
  ylab = "Parameter estimate", 
  the_col = "#b33d3d", 
  dir0 = "Plots/Mechanism/", 
  ref_year = 1845, 
  corner_text = "Control group: Less Market Access improvement",
  pretrend_test = FALSE,
  vadj_automatic = TRUE
)

migr = feols(
  Share ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% 
    mutate(Affected = limfjord_placement_west) %>% 
    filter(as.numeric(as.character(Year)) >= 1845) %>% 
    mutate(Year = relevel(Year, ref = "1845")) %>% 
    mutate(Share = Born_different_county / Pop),
  cluster = ~ GIS_ID
)
plot_mod(
  migr, "born_different_share_Dummy", 
  ylab = "Parameter estimate", 
  the_col = "#2c5c34", 
  dir0 = "Plots/Mechanism/", 
  ref_year = 1845, 
  pretrend_test = FALSE,
  vadj_automatic = TRUE
)

# Child women ratio
fertility = feols(
  Child_women_ratio ~ Year*Affected,
  data = reg_pop %>% 
    mutate(Affected = delta_lMA_theta_1_alpha_10),
  cluster = ~ GIS_ID
)
plot_mod(
  fertility, "fertility_MA", 
  ylab = "Parameter estimate", 
  the_col = "#b33d3d", 
  dir0 = "Plots/Mechanism/", 
  corner_text = "Control group: Less Market Access improvement",
  vadj_automatic = TRUE
)

fertility = feols(
  Child_women_ratio ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
  data = reg_pop %>% 
    mutate(Affected = limfjord_placement_west),
  cluster = ~ GIS_ID
)
plot_mod(
  fertility, "fertility_Dummy", 
  ylab = "Parameter estimate", 
  the_col = "#2c5c34", 
  dir0 = "Plots/Mechanism/", 
  corner_text = "Control group: Less Market Access improvement",
  vadj_automatic = TRUE
)


# Simple plot of fertility
reg_pop %>% 
  mutate(
    Year = as.numeric(as.character(Year))
  ) %>% 
  filter(
    limfjord_placement %in% c("west", "not")
  ) %>% 
  ggplot(aes(Year, Child_women_ratio, col = limfjord_placement)) +
  # geom_point(alpha = 0.1) + 
  geom_line(data = . %>%
              group_by(Year, limfjord_placement) %>%
              summarise(mean_child_women_ratio = mean(Child_women_ratio, na.rm = TRUE)),
            aes(y = mean_child_women_ratio)) + 
  theme_bw()

# ==== Effect on age ====
tmp = reg_pop %>%
  mutate_at(vars(Age_1_4:Age_65_125), ~./Pop) %>% 
  select(Age_1_4:Age_65_125) %>%
  data.frame() %>%
  select(where(~ any(sum_special(.) != 0)))

average_parish_size = reg_pop %>% 
  filter(Year == 1901) %>% 
  filter(limfjord_placement_west == 1) %>% 
  summarise(
    mean(Pop, na.rm = TRUE)
  ) %>% unlist()

effects_demographics = foreach(j = 1:NCOL(tmp), .errorhandling = "stop", .combine = "bind_rows") %do% {
  reg_pop$dem_j = tmp[,j]
  if(var(reg_pop$dem_j, na.rm = TRUE) == 0){
    return(NA)
  }
  
  exposed_pop = reg_pop %>% 
    filter(Year == 1901) %>% 
    filter(limfjord_placement_west == 1) %>% 
    summarise(
      mean(dem_j, na.rm = TRUE)
    ) %>% unlist()
  
  name_j = names(tmp)[j]
  
  # log(x+1)
  mod_i_MA = feols(
    dem_j ~ Year*Affected,
    data = reg_pop %>% 
      mutate(Affected = delta_lMA_theta_1_alpha_10),
    cluster = ~ GIS_ID
  )
  
  mod_i_Dummy = feols(
    dem_j ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
    data = reg_pop %>% 
      mutate(Affected = limfjord_placement_west),
    cluster = ~ GIS_ID
  )

  # Extract estimate in 1901
  MA_logx1 = plot_mod( # MA approach
    mod_i_MA, 
    paste0(name_j,"_MA"),
    corner_text = "Control group: Less Market Access improvement",
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Demographics/Age_groups/",
    return_data_and_plot = TRUE,
    vadj_automatic = TRUE
  ) %>% 
    filter(Year == 1901)
  
  Dummy_logx1 = plot_mod( # Dummy approach
    mod_i_Dummy, 
    paste0(name_j,"_Dummy"),
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Demographics/Age_groups/",
    return_data_and_plot = TRUE,
    vadj_automatic = TRUE
  ) %>% 
    filter(Year == 1901)
  
  data.frame(
    Exposed_pop = exposed_pop,
    average_parish_size = average_parish_size,
    Affected = rep(c("MA", "Dummy"), 2),
    Estimate = c(
      MA_logx1$Estimate, 
      Dummy_logx1$Estimate
    ),
    Std = c(
      MA_logx1$Std..Error, 
      Dummy_logx1$Std..Error
    ),
    Pval = c(
      MA_logx1$Pr...t.., 
      Dummy_logx1$Pr...t..
    ),
    Pretrend_est = c(
      MA_logx1$Pretrend_est, 
      Dummy_logx1$Pretrend_est
    ),
    Pretrend_pval = c(
      MA_logx1$Pretrend_pval, 
      Dummy_logx1$Pretrend_pval
    ),
    age_group = names(tmp)[j],
    n_parishes = 1589
  )
}

# Bonferroni CI
effects_demographics = effects_demographics %>% 
  mutate(
    Upper = Estimate + abs(qnorm(0.025/n()))*Std,
    Lower = Estimate - abs(qnorm(0.025/n()))*Std
  )

p1 = effects_demographics %>% 
  filter(Affected == "Dummy") %>% 
  mutate(age_group = gsub("Age_", "", age_group)) %>% 
  mutate(age_group = gsub("_", " to ", age_group)) %>% 
  mutate(
    age_group = forcats::fct_inorder(age_group)
  ) %>% 
  ggplot(aes(age_group, Estimate)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = Upper, ymin = Lower)) +
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(
    x = "Age group",
    y = "Effect in 1901"
  )

p1
ggsave("Plots/Mechanism/Age_composition_Dummy.png", plot = p1, width = 10, height = 8, units = "cm")

p1 = effects_demographics %>% 
  filter(Affected == "Dummy") %>% 
  mutate(age_group = gsub("Age_", "", age_group)) %>% 
  mutate(age_group = gsub("_", " to ", age_group)) %>% 
  mutate(
    age_group = forcats::fct_inorder(age_group)
  ) %>% 
  ggplot(aes(age_group, Estimate, col = Affected)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = Upper, ymin = Lower)) +
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(
    x = "Age group",
    y = "Effect in 1901"
  ) + 
  scale_color_manual(values = c(Dummy = "#2c5c34", MA = "#b33d3d")) + 
  theme(legend.position = "none")

p1
ggsave("Plots/Mechanism/Age_composition_Dummy.png", plot = p1, width = 10, height = 8, units = "cm")

p2 = effects_demographics %>% 
  filter(Affected == "MA") %>% 
  mutate(age_group = gsub("Age_", "", age_group)) %>% 
  mutate(age_group = gsub("_", " to ", age_group)) %>% 
  mutate(
    age_group = forcats::fct_inorder(age_group)
  ) %>% 
  ggplot(aes(age_group, Estimate, col = Affected)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = Upper, ymin = Lower)) +
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(
    x = "Age group",
    y = "Effect in 1901"
  ) + 
  scale_color_manual(values = c(Dummy = "#2c5c34", MA = "#b33d3d")) + 
  theme(legend.position = "none")

p2
ggsave("Plots/Mechanism/Age_composition_MA.png", plot = p2, width = 10, height = 8, units = "cm")

# ==== Effect on migration ====
tmp = reg_pop %>%
  mutate(
    Age_1_4_migr = Age_1_4_migr/Age_1_4,
    Age_5_14_migr = Age_5_14_migr/Age_5_14,
    Age_15_24_migr = Age_15_24_migr/Age_15_24,
    Age_25_34_migr = Age_25_34_migr/Age_25_34,
    Age_35_44_migr = Age_35_44_migr/Age_35_44,
    Age_45_54_migr = Age_45_54_migr/Age_45_54,
    Age_55_64_migr = Age_55_64_migr/Age_55_64,
    Age_65_125_migr = Age_65_125_migr/Age_65_125
  ) %>% 
  select(Age_1_4_migr:Age_65_125_migr) %>% 
  # select(starts_with("Age_")) %>%
  # select(contains("_migr")) %>% 
  # select(-contains("mean")) %>% 
  # select(-contains("Age_0_1")) %>% 
  data.frame() %>%
  select(where(~ any(sum_special(.) != 0)))

effects_demographics = foreach(j = 1:NCOL(tmp), .errorhandling = "stop", .combine = "bind_rows") %do% {
  reg_pop$dem_j = tmp[,j]
  if(var(reg_pop$dem_j, na.rm = TRUE) == 0){
    return(NA)
  }
  
  # Save original data to restore later
  reg_pop0 = reg_pop
  
  # Filter data
  reg_pop = reg_pop %>% 
    mutate(
      Year = relevel(Year, "1845")
    ) %>% 
    filter(
      as.numeric(as.character(Year)) > 1840
    )
  
  exposed_pop = reg_pop %>% 
    filter(Year == 1901) %>% 
    filter(limfjord_placement_west == 1) %>% 
    summarise(
      mean(dem_j, na.rm = TRUE)
    ) %>% unlist()
  
  name_j = names(tmp)[j]
  
  # log(x+1)
  mod_i_MA = feols(
    dem_j ~ Year*Affected,
    data = reg_pop %>% 
      mutate(Affected = delta_lMA_theta_1_alpha_10),
    cluster = ~ GIS_ID
  )
  
  mod_i_Dummy = feols(
    dem_j ~ Year*Affected + Year*limfjord_placement_middle + Year*limfjord_placement_east,
    data = reg_pop %>% 
      mutate(Affected = limfjord_placement_west),
    cluster = ~ GIS_ID
  )
  
  # Extract estimate in 1901
  MA_logx1 = plot_mod( # MA approach
    mod_i_MA, 
    paste0(name_j,"_MA"),
    corner_text = "Control group: Less Market Access improvement",
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Demographics/Migration/",
    return_data_and_plot = TRUE,
    ref_year = 1845,
    pretrend_test = FALSE,
    vadj_automatic = TRUE
  ) %>% 
    filter(Year == 1901)
  
  Dummy_logx1 = plot_mod( # Dummy approach
    mod_i_Dummy, 
    paste0(name_j,"_Dummy"),
    the_col = "#2c5c34",
    dir0 = "Plots/Mechanism/Demographics/Migration/",
    return_data_and_plot = TRUE,
    ref_year = 1845,
    pretrend_test = FALSE,
    vadj_automatic = TRUE
  ) %>% 
    filter(Year == 1901)
  
  
  # Restore original data
  reg_pop = reg_pop0
  
  # Output
  data.frame(
    Exposed_pop = exposed_pop,
    average_parish_size = average_parish_size,
    Affected = rep(c("MA", "Dummy"), 2),
    Estimate = c(
      MA_logx1$Estimate, 
      Dummy_logx1$Estimate
    ),
    Std = c(
      MA_logx1$Std..Error, 
      Dummy_logx1$Std..Error
    ),
    Pval = c(
      MA_logx1$Pr...t.., 
      Dummy_logx1$Pr...t..
    ),
    age_group = names(tmp)[j],
    n_parishes = 1589
  )
}

# Bonferroni CI
effects_demographics = effects_demographics %>% 
  mutate(
    Upper = Estimate + abs(qnorm(0.025))*Std,
    Lower = Estimate - abs(qnorm(0.025))*Std
  )

effects_demographics %>% 
  mutate(age_group = gsub("Age_", "", age_group)) %>% 
  mutate(
    gender = substrRight(age_group, 1)
  ) %>% 
  mutate(
    age_group = gsub("_f", "", age_group)
  ) %>% 
  mutate(
    age_group = gsub("_m", "", age_group)
  ) %>% 
  mutate(
    gender = ifelse(gender == "r", "All", gender)
  ) %>% 
  mutate(
    age_group = gsub("igr", "", age_group)
  ) %>% 
  mutate(
    age_group = gsub("_", " to ", age_group)
  ) %>% 
  mutate(
    age_group = forcats::fct_inorder(age_group)
  ) %>% 
  ggplot(aes(age_group, Estimate)) + 
  geom_point() + 
  geom_errorbar(aes(ymax = Upper, ymin = Lower)) +
  geom_hline(yintercept = 0) + 
  facet_wrap(gender~Affected, ncol = 2, scales = "free_y")  + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

