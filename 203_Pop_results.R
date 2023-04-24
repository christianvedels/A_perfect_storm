# Pop results
# Date updated:   2023-04-22
# Auhtor:         Christian Vedel 
#
# Purpose:        Construct the map showing the event        
# Output:     

# ==== Libraries ====
library(tidyverse)

# ==== Read data ====
reg_pop = read_csv2("Data/Popdata.csv")

# ==== Functions ====
plot_mod = function(
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
    Rem_rnames() %>%
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
    ) %>%
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
  
  fname0 = paste0("Plots/Short_paper/", fname, ".png")
  fname1 = paste0("Plots/Short_paper/Wide_", fname, ".png")
  ggsave(fname0,  plot = p1, width = 10, height = 8, units = "cm")
  ggsave(fname1,  plot = p1, width = 10, height = 6, units = "cm")
  
  return(p1)
}
