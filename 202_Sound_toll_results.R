# Sound toll results
# Date updated:   2023-04-22
# Auhtor:         Christian Vedel 
#
# Purpose:        Plot of Limfjord trafic + regressions

# ==== Libraries ====
library(tidyverse)
library(fixest)

# ==== Load data ====
data0 = read_csv2("Data/LocalSoundToll.csv")
data_channel = read_csv2("Data/1977 Svalgard.csv")

# ==== Default colors ====
# Regions:
regions_col = c(
  reference = "black",
  east = "#273a8f",
  middle = "#2c5c34",
  west = "#b33d3d"
)

# gradients 
gradients_col = c(
  low = "#273a8f",
  high = "#DE7500"
)

# ==== Clean data ====
data0 = data0 %>% 
  filter(port != "Limfjorden") %>% 
  rename(
    N_in = n_to,
    N_out = n_from
  ) %>% 
  group_by(port) %>% 
  mutate(
    max_trafic = max(trafic)
  ) %>% 
  filter(max_trafic > 1) %>% # Filter off micelanous ports (max 1 ship in a year)
  ungroup() %>% 
  filter(Year >= 1750 & Year <= 1855) %>% 
  mutate(After = as.numeric(Year >= 1834)) %>% 
  mutate(
    limfjord_placement = ifelse(
      limfjord_placement == "no", 
      "reference", 
      limfjord_placement
    )
  ) %>% 
  mutate(
    limfjord_placement = relevel(factor(limfjord_placement), ref = "reference")
  ) %>% 
  rename(
    latitude = decLatitude,
    longitude = decLongitude
  )
  

# ==== Plot ====
p1 = data0 %>% 
  group_by(Year, limfjord_placement) %>% 
  summarise(
    traffic = sum(trafic) # Sum and correct spelling mistake
  ) %>% 
  ggplot(aes(Year, log(traffic+1), col = limfjord_placement, shape = limfjord_placement)) + 
  geom_rect(
    fill = "lightgrey", 
    alpha = 0.75, 
    xmin = 1807, 
    xmax = 1814,
    ymin = 0,
    ymax = Inf,
    col = NA
  ) + 
  # geom_line() +
  geom_vline(xintercept = c(1825, 1834), lty = 2) + 
  # geom_smooth() + 
  theme_bw() + 
  annotate(
    geom = "text", x = 1810.5, y = 4.5, label = "Napoleonic wars", angle = 90
  ) + 
  annotate(
    geom = "text", x = 1826.5, y = 6, label = "Breach", angle = 90
  ) + 
  annotate(
    geom = "text", x = 1837.5, y = 6, label = "Agger channel\nfully navigable", angle = 90
  ) +
  scale_x_continuous(breaks = seq(1750, 1860, by = 5)) + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  ) + 
  geom_point() + 
  labs(
    col = "Location in Limfjord:",
    shape = "Location in Limfjord:"
  ) + 
  theme(
    legend.position = "bottom"
  ) +  
  scale_color_manual(
    values = regions_col
  ) + 
  scale_shape_manual(
    values = c(
      reference = 4,
      east = 15,
      middle = 16,
      west = 17
    )
  )

p1
ggsave("Plots/Ship_trafic.png", plot = p1, width = 8, height = 5)

# Average after:
data0 %>% 
  filter(limfjord_placement %in% c("west", "middle")) %>% 
  group_by(Year, After) %>% 
  summarise(trafic = sum(trafic)) %>% 
  group_by(After) %>% 
  summarise(mean(trafic))

# Average for other ports:
data0 %>% 
  filter(!limfjord_placement %in% c("west", "middle")) %>% 
  group_by(Year, After) %>% 
  summarise(trafic = 6*mean(trafic)) %>% 
  group_by(After) %>% 
  summarise(mean(trafic))

# ==== Svalgaard data ====
p2 = data_channel %>% 
  ggplot(aes(Year, Ships_total, shape = Channel)) + 
  geom_point(col = regions_col["west"]) + 
  theme_bw() + 
  labs(
    y = "Ships passing the channel\nlog scale"
  ) + 
  theme(
    legend.position = "bottom"
  ) + 
  scale_shape_manual(
    values = c(
      `Thyborøn channel` = 15,
      `Agger channel` = 17
    )
  ) +
  scale_x_continuous(breaks = seq(1830, 1875, by = 5)) + 
  scale_y_log10()

p2
ggsave("Plots/Ship_trafic_channel.png", plot = p2, width = 8, height = 4)

# ==== Regressions for appendix ====
mod0 = fepois(
  trafic ~ After*limfjord_placement,
  data = data0,
  cluster = ~port
)

mod1 = fepois(
  trafic ~ After*limfjord_placement,
  data = data0 %>% filter(!Year %in% c(1807:1814, 1825:1833)),
  cluster = ~port
)

mod2 = feols(
  log(trafic+1) ~ After*limfjord_placement, 
  data = data0,
  cluster = ~port
)

mod3 = feols(
  asinh(trafic) ~ After*limfjord_placement, 
  data = data0,
  cluster = ~port
)

mod4 = feols(
  trafic ~ After*limfjord_placement, 
  data = data0 %>% mutate(trafic = trafic>0),
  cluster = ~port
)

mods = list(mod0, mod1, mod2, mod3, mod4)

etable(mods, tex = TRUE)
