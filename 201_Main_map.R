# Main map
# Date updated:   2024-06-16
# Auhtor:         Christian Vedel 
# Purpose:        Construct the map showing the event    
#
# Output:         'Map.png' and 'Map_nobg.png'

# ==== Libraries ====
library(tidyverse)
library(ggspatial)

# ==== Load data ====
shape = rgdal::readOGR("Data/sogne_shape")
geo_data = read_csv2("Data/Geo.csv", guess_max = 2000)
parishMA = read_csv2("Data/MA_estimates.csv", guess_max = 2000)
market_towns = read.csv2("Data/Market_towns.csv", fileEncoding="latin1")

# ==== Edit shape file ====
shape = subset(shape, AMT != "Bornholm")
shape = subset(shape, AMT != "Haderslev")
shape = subset(shape, AMT != "Toender")
shape = subset(shape, AMT != "Aabenraa")
shape = subset(shape, AMT != "Soenderborg")

# ==== West and middle Limfjord market towns ====
west_middle_mts = market_towns %>% 
  rowwise() %>% 
  filter(Limfjord_coast %in% c("west", "middle"))

west_middle_mts = west_middle_mts %>% left_join(geo_data, by = "GIS_ID")

# ==== Add data to shape file ====
parishMA = parishMA %>% 
  filter(theta == -1, alpha == 10) %>% 
  mutate(dif_lMA = log(MA_after_before))

shape@data = shape@data %>% 
  left_join(
    geo_data %>% 
      select(GIS_ID, limfjord_placement) %>% 
      mutate(limfjord_placement = ifelse(limfjord_placement=="not","reference",limfjord_placement)),
    by = "GIS_ID"
  ) %>% 
  left_join(
    parishMA,
    by = "GIS_ID"
  )

# ==== Map with everything ====
# Plots creation pipeline
p1 = ggplot() + 
  layer_spatial(
    data = shape, aes(
      fill = dif_lMA,
      col = limfjord_placement
    )
  ) + 
  theme_void() +
  scale_color_manual(
    values = c(
      reference = "black",
      east = "#273a8f",
      middle = "#2c5c34",
      west = "#b33d3d"
    )
  ) + 
  scale_fill_gradient(
    low = "#273a8f",
    high = "#DE7500"
  ) + 
  geom_point(
    aes(x = 8.2, y = 56.71, shape = "Agger channel"), 
    size = 5
  ) +
  geom_curve( # Period 1200 to 1834
    data = data.frame(
      y = 56.550615, x = 8.305105,
      yend = 56.970915, xend = 9.207246
    ),
    aes(
      x = x, y = y, xend = xend, yend = yend,
      lty = "Period 1200 to 1834"
    ),
    curvature = 0.1,
    arrow = arrow(angle = 20)
  ) +
  geom_curve( # Period 1200 to 1834
    data = data.frame(
      y = 56.970915, x = 9.207246,
      yend = 56.947637, xend = 10.488074
    ),
    aes(
      x = x, y = y, xend = xend, yend = yend,
      lty = "Period 1200 to 1834"
    ),
    curvature = -0.2,
    arrow = arrow(angle = 20)
  ) +
  geom_curve( # Period 1200 to 1834
    data = data.frame(
      y = 56.947637, x = 10.488074,
      yend = 57.800385, xend = 10.808764
    ),
    aes(
      x = x, y = y, xend = xend, yend = yend,
      lty = "Period 1200 to 1834"
    ),
    curvature = 0.3,
    arrow = arrow(angle = 20)
  ) +
  geom_curve( # Period 1200 to 1834
    data = data.frame(
      y = 57.800385, x = 10.808764,
      yend = 57.641009, xend = 8.883548
    ),
    aes(
      x = x, y = y, xend = xend, yend = yend,
      lty = "Period 1200 to 1834"
    ),
    curvature = 0.3,
    arrow = arrow(angle = 20)
  ) +
  geom_curve( # Period 1200 to 1834
    data = data.frame(
      y = 57.641009, x = 8.883548,
      yend = 57.2, xend = 7.4
    ),
    aes(
      x = x, y = y, xend = xend, yend = yend,
      lty = "Period 1200 to 1834"
    ),
    curvature = -0.2,
    arrow = arrow(angle = 20)
  ) +
  geom_curve( # After 1834
    data = data.frame(
      y = 56.550615, x = 8.305105,
      yend = 56.9, xend = 7.4
    ),
    aes(
      x = x, y = y, xend = xend, yend = yend,
      lty = "After 1834"
    ),
    curvature = 0.4,
    arrow = arrow(angle = 20)
  ) +
  geom_point(
    aes(long, lat, shape = "New natural harbours"), 
    size = 2,
    data = west_middle_mts
  ) + 
  scale_shape_manual(values = c(
    "Agger channel" = 1,
    "New natural harbours" = 16
  )) + 
  labs(
    fill = expression(Delta~"log(MA)"),
    col = "Limfjord regions",
    shape = "",
    lty = ""
  )



p2 = p1 +
  theme(
    plot.background = element_rect(fill = "white", size = 0)
  )


p1
p2
ggsave(
  "Plots/Map_nobg.png", plot = p1, width = 18, height = 15,
  units = "cm"
)
ggsave(
  "Plots/Map.png", plot = p2, width = 18, height = 15,
  units = "cm"
)
