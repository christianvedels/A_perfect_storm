# Functions
#
# Date updated:   2023-04-11
# Auhtor:         Christian Vedel 
# Purpose:        Functions used in the rest of the project

# ==== substrRight =====
# substr but from right
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# ==== sub_scandi ====
# this substitutes scandinavian letters
sub_scandi = function(x){
  scandi_letters = c("Æ",
                     "æ",
                     "Ø",
                     "ø",
                     "Å",
                     "å")
  
  replacement = c("Ae",
                  "ae",
                  "Oe",
                  "oe",
                  "Aa",
                  "aa")
  
  for(i in 1:length(scandi_letters)){
    x = gsub(
      scandi_letters[i],
      replacement[i],
      x
    )
  }
  
  return(x)
  
}

# ==== sub_scandi_mis ====
# This subsitutes wrongly read 
sub_scandi_mis = function(x){
  scandi_letters = c(
    "Ã¸",
    "Ã¥",
    "Ã¦",
    "Ã˜",
    "Ã…",
    "Ã†"
  )
  
  replacement = c(
    "ø",
    "å",
    "æ",
    "Ø",
    "Å",
    "Æ"
  )
  
  for(i in 1:length(scandi_letters)){
    x = gsub(
      scandi_letters[i],
      replacement[i],
      x
    )
  }
  
  return(x)
}



# ==== Clip_it ====
# Clip a shape file by a bounding box
# shp:  Shape 
# bb:   Bounding box (e.g. matrix(c(8, 54, 16, 58), nrow=2))

Clip_it = function(shp, bb){
  require(rgeos)
  if(class(bb)[1] == "matrix") b_poly <- as(raster::extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(raster::extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}


# ==== degrees_to_km() ====
# Converts lat long grid til km grid
# Based on circle approximation of earth
#
# Args:
# lat:  Decimal degrees lattitude
# long: Decimal degrees longitude
# 

degrees_to_km = function(lat, long) {
  mean_earth_radius =  6371.0088
  to_radians = 1 / 360 * 2 * pi
  
  # Lat
  earth_circumference = mean_earth_radius * 2 * pi
  lat_km = earth_circumference * lat / 360
  
  # Long
  radius_at_lat = mean_earth_radius * cos(lat * to_radians)
  circumference_at_lat = 2 * pi * radius_at_lat
  long_km = long / 360 * circumference_at_lat
  
  return(c(lat_km, long_km))
  
}

# ==== construct_panel() ====
# This takes archaeological samples from the data from 009_Archaeological_monte_carlo.R
# and turns them into a panel
construct_panel = function(arch_samples){
  suppressMessages({
    arch_samples %>%
      # Add distinct ID for repeated GIS_IDs
      group_by(GIS_ID, rYear) %>% 
      summarise(
        activity = mean(activity),
        n = mean(n)
      )
  })
}

# ==== arch_sampler() ====
arch_sampler = function(arch_samples, capB = 1000){
  require(foreach)
  
  # Check if geo data is loaded. Otherwise load it
  if(!"geo_data" %in% ls()){
    geo_data = read_csv2("Data/Geo.csv", guess_max = 2000)
  }
  
  Uniques_GIS_IDs = geo_data$GIS_ID
  max_b = arch_samples$b %>% max()
  
  start_t = Sys.time()
  
  panels = foreach(b = 1:capB) %do% {
    # Sample parishes
    GIS_ID_b = sample(Uniques_GIS_IDs, size = length(Uniques_GIS_IDs), replace = TRUE)
    b_b = sample(seq(1, max_b), size = max_b, replace = TRUE)
    
    # Construct samples file from sampled parishes
    res = expand.grid(
      GIS_ID = GIS_ID_b,
      b = b_b
    ) %>% 
      distinct(GIS_ID, b) %>% 
      left_join(arch_samples, by = c("GIS_ID", "b")) %>%
      # Turn this into a panel like the main reg panel
      construct_panel()
    
    
    # Report status to console
    time_t = Sys.time()
    delta_t = time_t - start_t
    per_step = delta_t/b
    total_t = capB * per_step
    remaining_t = total_t - delta_t
    
    message0 = paste0(
      as.character(time_t),
      ": b = ", b,
      " ellapsed time: ", round(delta_t, 3), " ", units(delta_t),
      " remaining: ", round(remaining_t, 3), " ", units(remaining_t),
      " of ", round(total_t, 3), " ", units(total_t),
      "                \r"
    )
    
    cat(message0)
    
    return(res)
  }
  
  return(panels)
}

# ==== vcov_funciton_boot ====
# Custom vcov from samples from arch sampler
vcov_function_boot = function(formula, samples, capB = 100, affected = "delta_lMA_theta_1_alpha_10", Uniques_GIS_IDs = NA){
  
  start_t = Sys.time()
  
  beta_samples = foreach(i = seq(1, capB), .combine = "rbind") %do% {
    data_i = samples[[i]]
    data_i$Affected = data_i[,affected] %>% unlist()
    
    mod1 = feols(
      formula,
      data = data_i,
      vcov = "iid"
    )
    # cat(i,"         \r")
    
    res_i = data.frame(mod1$coefficients) %>% t()
    
    if(i%%10 == 0){
      # Report status to console
      time_t = Sys.time()
      delta_t = time_t - start_t
      per_step = delta_t/i
      total_t = capB * per_step
      remaining_t = total_t - delta_t
      
      message0 = paste0(
        as.character(time_t),
        ": i = ", i,
        " ellapsed time: ", round(delta_t, 3), " ", units(delta_t),
        " remaining: ", round(remaining_t, 3), " ", units(remaining_t),
        " of ", round(total_t, 3), " ", units(total_t),
        "                \r"
        
      )
      
      cat(message0)
    }
  
    return(res_i)
  }
  return(
    list(
      vcov = cov(beta_samples),
      beta_samples = beta_samples
    )
  )
  
}


# ==== plot_mod ====
# Function to plot event studies and save it

plot_mod = function(
    mod, 
    fname, 
    ref_year = 1801, 
    ylab = "",
    vadj = 0.15,
    the_col = "#b33d3d",
    corner_text = "Control: Non-Limfjord parishes",
    return_data = FALSE,
    dir0 = "Plots/Regression_plots/",
    return_data_and_plot = FALSE,
    pretrend_test = TRUE,
    vadj_automatic = FALSE
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
  
  if(vadj_automatic){
    vadj = mean(data0$Estimate)
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
  
  fname0 = paste0(dir0, fname, ".png")
  ggsave(fname0,  plot = p1, width = 10, height = 8, units = "cm")
  
  # Testing potential pretrends
  if(pretrend_test){
    pre_trend = data0 %>% 
      filter(Year == 1787) %>% 
      transmute(
        Pretrend_est = Estimate,
        Pretrend_std = Std..Error,
        Pretrend_pval = Pr...t..
      )
    
    data0 = bind_cols(data0, pre_trend)
  }
  
  if(return_data_and_plot){
    return(data0)
  }
  
  return(p1)
}

# ==== Round0 ====
Round0 = function(x, digits = 3){
  if(is.numeric(x)){
    return(round(x, digits = digits))
  } else {
    return(x)
  }
  
  stop("Unexpected error")
}

# ==== sum0 ====
# Summary which defaults to na.rm = TRUE
sum0 = function(x){
  sum(x, na.rm=TRUE)
}

# ==== sum_special ====
# Summary which return 100 if a character vector is inputted
sum_special = function(x){
  if(is.character(x)){
    return(x)
  } else {
    return(sum(x, na.rm=TRUE))
  }
}


# ==== cut_string ====
# Cuts strings at the closest word to a limit
cut_strings = function(x, limit = 20){
  strsplit(x, " ") %>% 
    lapply(function(y){
      nchar_y = nchar(y)
      cumsum_nchar = cumsum(nchar_y + 1) # + 1 to include spaces
      
      # NA handling
      if(all(is.na(y))){
        return(" ")
      }
      
      # Return unaltered string if it already is within limit
      if(max(cumsum_nchar) <= (limit+1)){ # + 1 to allow exact limit
        res = paste(y, collapse = " ")
        return(res)
      }
      
      # Return those below limit
      res = y[which(cumsum_nchar <= limit)]
      res = paste(res, collapse = " ")
      res = paste0(res, "...")
      
      return(res)
    }) %>% 
    unlist()
}
