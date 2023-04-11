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

