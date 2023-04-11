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