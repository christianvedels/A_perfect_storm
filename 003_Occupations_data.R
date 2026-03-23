# Adding occupations
#
# Date updated:   2024-06-15
# Auhtor:         Christian Vedel 
# Purpose:        Join on data on HISCO codes and categories
#
# Output:         'merged_data' enriched with HISCO codes + occupational categories
#
# HISCO codes generated from automatic HISCO classifier. 
# See https://arxiv.org/abs/2402.13604 

# ==== Options ====
if(!exists("OVERWRITE")) OVERWRITE = 2  # 0: never overwrite; 1: overwrite if files are older than 7 days; 2: always overwrite

# ==== Libraries ====
library(tidyverse)
library(foreach)
library(fst)
library(dataverse)

# ==== Set dataverse env ====
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")

fpath_out     = "Data/tmp_census.fst"
fpath_sentinel = "Data/RowID_GIS_ID_key.csv"  # Unique to 003 — use as freshness sentinel
file_fresh = file.exists(fpath_sentinel) &&
  difftime(Sys.time(), file.mtime(fpath_sentinel), units = "days") < 7 &&
  file.exists(fpath_out) && file.mtime(fpath_out) >= file.mtime(fpath_sentinel)

if(OVERWRITE == 0 || (OVERWRITE == 1 && file_fresh)){
  cat("Skipping 003 (output file up to date or OVERWRITE = 0)\n")
  merged_data = read_fst(fpath_out)
} else {

# ==== Load data ====
merged_data = read_fst("Data/tmp_census.fst") 
hisco_local = "Data/Census_HISCO_codes_clean.csv"
if(file.exists(hisco_local)){
  cat("Loading HISCO data from local file\n")
  hisco = read_csv(hisco_local)
} else {
  cat("Downloading HISCO data from Dataverse\n")
  hisco = get_dataframe_by_name(
    filename = "Census_HISCO_codes_clean.csv",
    dataset = "10.7910/DVN/WZILNI", # DOI
    server = "dataverse.harvard.edu",
    .f = function(x) read_csv(x)
  )
  if(NROW(hisco) == 0 || NCOL(hisco) == 0){
    stop("Dataverse download of Census_HISCO_codes_clean.csv returned an empty file. Check your internet connection and try again.")
  }
  write_csv(hisco, hisco_local)
  cat("Saved to", hisco_local, "\n")
}

# Extract GIS_ID/RowID key for other projects
tmp = merged_data %>% 
  distinct(GIS_ID, RowID) %>% 
  drop_na(GIS_ID)

tmp %>% 
  write_csv("Data/RowID_GIS_ID_key.csv")

# Delete occ cols if any, to be able to rerun this multiple times:
merged_data = merged_data %>% 
  select(pa_id:Born_different_county)

# ==== Data cleaning ====
# 'Year' from 'Kilde'
x = hisco$Kilde %>% unique()

hisco = hisco %>% 
  mutate(Year = gsub("[^0-9]", "", Kilde))

hisco = hisco %>% 
  select(pa_id, Year, Erhverv, Stilling_i_husstanden, hisco_1:desc_5) %>% 
  rename(
    Occupation = Erhverv,
    Household_position = Stilling_i_husstanden,
  ) %>% 
  mutate(
    Year = as.character(Year),
    pa_id = as.character(pa_id)
  )

# Check uniqueness of ids
cat("Unique pa_id in HISCO data:", hisco$pa_id %>% unique() %>% length(), "\n")
hisco %>% group_by(Year, pa_id) %>% count() %>% filter(n>1) %>% print()

# Check data quality in 1000 random subsamples
# set.seed(20)
# hisco %>% 
#   sample_n(1000) %>% 
#   write_csv2("Data/HISCO_to_check.csv")

# Check occupational observations
cat("Total observations in HISCO data:", nrow(hisco), "\n")
hisco %>% 
  group_by(Year) %>% 
  summarise(
    NA_Occupation = sum(is.na(Occupation)),
    NA_Household_position = sum(is.na(Household_position)),
    NA_both = sum(is.na(Occupation) & is.na(Household_position))
  ) %>% print()

# ==== 0-9 first digit HISCO ====
fix_hisco = function(x){
  x = as.character(x)
  x = ifelse(nchar(x)==4, paste0("0", x), x)
  return(x)
}

# Pre-compute the HISCO codes that appear globally (to get consistent columns across batches)
hisco_cols = hisco %>% select(starts_with("hisco_")) %>% mutate_all(fix_hisco)
appeared_2digit = sort(unique(unlist(apply(hisco_cols, 1, function(x) unique(substr(x, 1, 2))))))
appeared_3digit = sort(unique(unlist(apply(hisco_cols, 1, function(x) unique(substr(x, 1, 3))))))
appeared_2digit = appeared_2digit[!is.na(appeared_2digit) & appeared_2digit != "NA"]
appeared_3digit = appeared_3digit[!is.na(appeared_3digit) & appeared_3digit != "NA"]
rm(hisco_cols)

# ==== Process in yearly batches ====
batch_dir = "Data/tmp_occ_batches"
if(!dir.exists(batch_dir)) dir.create(batch_dir)

process_year = function(yr){
  fpath_batch = file.path(batch_dir, paste0(yr, ".fst"))
  batch_fresh = file.exists(fpath_batch) &&
    difftime(Sys.time(), file.mtime(fpath_batch), units = "days") < 7
  if(OVERWRITE == 0 || (OVERWRITE == 1 && batch_fresh)){
    cat("  Year", yr, "- loading cached batch\n")
    return(read_fst(fpath_batch))
  }
  cat("  Year", yr, "- processing\n")

  md_yr = merged_data %>%
    filter(Year == yr) %>%
    left_join(hisco %>% filter(Year == as.character(yr)), by = c("Year", "pa_id")) %>%
    mutate_at(vars(starts_with("hisco_") & !starts_with("hisco_1st") &
                     !starts_with("hisco_2nd") & !starts_with("hisco_3rd")), fix_hisco)

  hisco_raw_cols = names(md_yr)[!grepl("^en_hisco_text", names(md_yr)) & grepl("^hisco_[0-9]", names(md_yr))]
  hisco_mat = as.matrix(md_yr[, hisco_raw_cols])

  mat1 = matrix(substr(hisco_mat, 1, 1), nrow = nrow(hisco_mat))
  mat2 = matrix(substr(hisco_mat, 1, 2), nrow = nrow(hisco_mat))
  mat3 = matrix(substr(hisco_mat, 1, 3), nrow = nrow(hisco_mat))
  rm(hisco_mat); gc()

  # First digit — free mat1 immediately after
  for(d in 0:9){
    md_yr[[paste0("hisco_1st_digit", d)]] = as.integer(rowSums(mat1 == as.character(d), na.rm = TRUE) > 0)
  }
  rm(mat1); gc()

  # Second digit — free mat2 immediately after
  for(code in appeared_2digit){
    md_yr[[paste0("hisco_2nd_digit", code)]] = as.integer(rowSums(mat2 == code, na.rm = TRUE) > 0)
  }
  rm(mat2); gc()

  # Third digit — column-wise OR avoids allocating a full mat3 == code matrix per iteration
  col_match = function(mat, code){
    result = mat[, 1] == code & !is.na(mat[, 1])
    for(j in seq_len(ncol(mat))[-1]) result = result | (mat[, j] == code & !is.na(mat[, j]))
    as.integer(result)
  }
  for(code in appeared_3digit){
    md_yr[[paste0("hisco_3rd_digit", code)]] = col_match(mat3, code)
  }
  rm(mat3); gc()
  write_fst(md_yr, fpath_batch, compress = 0)
  return(md_yr)
}

years = sort(unique(merged_data$Year))
merged_data = lapply(years, process_year) %>% bind_rows()

# ==== Saving data enriched data ====
write_fst(merged_data, fpath_out, compress = 0)

} # end OVERWRITE check


