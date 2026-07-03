library(tidyverse)
library(matilda)

normalize_gmst <- function(data,
                           ref_start = 1850,
                           ref_end = 1900) {
  
split_by_run_number <- split(data, data$run_number)

normalized_list <- lapply(split_by_run_number, function(df) {

  gmst_subset <- subset(df, variable == "gmst")
  var_subset <- subset(df, variable != "gmst")  

  ref_period <- subset(
    gmst_subset,
    year >= ref_start & 
      year <= ref_end
  )  

mean_ref_dat <- mean(ref_period$value, na.rm = TRUE)  

gmst_subset$value <- gmst_subset$value - mean_ref_dat  

