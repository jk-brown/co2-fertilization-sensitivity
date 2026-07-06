# Normalizing Hector-Matilda data

library(matilda)
library(tidyverse)

mod_result <- mod1

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
    
    df_normalized <- rbind(var_subset, gmst_subset) 
    
    return(df_normalized)
  })
  
  mod_result_norm <- do.call(rbind, normalized_list)
  
  rownames(mod_result_norm) <- NULL
  
  return(mod_result_norm)
  
}

# Normalize gmst values in mod_result

mod_result_norm <- normalize_gmst(mod_result, 
                                  ref_start = 1850, 
                                  ref_end = 1900)

# Define metric - e.g., late-century warming 

lc_warming <- new_metric(var = "gmst", years = 2081:2100, op = mean)

# Computing metrics

lc_warming_results <- metric_calc(x = mod_result_norm, 
                                  metric = lc_warming)

head(lc_warming_results)
