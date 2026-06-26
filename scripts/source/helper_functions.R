# helper functions

#' Normalize GMST anomaly to a reference period
#'
#' This function normalizes time-series global mean surface temperature anomaly
#' data.
#'
#' @param data a data frame object with GMST required as a variable. 
#' @param ref_start start date for reference year range. Defaults to 1850.
#' @param ref_end end date for the reference year range. Defaults to 1900.
#'
#' @returns returns a df with with GMST values normalized.
#' @export
#'
#' @examples
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


#' Calculate summary metrics for specified variables from model result
#'
#' This function uses a model result and computes summary metrics for provided 
#' variable across provided years.
#'
#' @param data A model result data frame.
#' @param var Variables to calculate metrics for. Variables must be present in the 
#' model result.
#' @param years Year range to compute summary metrics for. Defaults to late-century
#' year range; 2081:2100.
#' @param FUN A mathematical operation function for the metric to compute, e.g.,
#' mean, median, max, min, etc.
#'
#' @returns returns a a summary table data frame for each of the variable across 
#' all model ensemble members.
#' @export
#'
produce_metrics <- function(data, var, years = 2081:2100, FUN = mean) {
  
  metric <- new_metric(var = var, years = years, op = FUN)
  
  metric_results <- metric_calc(data, metric = metric)
  
  return_df <- metric_results %>%
    mutate(variable = var) %>% 
    rename(value = metric_result) %>% 
    left_join(beta_vals_rn, by = "run_number")
  
  return(return_df)
}