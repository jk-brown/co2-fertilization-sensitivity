# helper functions

#' This function normalizes global mean surface temperature anomaly (`gmst`)
#' values to a specified reference period. Normalization is performed separately
#' for each `run_number` by subtracting the mean GMST value during the reference
#' period from all GMST values in that run. All non-GMST variables are returned
#' unchanged.
#'
#' @param data A data frame containing model output. Must include the columns
#'   `run_number`, `variable`, `year`, and `value`.
#' @param ref_start Numeric. First year of the reference period. Defaults to 1850.
#' @param ref_end Numeric. Last year of the reference period. Defaults to 1900.
#'
#' @return A data frame with GMST values normalized to the reference period.
#'   Non-GMST variables are unchanged.
#' @export
#'
#' @examples
#' normalized_data <- normalize_gmst(model_output)
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
    rename(value = metric_result)
  
  return(return_df)
}