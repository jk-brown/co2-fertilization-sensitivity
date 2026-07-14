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



# beta_effect_data
#' Title
#'
#' @param data 
#' @param beta_low 
#' @param beta_high 
#'
#' @returns
#' @export
#'
#' @examples
beta_effect_data <- function(data, beta_low = 0.20, beta_high = 0.80) {
  
  # define low and high beta values
  beta_low_cutoff <- quantile(data$BETA, beta_low, na.rm = TRUE)
  beta_high_cutoff <- quantile(data$BETA, beta_high, na.rm = TRUE)
  
  # Add BETA group labels
  data_beta <- data %>%
    mutate(
      beta_group = case_when(
        BETA <= beta_low_cutoff ~ "Low BETA",
        BETA >= beta_high_cutoff ~ "High BETA",
        TRUE ~ "Middle BETA"
      ),
      beta_group = factor(
        beta_group,
        levels = c("Low BETA", "Middle BETA", "High BETA")
      )
    )
  
  # summarize variable values for plotting 
  data_for_plot <- data_beta %>%
    group_by(variable, beta_group) %>%
    summarize(
      mean_value = mean(value, na.rm = TRUE),
      lower_value = quantile(value, 0.05, na.rm = TRUE),
      upper_value = quantile(value, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
  
  # return this data frame
  return(data_for_plot)
}

#beta_signal

#' beta_signal_data
#'
#' @param data 
#' @param beta_low 
#' @param beta_high 
#'
#' @returns
#' @export
#'
#' @examples
beta_signal_data <- function(data, beta_low = 0.20, beta_high = 0.80) {
  
  # define low and high beta values
  beta_low_cutoff <- quantile(data$BETA, beta_low, na.rm = TRUE)
  beta_high_cutoff <- quantile(data$BETA, beta_high, na.rm = TRUE)
  
  # Keep ONLY low and high BETA runs
  data_beta <- data %>%
    mutate(
      beta_group = case_when(
        BETA <= beta_low_cutoff ~ "low",
        BETA >= beta_high_cutoff ~ "high",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(beta_group))
  
  # Summarize variables based by their BETA group
  group_summary <- data_beta %>%
    group_by(variable, beta_group) %>%
    summarize(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  # calculate standardized separation -- how far apart are high and low beta runs
  separation_plot_data <- group_summary %>%
    tidyr::pivot_wider(
      names_from = beta_group,
      values_from = c(mean_value, sd_value, n)
    ) %>%
    mutate(
      difference = mean_value_high - mean_value_low,
      pooled_sd = sqrt(
        ((n_low - 1) * sd_value_low^2 + (n_high - 1) * sd_value_high^2) / (n_low + n_high - 2)),
      standardized_separation = difference / pooled_sd,
      abs_standardized_separation = abs(standardized_separation)
    )
  
  # return the new data frame
  return(separation_plot_data)
  
}


#' Plotting Beta Effect for given data set
#'
#' @param colors Character string of colors for BETA factor levels.
#' @param x 
#' @param var 
#'
#' @returns A plot with summary values for each BETA factor level.
#' @export
#'
#' @examples
plot_beta_effect <- function(x,units, colors, var) {
  
  #subset
  data <- subset(x = x,
                variable == var)
  
  # plotting
  ggplot(data = data, aes(x = beta_group, y = mean_value, color = beta_group)) +
  geom_pointrange(aes(ymin = lower_value, ymax = upper_value), size = 0.8) +
  labs(y = units,
       x = "BETA Group",
       title = unique(data$variable)) +
  scale_color_manual(values = setNames(colors, c("Low BETA", "Middle BETA", "High BETA"))) +
  guides(color = "none") +
  theme_light()
}




