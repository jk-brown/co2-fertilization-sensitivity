# Goal: Add a BETA column based on run number, label BETA values as "high" "middle" or "low"
# based on BETA value. Save result.

# read in beta params with run_number
beta_values_rn <- read.csv("data/beta_values_rn.csv")

# read in initial model results
mod_result <- read.csv("outputs/initial_model_result.csv")

# 1.0 Join beta values with model results 
mod_beta_plot_data <- 
  beta_values_rn %>% 
  left_join(mod_result, by = "run_number")
    
# 1.1 Add BETA group column
beta_low <- quantile (mod_beta_plot_data$BETA, probs = 0.20)
beta_high <- quantile (mod_beta_plot_data$BETA, probs = 0.80)

# 1.2 edit plot data with BETA groups 
high_low_beta_plot_data <- mod_beta_plot_data %>% 
  mutate(
    beta_group = case_when(
      BETA <= beta_low ~ "Low BETA (< 0.59)",
      BETA >= beta_high ~ "High BETA (> 0.75)",
      TRUE ~ "Middle BETA"), 
    beta_group = factor(
      beta_group, 
      levels = c(
        "Low BETA (< 0.59)", 
        "Middle BETA", 
        "High BETA (> 0.75)"
      )
    )
  )

# 1.3 Save time series plot data with BETA groupings
write.csv(high_low_beta_plot_data, 
          "outputs/time_series_plot_data.csv", 
          row.names = F)
