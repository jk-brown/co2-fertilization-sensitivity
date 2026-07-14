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
    
# 1.1 Calculate BETA group cutoffs
beta_low <- quantile(
  mod_beta_plot_data$BETA,
  probs = 0.20,
  na.rm = TRUE
)

beta_high <- quantile(
  mod_beta_plot_data$BETA,
  probs = 0.80,
  na.rm = TRUE
)

# Automatically create labels from the calculated cutoffs
low_beta_label <- paste0(
  "Low BETA (< ",
  round(beta_low, 2),
  ")"
)

high_beta_label <- paste0(
  "High BETA (> ",
  round(beta_high, 2),
  ")"
)

# 1.2 Add BETA groups
time_series_plot_data <- mod_beta_plot_data %>% 
  mutate(
    beta_group = case_when(
      BETA <= beta_low  ~ low_beta_label,
      BETA >= beta_high ~ high_beta_label,
      TRUE              ~ "Middle BETA"
    ),
    beta_group = factor(
      beta_group, 
      levels = c(
        low_beta_label,
        "Middle BETA",
        high_beta_label
      )
    )
  )
