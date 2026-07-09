# Source 
source("scripts/source/source_all.R")

# load metric_results 
lc_metric_results <- read.csv("outputs/lc_metric_results.csv")

# BETA Effect Summarization

# Questions
# 1) How different are model output variables across low, middle, high BETA values. 

# 2) How does the difference between low and high BETA values change through the model 
# process?

# Q1) Compute the beta effect for all variables using beta_effect_data function (Sofia)
beta_effect <-beta_effect_data(lc_metric_results)

# Q2) Compute standardized difference between low and high beta for each variable (Izzah)
beta_signal <- beta_signal_data(lc_metric_results)

## Plotting Results 

# Q1 -- Plot mean values with CI (5-95%)

beta_effect_plot <- ggplot(
  data = beta_effect, 
  aes(
    x = beta_group,
    y = mean_value)) + 
  geom_pointrange(
    aes(
    ymin = lower_value,
    ymax = upper_value)) + 
  facet_wrap(~variable, scales = "free_y") + 
  labs(
    x = "BETA level",
    y = "Late-century value",
    title = "Late-century outputs - Difference across BETA levels"
  ) + 
  theme_light()

beta_effect_plot

# Q2 
beta_signal_plot <- 
  ggplot() + 
  geom_col(
    data = beta_signal,
    aes(
      x = variable,
      y = abs_standardized_separation
    )
  ) + 
  labs(
    x = "Variable",
    y = "Standardized Difference between low- and high-BETA",
    title = "Beta-driven differences across variables"
  ) + 
  theme_light()
beta_signal_plot
  





