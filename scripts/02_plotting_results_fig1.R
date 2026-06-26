# read in beta params with run_number
beta_values_rn <- read.csv("data/beta_values_rn.csv")

# read in inital model results
mod_result <- read.csv("outputs/initial_model_result.csv")

# plot time series colored by BETA value 
# 1.0 Join beta values with model results 
mod_beta_plot_data <- 
  beta_values_rn %>% 
  left_join(mod_result, by = "run_number")

# 1.2 construct initial plot 
plot_mod_result1 <- 
  ggplot(data = mod_beta_plot_data) +
  geom_line(
    aes(
      x = year,
      y = value,
      group = run_number,
      color = BETA)) +
  facet_wrap(~variable, scales = "free_y") + 
  theme_light()
plot_mod_result1
    
# Add BETA group column
beta_low <- quantile (mod_beta_plot_data$BETA, probs = 0.20)
beta_high <- quantile (mod_beta_plot_data$BETA, probs = 0.80)

# edit plot data with BETA groups 
high_low_beta_plot_data <- mod_beta_plot_data %>% 
  mutate(
    beta_group = case_when(
    BETA <= beta_low ~ "Low BETA",
    BETA >= beta_high ~ "High BETA",
    TRUE ~ "Middle BETA"))

# Plotting BETA group 
group_beta_plot <-
  ggplot(data = high_low_beta_plot_data) +
  geom_line(
    aes(
      x = year,
      y =value,
      group = run_number,
      color = beta_group),
      alpha = 0.35) +
    
  facet_wrap(~variable, scales = "free_y") +
    scale_color_manual(
      values = c(
        "Low BETA" = "red",
        "Middle BETA" = "gold",
        "High BETA" = "blue")) + 
    theme_light()
  group_beta_plot



