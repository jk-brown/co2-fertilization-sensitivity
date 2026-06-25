# load libraries
source("scripts/load_libraries.R")

# read in beta params with run_number
beta_values_rn <- read.csv("data/beta_values_rn.csv")

# read in inital model results
mod_result <- read.csv("outputs/initial_model_result.csv")

# plot time series colored by BETA value 
# 1.0 Join beta values with model results 
mod_beta_plot_data <- 
  beta_values_rn %>% 
  left_join(mod_result, by = "run_number")

# 1.2 construct plot 
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
    