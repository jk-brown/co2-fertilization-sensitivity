## Before running the code below, run code in scripts labeled 01-04.

# Source scripts with helper functions and figure mapping information
source("scripts/source/source_all.R")
## Source includes mapping for figure labels and colors.

# create figs directory
dir.create("figures")

# Plot 1: Time series plots for each variable individually

## Plot each panel individually
## Plot 1a - NPP
# get subset of NPP data
npp_dat <- subset(x = time_series_plot_data, 
                  variable == "NPP")
npp_units <- unique(npp_dat$units)

# plot NPP
npp_plot <-
  ggplot(data = npp_dat) +
  geom_line(aes(
    x = year,
    y = value,
    group = run_number,
    color = beta_group
  ),
  alpha = 0.35) +
  labs(x = "Year", 
       y = npp_units, 
       color = "BETA Group") +
  scale_color_manual(values = setNames(
    beta_group_cols, 
    c(low_beta_label, "Middle BETA", high_beta_label))) +
  theme_light()
npp_plot

# save plot 
ggsave(filename ="figures/npp_plot.png", 
       plot = npp_plot, 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in", 
       dpi = 300)

# Izzah 
#TODO
## Plot 1b - veg_c 
# get subset of veg_c data
veg_c_dat <- subset(x = time_series_plot_data, 
                  variable == "veg_c")
veg_c_units <- unique(veg_c_dat$units)

# plot VEG_C

veg_c_plot <-
  ggplot(data = veg_c_dat) +
  geom_line(aes(
    x = year,
    y = value,
    group = run_number,
    color = beta_group
  ),
  alpha = 0.35) +
  labs(x = "Year", 
       y = veg_c_units, 
       color = "BETA Group") +
  scale_color_manual(values = setNames(
    beta_group_cols, 
    c(low_beta_label, "Middle BETA", high_beta_label))) +
  theme_light()
veg_c_plot

# save plot 
ggsave(filename ="figures/veg_c_plot.png", 
       plot = veg_c_plot, 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in", 
       dpi = 300)

# Izzah 
#TODO
## Plot 1c - SOIL_C 
# get subset of SOIL_C data

soil_c_dat <- subset(x = time_series_plot_data, 
                    variable == "soil_c")
soil_c_units <- unique(soil_c_dat$units)

# plot SOIL_C

soil_c_plot <-
  ggplot(data = soil_c_dat) +
  geom_line(aes(
    x = year,
    y = value,
    group = run_number,
    color = beta_group
  ),
  alpha = 0.35) +
  labs(x = "Year", 
       y = soil_c_units, 
       color = "BETA Group") +
  scale_color_manual(values = setNames(
    beta_group_cols, 
    c(low_beta_label, "Middle BETA", high_beta_label))) +
  theme_light()
soil_c_plot

# save plot 
ggsave(filename ="figures/soil_c_plot.png", 
       plot = veg_c_plot, 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in", 
       dpi = 300)

# Sofia 
#TODO
## Plot 1d - CONCENTRATIONS_CO2 
# get subset of CONCENTRATIONS_CO2 data
CO2_concentration_dat <- subset(x = time_series_plot_data, 
                  variable == "CO2_concentration")
CO2_concentration_units <- unique(CO2_concentration_dat$units)

# plot CONCENTRATIONS_CO2
CO2_concentration_plot <-
  ggplot(data = CO2_concentration_dat) +
  geom_line(aes(
    x = year,
    y = value,
    group = run_number,
    color = beta_group
  ),
  alpha = 0.35) +
  labs(x = "Year", 
       y = CO2_concentration_units, 
       color = "BETA Group") +
  scale_color_manual(values = setNames(
    beta_group_cols, 
    c(low_beta_label, "Middle BETA", high_beta_label))) +
  theme_light()
CO2_concentration_plot

# save plot 
ggsave(filename ="figures/CO2_concentration_plot.png", 
       plot = CO2_concentration_plot, 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in", 
       dpi = 300)

# Sofia 
#TODO
## Plot 1e - GMST 
# get subset of GMST data
gmst_dat <- subset(x = time_series_plot_data, 
                  variable == "gmst")
gmst_units <- unique(gmst_dat$units)

# plot CONCENTRATIONS_CO2
gmst_plot <-
  ggplot(data = gmst_dat) +
  geom_line(aes(
    x = year,
    y = value,
    group = run_number,
    color = beta_group
  ),
  alpha = 0.35) +
  labs(x = "Year", 
       y = gmst_units, 
       color = "BETA Group") +
  scale_color_manual(values = setNames(
    beta_group_cols, 
    c(low_beta_label, "Middle BETA", high_beta_label))) +
  theme_light()
gmst_plot

# save plot 
ggsave(filename ="figures/gmst_plot.png", 
       plot = gmst_plot, 
       device = "png", 
       width = 7, 
       height = 5, 
       units = "in", 
       dpi = 300)


###### Keep below for making combined figures. 
library(patchwork)

combined_plot <-
  # may need to replace names depending on code above. 
  npp_plot +
  veg_c_plot +
  soil_c_plot +
  co2_plot +
  gmst_plot +
  guide_area() +
  plot_layout(
    design = "
    ABC
    DEF
    ",
    guides = "collect",
    axis_titles = "collect_x"
  ) 
combined_plot


# Plot 2: Mean late century response for each variable across BETA levels

## Plot each panel individually
## Plot 2a - NPP
beta_on_npp_effect <- subset(x = beta_effect, 
                             variable == "NPP")
beta_on_npp_plot <- ggplot(data = beta_on_npp_effect, 
                      aes(x = beta_group, 
                          y = mean_value,
                          color = beta_group)) +
  geom_pointrange(
    aes(
      ymin = lower_value,
      ymax = upper_value), 
    size = 0.8) + 
  labs(
    y = npp_units,
    x = "BETA Group",
    title = "Mean Late-century variable response (95% CI)"
  ) + 
  scale_color_manual(
    values = setNames(
      beta_group_cols, 
    c("Low BETA", "Middle BETA", "High BETA"))) +
  guides(color = "none") +
  theme_light()
beta_on_npp_plot

# save plot 
ggsave(filename ="figures/beta_on_npp_plot.png", 
       plot = beta_on_npp_plot, 
       device = "png", 
       width = 5, 
       height = 3, 
       units = "in", 
       dpi = 300)

### We need to make the above figure for each of the 

## Plot 2b - VEG_C


## Plot 2c - SOIL_C


## Plot 2d - CO2_concentrations


## Plot 2e - gmst


# Plot 3: Standardized BETA signal across variables

# reorganize the factors so the order is correct.
beta_signal$variable <- factor(
  beta_signal$variable,
  levels = c("NPP", "veg_c", "soil_c", "CO2_concentration", "gmst")
)

beta_signal_plot <- 
  ggplot() + 
  geom_col(
    data = beta_signal,
    aes(
      x = variable,
      y = abs_standardized_separation
    ),
    fill = "white",
    color = "black"
  ) + 
  labs(
    x = "Variable",
    y = "Standardized difference between low- and high-BETA",
    title = "BETA-driven differences across variables"
  ) + 
  theme_light()

beta_signal_plot



