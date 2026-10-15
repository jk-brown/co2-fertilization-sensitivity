## Static BETA distribution figure for presentation

library(tidyverse)

# Assign each BETA value to a group
beta_plot_data <- beta_values %>%
  mutate(
    beta_group = case_when(
      BETA < 0.55 ~ "Low BETA",
      BETA > 0.75 ~ "High BETA",
      TRUE        ~ "Middle BETA"
    ),
    beta_group = factor(
      beta_group,
      levels = c("Low BETA", "Middle BETA", "High BETA")
    )
  )

# Select 5 values spread across each BETA category
beta_plot_subset <- beta_plot_data %>%
  group_by(beta_group) %>%
  arrange(BETA) %>%
  slice(round(seq(1, n(), length.out = 5))) %>%
  ungroup()

# Make static figure
beta_plot <- ggplot() +
  geom_density(
    data = beta_plot_data,
    aes(x = BETA),
    adjust = 3,
    n = 2048,
    linewidth = 1
  ) +
  geom_point(
    data = beta_plot_subset,
    aes(
      x = BETA,
      y = 0,
      color = beta_group
    ),
    size = 4
  ) +
  scale_color_manual(
    values = c(
      "Low BETA" = "red",
      "Middle BETA" = "gold",
      "High BETA" = "blue"
    )
  ) +
  scale_x_continuous(
    limits = c(
      min(beta_plot_data$BETA) - 0.1,
      max(beta_plot_data$BETA) + 0.1
    ),
    expand = expansion(mult = 0)
  ) +
  labs(
    x = "BETA Values",
    y = NULL,
    color = NULL
  ) +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = "Mean = 0.65\nSD = 0.10",
    hjust = 1.3,
    vjust = 1.5,
    size = 5
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    axis.line = element_blank(),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    legend.position = "bottom"
  )

beta_plot

ggsave("figures/sample_figs/beat_sampling.png", 
       beta_plot, 
       width = 7, 
       height = 5, 
       units = "in", 
       device = "png")


## new constrained distribution
# Hypothetical distribution after applying observational constraints
constrained_mean <- 0.70
constrained_sd   <- 0.08

constrained_distribution <- tibble(
  BETA = seq(
    min(beta_plot_data$BETA) - 0.1,
    max(beta_plot_data$BETA) + 0.1,
    length.out = 2048
  )
) %>%
  mutate(
    density = dnorm(
      BETA,
      mean = constrained_mean,
      sd = constrained_sd
    )
  )

# plotting
beta_plot2 <- ggplot() +
  geom_density(
    data = beta_plot_data,
    aes(x = BETA),
    adjust = 3,
    n = 2048,
    linewidth = 1
  ) +
  geom_line(
    data = constrained_distribution,
    aes(
      x = BETA,
      y = density
    ),
    color = "dodgerblue",
    linetype = "dashed",
    linewidth = 1.2
  ) +
  geom_point(
    data = beta_plot_subset,
    aes(
      x = BETA,
      y = 0,
      color = beta_group
    ),
    size = 4
  ) +
  scale_color_manual(
    values = c(
      "Low BETA" = "red",
      "Middle BETA" = "gold",
      "High BETA" = "blue"
    )
  ) +
  scale_x_continuous(
    limits = c(
      min(beta_plot_data$BETA) - 0.1,
      max(beta_plot_data$BETA) + 0.1
    ),
    expand = expansion(mult = 0)
  ) +
  labs(
    x = "BETA Values",
    y = NULL,
    color = NULL
  ) +
  annotate(
    "text",
    x = Inf,
    y = Inf,
    label = "Mean = 0.65\nSD = 0.10",
    hjust = 1.3,
    vjust = 1.5,
    size = 5
  ) +
  annotate(
    "text",
    x = constrained_mean,
    y = max(constrained_distribution$density) * 0.92,
    label = "Hypothetical constrained distribution",
    color = "dodgerblue",
    hjust = 1.15,
    vjust = 1.5,
    size = 3.7
  ) +
  theme_classic(base_size = 16) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    axis.line = element_blank(),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    legend.position = "bottom"
  )

beta_plot2
