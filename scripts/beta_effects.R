# Source 
source("scripts/source/source_all.R")

# BETA Effect Summarization

# Questions
# 1) How different are model output variables across low, middle, high BETA values. 

# 2) How does the difference between low and high BETA values change through the model 
# process?

# Q1) Compute the beta effect for all variables using beta_effect_data function (Sofia)
beta_effect <-beta_effect_data(lc_metric_results)

# Q2) Compute standardized difference between low and high beta for each variable (Izzah)
