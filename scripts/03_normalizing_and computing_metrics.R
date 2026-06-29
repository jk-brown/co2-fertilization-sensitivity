# The goal of this script is to use our model results to compute metrics.
# Metrics are for example: Mean long-term GMST for 2081-2100 relative to the 1850-1900 reference period. 
# I would like you to make these calculations in this script using the GitHub workflow.
# You will use functions that I wrote and saved in this project to:
## 1) Normalize our GMST data so the values are relative to 1850-1900
## 2) Compute late-century median for our important variables: NPP, VEG_C, SOIL_C, CONCENTRATIONS_CO2, and GMST
# This will give us the 20 year median for each of our variables using all the runs in our model. 
# From this we will be able to state something like: When BETA is assumed "high" (>0.75) our model projections show
# global mean surface temperature anomaly will be 2.5 C relative to pre-industrial timer period, compared to a GMST of
# 2.0 C when BETA is assumed "low" (< 0.55).

# TODO:
# Izzah (1): Use normalization function to normalize GMST() to the 1850-1900 reference period.
# Izzah (2): Use the produce_metrics() function to compute long-term medians for: NPP() and VEG_C()
# Sofia: Use the produce_metrics() function to compute long-term median for: SOIL_C(), CONCENTRATIONS_CO2(), and GMST()

# 1.0 Load our prelim test data saved in our project.
model_result <- read.csv("outputs/initial_model_result.csv")

# 1.1 Source our helper functions 
# Check out the the helper_functions.R in the 'source' folder.
# We will use source all to make sure we have all the packages and original functions we need.
source("scripts/source/source_all.R")

# Izzah (1):
# 2.0 Normalize GMST using model_result to the 1850:1900 reference period.
# Write this code on the first empty line following these instructions.
# Open the file `scripts/source/helper_functions.R` and read the usage comments above 
# the normalize_gmst() for how to use.
# Be sure to use a good object name.



# Izzah (2): 
# 3.0 Compute long-term median for NPP() and VEG_C()
# Open the file `scripts/source/helper_functions.R` and read the usage comments above 
# the produce_metrics() for how to use the function.
# Write this code on the first empty line following these instructions.
# Name each object with lc (for late-century) and the variable being summarized:
# ex: 'lc_npp'
# The result will be a new dataframe added to the global environment (top right panel).



# Sofia (1):
# 3.1 Compute long-term median for SOIL_C(), CONCENTRATIONS_CO2(), GMST() 
# Open the file `scripts/source/helper_functions.R` and read the usage comments above 
# the produce_metrics() for how to use the function.
# Write this code on the first empty line following these instructions.
# Name each object with lc (for late-century) and the variable being summarized:
# ex: 'lc_soil_c'



# Once metric summary data frames are created from the above code, we can combine all 
# these metric data frames into a single data frame with all the long-term, late-century (2081-2100) 
# median values.
lc_metric_results <- do.call(rbind,
                             list(lc_npp, 
                                  lc_veg_c, 
                                  lc_soil_c, 
                                  lc_co2_concentration,
                                  lc_gmst))
