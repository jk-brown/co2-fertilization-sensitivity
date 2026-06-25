# Config and run Hector with Matilda

# 1.0 Load libraries 
library(matilda)
library(tidyverse)

# 2.0 Initialize model instance 
## Load INI file -- SSP2-4.5 Climate Scenario
ssp245_ini = system.file("input/hector_ssp245.ini", package = "hector")

# 2.1 Create Hector core
core <- newcore(ssp245_ini, name = "SSP2-4.5")

# 3.0 Generate BETA param values
set.seed(25) # standardize random param draws

# 3.1 produce 50 draws of BETA
param_values <- generate_params(core, draws = 50)
  
## sanity check
head(param_values)

# 3.2 creating BETA parameter df
beta_values <- param_values %>% 
  select(BETA)

# 3.3 Add run_numbers to BETA
beta_values_rn <- beta_values %>% 
  mutate(run_number = row_number())
write.csv(beta_values_rn, "data/beta_values_rn.csv", row.names = FALSE)

