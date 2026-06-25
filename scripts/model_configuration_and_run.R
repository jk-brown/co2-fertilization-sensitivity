# Config and run Hector with Matilda

# 1.0 Load libraries 
library(matilda)
library(tidyverse)

# 2.0 Initialize model instance 
## Load INI file -- SSP2-4.5 Climate Scenario
ssp245_ini = system.file("input/hector_ssp245.ini", package = "hector")

# 2.1 Create Hector core
core <- newcore(ssp245_ini, name = "SSP2-4.5")

