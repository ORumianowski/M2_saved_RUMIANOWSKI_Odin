
library(IPMbook)
library(jagsUI)
library(tidyverse)

# Dataset -----------------------------------------------------------------


source("simul_cmr_v3.R")
source("simul_survey_v3.R")

n.colony = 5

survey_data = B # Survey dataset - Number of breeders reported
marr = marray(y, unobs=n.colony+1) # convert capture history to marray # unobserved states: 10 (adults in AliveElsewhere) and 11 to 15 (prebreeders)

save(B, file = "B_simulation_v3.Rda")
save(marr, file = "marr_simulation_v3.Rda")
CH = y
save(CH, file = "CH_simulation_v3.Rda")


