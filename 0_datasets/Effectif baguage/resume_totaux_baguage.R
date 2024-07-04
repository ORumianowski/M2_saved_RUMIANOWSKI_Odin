
library(tidyverse)

dtotaux <- read.csv('totaux_baguage_v1.csv') %>% 
  rename(colony = id_site_mesure,
         year = annee) %>% 
  dplyr::select(colony, year, n_bague, age)
  

str(dtotaux)

ad = subset(dtotaux, age=='adult') %>% 
  dplyr::select(-age)
juv = subset(dtotaux, age=='chick') %>% 
  dplyr::select(-age)


ad_pivote <- ad %>%
  pivot_wider(names_from = year, values_from = n_bague)

ad_LR = subset(ad_pivote, colony %in% c("LR_C", "LR_E"))
ad_SAT = subset(ad_pivote, colony %in% c("MA_E", "V5_E", "PC_E"))

#colSums(ad_pivote[,2:ncol(ad_pivote)], na.rm = T)


juv_pivote <- juv %>%
  pivot_wider(names_from = year, values_from = n_bague)

juv_LR = subset(juv_pivote, colony %in% c("LR_C", "LR_E"))
juv_SAT = subset(juv_pivote, colony %in% c("MA_E", "V5_E", "PC_E"))

total_LR = colSums(juv_LR[,2:ncol(juv_LR)], na.rm = T)
total_SAT = colSums(juv_SAT[,2:ncol(juv_SAT)], na.rm = T)

bilan = data.frame(total_LR, total_SAT)

ringed_nest_for_simulation = bilan[c(as.character(1985:2005)),] %>% 
  as.data.frame() 

ringed_nest_for_simulation = ringed_nest_for_simulation %>% 
  mutate(year = rownames(ringed_nest_for_simulation) %>% as.integer())



save(ringed_nest_for_simulation, file = "ringed_nest_for_simulation.Rda")

