setwd("C:/Users/33763/Desktop/Odin_RUMIANOWSKI_M2_saved/modele_du_rapport")


library(tidyverse)
library(ggplot2)
library(readxl)

path_dataset_fecundity = "0_datasets/Peron_dataset_0502024/public-colonie.csv"

fecundity = read.csv(path_dataset_fecundity) %>% 
  dplyr::select(annee_observation_colonie,
                id_site_mesure,
                indice_reproduction_colonie) %>% 
  rename(year = annee_observation_colonie,
         site = id_site_mesure,
         active = indice_reproduction_colonie) %>% 
  mutate(site = case_when(
    site == "LR_E" ~ "LR",
    site == "MA_E" ~ "MA",
    site == "PC_E" ~ "VE",
    site == "V5_E" ~ "V5",
    site == "VS_E" ~ "VS",
    site == "LA_E" ~ "LA",
    site == "SV_E" ~ "SV",
    site == "WV_E" ~ "WV",
    site == "SZ_E" ~ "SZ",
    TRUE ~ "AE")
    )



fecundity = fecundity %>%  
  subset(., year %in% 1986:2019)%>%
  filter(!(site == "AE"))



fecundity = fecundity%>%
  pivot_wider(names_from = year, values_from = active) 

fecundity = fecundity[, 2:ncol(fecundity)]

table(fecundity %>% as.data.frame() %>% as.matrix() %>%  as.vector())

fecundity[fecundity==3] = 1
fecundity[fecundity==2] = 1
fecundity[fecundity==1] = 1

fecundity[fecundity==0] = 0

fecundity[is.na(fecundity)] = 2

table(fecundity %>% as.data.frame() %>% as.matrix() %>%  as.vector())


fecundity = as.matrix(fecundity)
colnames(fecundity) <- NULL


fecundity = rbind(fecundity,
                  matrix(1, nrow = 1, ncol = 34))

fecundity = fecundity[c(1:2, 5:8,10), 21:22]



save(fecundity, file = "fecundity_2_4.Rda")















