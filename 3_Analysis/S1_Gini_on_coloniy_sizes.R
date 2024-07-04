setwd("C:/Users/33763/Desktop/Odin_RUMIANOWSKI_M2_saved/modele_du_rapport")

library(tidyverse)
library(ggplot2)
library(readxl)
library(ineq)

path_dataset_survey1 = "0_Datasets/Peron_dataset_0502024/public-colonie.csv"


survey1 = read.csv(path_dataset_survey1) %>% 
  dplyr::select(annee_observation_colonie,
                id_site_mesure,
                nombre_couple_colonie) %>% 
  rename(year = annee_observation_colonie,
         site = id_site_mesure,
         N = nombre_couple_colonie)

survey_p1 = survey1 %>%  
  subset(., year %in% 1986:2019)

survey = survey_p1 %>% spread(key = year, value = N)

survey[is.na(survey)] = 0

B = survey

# Nombre de couple issu de la thèse de V.GROBOIS
B[B$site == "LR_E",2:22] = c(7145, 8059, 5585, 7893, 8487, 10428, 10877, 7310, 5882, 6027, 6584, 8499, 9225, 6639, 7000, 7000, 7000, 7000, 7000, 7000, 7000)%/%2

# Sans les noms de sites
B = B[,2:35]


gini_no0 = B |> 
  pivot_longer(everything(), names_to = "year", values_to = "n") |> 
  filter(n != 0) |> 
  group_by(year) |> 
  summarise(coefficients_gini = Gini(n)) |> 
  mutate(year = as.numeric(year))


my_function = function(B_t){
  sd(B_t/sum(B_t))
}

coefficients_my_function <- apply(B, 2, my_function)

coefficients_gini <- apply(B, 2, Gini)


dt_gini = data.frame(coefficients_gini,
                     coefficients_my_function,
                     year = 1986:2019)


ggplot()+
  geom_line(data = gini_no0, aes(x= year, y= coefficients_gini))+
  geom_vline(xintercept = 2006.5, color = "bisque3", alpha = 0.5, linewidth=3)+
  labs(x = "Year", y = "Colony Sizes - Gini Coefficient", title = "") +
  theme_classic()
  


