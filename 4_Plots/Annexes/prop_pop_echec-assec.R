#*##############################################################################
library(tidyverse)

assec = read.csv("public-assecs.csv") |> 
  filter(grepl("oui", assec))
colonies = read.csv("public-colonie.csv") |> 
  mutate(annee_observation_colonie = annee_observation_colonie + 1) |> 
  left_join(assec, by = c("annee_observation_colonie" = "annee_observation_assecs", "id_site_mesure"))


################################################################################.
## Proportion de la population sur colonies en échec ---------------------------
prop_pop_echec = colonies |> 
  mutate(echec = indice_reproduction_colonie == 0) |> 
  group_by(annee_observation_colonie) |> 
  mutate(total_annee = sum(nombre_couple_colonie)) |> 
  group_by(annee_observation_colonie, total_annee, echec) |> 
  summarise(total_srep = sum(nombre_couple_colonie)) |> 
  filter(echec == T) |> 
  mutate(prop_srep = total_srep/total_annee)

plot.prop_pop_echec = ggplot() +
  geom_line(data = prop_pop_echec, aes(x = annee_observation_colonie, y = prop_srep)) +
  geom_point(data = prop_pop_echec, aes(x = annee_observation_colonie, y = prop_srep)) +
  scale_x_continuous(breaks = c(1975 + (1:9)*5)) +
  labs(x = "Année", y = "Proportion de la population en échec") +
  theme_classic()

plot.prop_pop_echec

ggsave(plot.prop_pop_echec, filename = "prop_pop_echec.png",
       dpi = 600, width = 18, height = 10, units = "cm")



#*##############################################################################
## Taille colonies en assec ----------------------------------------------------
assec_taille = crossing(annee_observation_colonie = 1976:2023) |> 
  left_join(colonies |> 
              filter(!is.na(assec) & assec == "oui") |> 
              select(annee_observation_colonie, id_site_mesure, nombre_couple_colonie)) |> 
  replace_na(list(nombre_couple_colonie = 0))

plot.assec_taille = ggplot() +
  geom_point(data = assec_taille, aes(x = annee_observation_colonie, y = sum)) +
  geom_area(data = assec_taille |> group_by(annee_observation_colonie) |> summarise(max_couple_colonie = max(sum)),
            aes(x = annee_observation_colonie, y = max_couple_colonie), alpha = 0.5) +
  scale_x_continuous(breaks = c(1975 + (1:9)*5)) +
  labs(x = "Year", y = " Number of breeding pairs affected \n by the drying of ponds") +
  theme_classic()

plot.assec_taille

ggsave(plot.assec_taille, filename = "assec_taille.png",
       dpi = 600, width = 18, height = 10, units = "cm")



assec_taille = assec_taille %>% 
  group_by(annee_observation_colonie) %>% 
  mutate(sum = sum(nombre_couple_colonie))

