library(tidyverse)
library(patchwork)

setwd("C:/Users/33763/Desktop/Odin_RUMIANOWSKI_M2_saved/modele_du_rapport")


colonies <- read.csv("0_datasets/survey_post_2008_150424/public-colonie.csv")
sites <- read.csv("0_datasets/survey_post_2008_150424/public-site_mesure.csv")


#*##############################################################################
## Occupation de la plaine du Forez --------------------------------------------
abund_df <- colonies |> 
  select(!indice_reproduction_colonie) |> 
  pivot_wider(names_from = annee_observation_colonie, values_from = nombre_couple_colonie) |> 
  pivot_longer(!id_site_mesure, names_to = "annee_observation_colonie", values_to = "nombre_couple_colonie") |> 
  replace_na(list(nombre_couple_colonie = 0)) |> 
  left_join(sites |> 
              select(id_site_mesure, longitude_site_mesure, latitude_site_mesure)) |> 
  mutate(longitude_site_mesure = as.numeric(longitude_site_mesure),
         latitude_site_mesure = as.numeric(latitude_site_mesure),
         annee_observation_colonie = as.integer(annee_observation_colonie)) %>% 
  select(!nombre_couple_colonie) %>% 
  select(!annee_observation_colonie) %>% 
  unique()



lims = data.frame(X = c(4.03, 4.30), Y = c(45.47, 45.88))

# Map background:
world = OpenStreetMap::openmap(upperLeft = c(lims$Y[1], lims$X[1]), 
                               lowerRight = c(lims$Y[2], lims$X[2]), 
                               zoom = 13,
                               type = "esri-topo", mergeTiles = TRUE)
world = OpenStreetMap::openproj(world)



abund_df = abund_df %>% 
  mutate(site = case_when(
    id_site_mesure == "LR_E" ~ "LR", 
    id_site_mesure == "MA_E" ~ "MA", 
    id_site_mesure == "PC_E" ~ "VE", 
    id_site_mesure == "V5_E" ~ "V5", 
    id_site_mesure == "VS_E" ~ "VS",
    id_site_mesure == "LA_E" ~ "LA", 
    id_site_mesure == "SV_E" ~ "SV", 
    id_site_mesure == "WV_E" ~ "WV", 
    id_site_mesure == "SZ_E" ~ "SZ", 
    TRUE ~ "AE")) %>% 
  mutate(site_type = case_when(
    id_site_mesure == "LR_E" ~ "Main colony (LR)", 
    id_site_mesure == "MA_E" ~ "Satellite colony (SAT)", 
    id_site_mesure == "PC_E" ~ "Satellite colony (SAT)", 
    id_site_mesure == "V5_E" ~ "Satellite colony (SAT)", 
    id_site_mesure == "VS_E" ~ "Satellite colony (SAT)",
    id_site_mesure == "LA_E" ~ "Satellite colony (SAT)", 
    id_site_mesure == "SV_E" ~ "Satellite colony (SAT)", 
    id_site_mesure == "WV_E" ~ "Satellite colony (SAT)", 
    id_site_mesure == "SZ_E" ~ "Satellite colony (SAT)", 
    TRUE ~ "Alive Elsewhere colony (AE)"))


library(OpenStreetMap)
library(ggplot2)
library(dplyr)
library(ggspatial)

# Assume 'world' and 'abund_df' are already defined in your environment

# Reorder the data frame so that smaller points are plotted first
abund_df <- abund_df %>% 
  arrange(site_type)

abund_df <- abund_df %>% 
  mutate(site_type = factor(site_type, levels = c("Main colony (LR)", "Satellite colony (SAT)", "Alive Elsewhere colony (AE)")))

location_map = OpenStreetMap::autoplot.OpenStreetMap(world) +
  geom_point(data = abund_df,
             aes(x = longitude_site_mesure, y = latitude_site_mesure,
                 shape = site_type,
                 color = site_type,
                 size = site_type  # Map size to site_type
             ),
             shape = 16) +
  geom_text(data = abund_df %>% filter(site != "AE"),
            aes(x = longitude_site_mesure, y = latitude_site_mesure,
                label = site),
            size = 3,
            fontface = "bold"
  ) +
  scale_size_manual(values = c(12, 8, 2)) +  # Define custom sizes
  scale_color_manual(values = c("darkgrey","darkgrey","black")) +
  labs(title = "",
       x = "Longitude", y = "Latitude",
       size = "Colony category", color = "Colony category"  # Add legend title for size
  ) + theme(legend.position = "bottomleft")+
  #annotation_scale(plot_unit = "km")+
  annotation_north_arrow(location = "tr")  #+ theme(legend.position = "bottomleft")

# Print the map
print(location_map)

# faire avec des disque gris clair autouré de noir



