#setwd("C:/Users/gregory/Documents/These CEFE")

library(tidyverse)
library(patchwork)
library(gganimate)
library(animation)



####################################*###########################################
## Effectifs plaine & nombre colonies ------------------------------------------
colonies <- read.csv("public-colonie.csv")
sites <- read.csv("public-site_mesure.csv")

### > Effectifs plaine ----
total_forez <- colonies |> 
  # Ajout des effectifs à la Ronze par année:
  left_join(colonies |> 
              filter(grepl("LR_", id_site_mesure)) |> 
              select(annee_observation_colonie, nombre_couple_colonie) |> 
              rename(n_LR = nombre_couple_colonie)) |> 
  # Total des effectifs par année:
  group_by(annee_observation_colonie) |> 
  summarise(n_Total = sum(nombre_couple_colonie),
            n_La_Ronze = unique(n_LR)) |> 
  # Compléter NA en 0 pour la Ronze en 2007:
  replace_na(list(n_La_Ronze = 0)) |>
  # Changer la structure du tableau pour le plot:
  pivot_longer(starts_with("n_"), names_to = "type", names_prefix = "n_", values_to = "n") |> 
  mutate(type = factor(type, levels = c("Total", "La_Ronze")))

plot.total_forez <- ggplot() +
  geom_line(data = total_forez, aes(x = annee_observation_colonie, y = n, 
                                    colour = type, linetype = type)) +
  scale_colour_manual(values = c("black", "grey50")) +
  scale_linetype_manual(values = c(1, 2)) +
  geom_point(data = total_forez |> filter(type == "Total"),
             aes(x = annee_observation_colonie, y = n)) +
  scale_x_continuous(breaks = 1970+1:10*5) +
  scale_y_continuous(breaks = 1:8*1000,
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "", y = "",
       colour = "", linetype = "",
       title = "Nombre de couples dans la plaine du Forez") +
  theme_classic() +
  theme(legend.text = element_text(size = 10))

plot.total_forez

ggsave(plot.total_forez, filename = "Labo/Forez - Conférence rieuse/total_forez_LR.png",
       dpi = 600, width = 18, height = 10, units = "cm")



### > Nombre de colonies ----
colonies_forez <- colonies |> 
  # Vérification que les colonies n'apparaissent qu'une fois par année:
  group_by(annee_observation_colonie, id_site_mesure) |> 
  summarise() |> 
  # Nombre de colonies par année:
  group_by(annee_observation_colonie) |> 
  summarise(n_colonies = n())

plot.colonies_forez <- ggplot() +
  geom_line(data = colonies_forez, aes(x = annee_observation_colonie, y = n_colonies)) +
  geom_point(data = colonies_forez, aes(x = annee_observation_colonie, y = n_colonies)) +
  scale_x_continuous(breaks = 1970+1:10*5) +
  scale_y_continuous(limits = c(0, max(colonies_forez$n_colonies)),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Year", y = "Number of occupied ponds ",
       title = "") +
  theme_classic()

plot.colonies_forez

ggsave(plot.colonies_forez, filename = "Labo/Forez - Conférence rieuse/colonies_forez_LR.png",
       dpi = 600, width = 14, height = 10, units = "cm")





####################################*###########################################
## Colonies majeures & AE ------------------------------------------------------
id_grandes_colonies <- c("LR_E", "MA_E", "PC_E", "V5_E", "VS_E", "LV_E", 
                         "LA_E", "SV_E", "HV_E", "WV_E", "SZ_E")

grandes_colonies <- colonies |> 
  # Regroupement des colonies "mineures" en Alive Elswhere (AE):
  mutate(id_site_mesure = ifelse(id_site_mesure %in% id_grandes_colonies, id_site_mesure, "AE_E")) |> 
  # Fusion de LV-VS et SV-HV:
  mutate(id_site_mesure = ifelse(id_site_mesure == "VS_E", "LV_E", id_site_mesure),
         id_site_mesure = ifelse(id_site_mesure == "HV_E", "SV_E", id_site_mesure)) |> 
  group_by(annee_observation_colonie, id_site_mesure) |> 
  summarise(nombre_couple_colonie = sum(nombre_couple_colonie)) |> 
  # Ajout des 0:
  pivot_wider(names_from = annee_observation_colonie, values_from = nombre_couple_colonie) |> 
  pivot_longer(!id_site_mesure, names_to = "annee_observation_colonie", values_to = "nombre_couple_colonie") |> 
  replace_na(list(nombre_couple_colonie = 0)) |> 
  # Ajout du nom de l'étang:
  left_join(sites |> 
              filter(id_site_mesure %in% id_grandes_colonies) |> 
              add_row(id_site_mesure = "AE_E", nom_site_mesure = "Autres étangs") |> 
              group_by(id_site_mesure, nom_site_mesure) |> 
              summarise() |> 
              mutate(nom_site_mesure = gsub(".*_", "", nom_site_mesure),
                     nom_site_mesure = ifelse(id_site_mesure == "SZ_E", "La Sauzee", nom_site_mesure)))


### > All ----
plot.grandes_colonies <- ggplot() +
  geom_line(data = grandes_colonies, aes(x = annee_observation_colonie, y = nombre_couple_colonie, 
                                         group = nom_site_mesure, colour = nom_site_mesure)) +
  coord_cartesian(ylim = c(0, 2000)) +
  theme_classic()

plot.grandes_colonies



### > Panels ----
plot.colonies_panel <- function(etang){
  if(etang == "LR_E" | etang == "AE_E"){
    y_breaks <- 0:5*1000
  }else{
    y_breaks <- 0:4*400
  }
  
  nom_site <- unique(grandes_colonies$nom_site_mesure[grandes_colonies$id_site_mesure == etang])

  panel.grandes_colonies <- grandes_colonies |> 
    mutate(focus = ifelse(id_site_mesure == etang, etang, "Autres"),
           focus = factor(focus, levels = c(etang, "Autres")),
           annee_observation_colonie = as.integer(annee_observation_colonie)) |> 
    ggplot() +
    geom_line(aes(x = annee_observation_colonie, y = nombre_couple_colonie, 
                  group = id_site_mesure, colour = focus, linewidth = focus, alpha = focus),
              show.legend = F) +
    scale_colour_manual(values = c("navyblue", "black")) +
    scale_linewidth_manual(values = c(1.5, 1)) +
    scale_alpha_manual(values = c(1, 0.2)) +
    scale_x_continuous(breaks = 1970+1:10*5) +
    scale_y_continuous(breaks = y_breaks,
                       expand = expansion(mult = c(0, 0.05))) +
    coord_cartesian(ylim = c(0, max(y_breaks))) +
    labs(x = "", y = "",
         # title = "Nombre de couples dans la plaine du Forez",
         subtitle = nom_site,
         colour = "", linewidth = "", alpha = "") +
    theme_classic() +
    theme(legend.text = element_text(size = 10),
          plot.subtitle = element_text(margin = margin(t = 0, b = -10), hjust = 0.02))
  
  return(panel.grandes_colonies)
}

plot.grandes_colonies <-  (plot.colonies_panel("LR_E") | plot.colonies_panel("AE_E")) /
  (plot.colonies_panel("MA_E") + plot.colonies_panel("PC_E")) /
  (plot.colonies_panel("V5_E") + plot.colonies_panel("LV_E")) /
  (plot.colonies_panel("LA_E") + plot.colonies_panel("SV_E")) /
  (plot.colonies_panel("WV_E") + plot.colonies_panel("SZ_E")) +
  plot_annotation(title = "Nombre de couples dans la plaine du Forez")

plot.grandes_colonies

ggsave(plot.grandes_colonies, filename = "Labo/Forez - Conférence rieuse/grandes_colonies.png",
       dpi = 600, width = 20, height = 25, units = "cm")


grandes_colonies <- grandes_colonies |> 
  filter(!id_site_mesure %in% c("LR_E", "AE_E", "LA_E", "V5_E")) |> 
  mutate(nom_site_mesure = ifelse(nom_site_mesure == "Sury", "Sury & Helene", nom_site_mesure))

plot.grandes_colonies <-
  (plot.colonies_panel("MA_E") + plot.colonies_panel("PC_E") + plot.colonies_panel("LV_E")) /
  (plot.colonies_panel("SV_E") + plot.colonies_panel("WV_E") + plot.colonies_panel("SZ_E")) +
  plot_annotation(title = "Nombre de couples dans la plaine du Forez")

plot.grandes_colonies

ggsave(plot.grandes_colonies, filename = "Labo/Forez - Conférence rieuse/grandes_colonies2.png",
       dpi = 600, width = 30, height = 10, units = "cm")


####################################*###########################################
## Colonisation/Extinction dernière version ------------------------------------

### > Sorties de modèle ----
type = "JAGS"
model = paste0("SrepnoF_assec1", 1:3)   # Rhat < 1.1, neff OK for w.gam.sigma /!\


for (mo in model){
  load(paste0("1- Occ multi echelles Charlotte/Output/cr_occ_final-", type, "-", mo, ".rda"))
  
  chain_name = paste0("chain", which(model == mo))
  
  if(type == "JAGS"){
    assign(chain_name, cr_occ$samples[[1]])
  }else if (type == "Nimble"){
    assign(chain_name, cr_occ$samples)
  }
  rm(cr_occ)
}
cr_occ = list(samples = list(chain1 = chain1, chain2 = chain2, chain3 = chain3))

C1 = cr_occ$samples$chain1
C2 = cr_occ$samples$chain2
C3 = cr_occ$samples$chain3

fit1 = as.data.frame(rbind(C1, C2, C3))



### > Variations temporelles ----
Years = 1976:2019
t.predict = data.frame(year = Years[-1], t = 2:length(Years)) %>% 
  mutate(t.normalised = (t-mean(t))/sd(t))


## Predictions - persistence:
phi.t = fit1 %>% 
  select("b.phi.t") %>% 
  rownames_to_column("ID.row") %>% 
  mutate(ID.row = as.numeric(ID.row)) %>%
  crossing(t.predict) %>% 
  bind_cols(fit1 %>% 
              select(starts_with("rd.phi.t[")) %>% 
              pivot_longer(everything(), names_to = "param", values_to = "rd.phi.t")) %>% 
  mutate(phi.t = plogis(b.phi.t*t.normalised + rd.phi.t)) %>% 
  group_by(year) %>% 
  summarise(med.phi.t = median(phi.t),
            i95_inf.phi.t = quantile(phi.t, probs = 0.025),
            i95_sup.phi.t = quantile(phi.t, probs = 0.975)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = 1-med.phi.t)) +
  geom_point(aes(x = year, y = 1-med.phi.t), colour = "white", size = 5) +
  geom_point(aes(x = year, y = 1-med.phi.t), colour = "black", size = 2) +
  geom_ribbon(aes(x = year, ymin = 1-i95_inf.phi.t, ymax = 1-i95_sup.phi.t), alpha = 0.2) +
  labs(x = "", y = "Probabilité\nd'Abandon") +
  coord_cartesian(ylim = c(0,1)) +
  theme_classic() +
  theme(plot.margin = unit(c(0, 0, 0, 0), units = "cm"))


## Predictions - colonisation:
gam.t = fit1 %>% 
  select("b.gam.t") %>% 
  rownames_to_column("ID.row") %>% 
  mutate(ID.row = as.numeric(ID.row)) %>%
  crossing(t.predict) %>% 
  bind_cols(fit1 %>% 
              select(starts_with("rd.gam.t[")) %>% 
              pivot_longer(everything(), names_to = "param", values_to = "rd.gam.t")) %>% 
  mutate(gam.t = plogis(b.gam.t*t.normalised + rd.gam.t)) %>% 
  group_by(year) %>% 
  summarise(med.gam.t = median(gam.t),
            i95_inf.gam.t = quantile(gam.t, probs = 0.025),
            i95_sup.gam.t = quantile(gam.t, probs = 0.975)) %>% 
  ggplot() +
  geom_line(aes(x = year, y = med.gam.t)) +
  geom_point(aes(x = year, y = med.gam.t), colour = "white", size = 5) +
  geom_point(aes(x = year, y = med.gam.t), colour = "black", size = 2) +
  geom_ribbon(aes(x = year, ymin = i95_inf.gam.t, ymax = i95_sup.gam.t), alpha = 0.2) +
  labs(x = "", y = "Probabilité\nde Colonisation") +
  coord_cartesian(ylim = c(0,1)) +
  theme_classic() +
  theme(plot.margin = unit(c(0, 0, 0, 0), units = "cm"))


## Plot:
t.plot = phi.t / gam.t

t.plot

ggsave(plot = t.plot, filename = "Labo/Forez - Conférence rieuse/abandon_colonisation_time.png",
       dpi = 600, width = 14, height = 12, units = "cm")





####################################*###########################################
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
         annee_observation_colonie = as.integer(annee_observation_colonie))

occupied_colonies <- abund_df |> 
  group_by(id_site_mesure, longitude_site_mesure, latitude_site_mesure) |> 
  summarise()

lims = data.frame(X = c(4.03, 4.30), Y = c(45.47, 45.88))

# Map background:
world = OpenStreetMap::openmap(upperLeft = c(lims$Y[1], lims$X[1]), 
                               lowerRight = c(lims$Y[2], lims$X[2]), 
                               zoom = 13,
                               type = "esri-topo", mergeTiles = TRUE)
world = OpenStreetMap::openproj(world)

# Plot map:

# ani.options(animation.format = "png")
# saveGIF(movie.name = "Labo/Forez - Conférence rieuse/forez.gif"{
#   for (y in 2012:2023){
#     location_map = OpenStreetMap::autoplot.OpenStreetMap(world) +
#       geom_point(data = abund_df |> filter(annee_observation_colonie == y),
#                  aes(x = longitude_site_mesure, y = latitude_site_mesure,
#                      size = nombre_couple_colonie),
#                  fill = "darkred", shape = 21, alpha = 0.4) +
#       scale_size(breaks = c(100, 200, 500, 1000, 2000, 5000),
#                  limits = c(0,5000)) +
#       labs(title = "",
#            x = "Longitude", y = "Latitude", 
#            size = "Nombre de colonies")
#     
#     print(location_map)
#     ani.pause(interval)
#   }
# })

location_map = OpenStreetMap::autoplot.OpenStreetMap(world) +
  # geom_point(data = occupied_colonies,
  #            aes(x = longitude_site_mesure, y = latitude_site_mesure),
  #            shape = 1, size = 0.5, alpha = 0.4) +
  geom_point(data = abund_df,
             aes(x = longitude_site_mesure, y = latitude_site_mesure,
                 size = nombre_couple_colonie,
                 alpha = ifelse(nombre_couple_colonie == 0, 0, 0.6)),
             fill = "darkred", shape = 21) +
  scale_size_continuous(trans = "sqrt",
                        range = c(1,8),
                        breaks = c(10, 50, 100, 500, 1000, 5000)) +
  scale_alpha_continuous(guide = "none") +
  labs(title = "",
       x = "Longitude", y = "Latitude", 
       size = "Nombre de couples") +
  coord_cartesian(expand = F) +
  transition_time(annee_observation_colonie) +
  ease_aes("linear") +
  # enter_grow() +
  # exit_shrink() +
  labs(title = "Annee : {frame_time}")

animate(location_map, fps = 10)
animate(location_map, fps = 10, height = 20, width = 20, unit = "cm", res = 150)

anim_save("Labo/Forez - Conférence rieuse/dynamique_occupation_etangs.gif")
