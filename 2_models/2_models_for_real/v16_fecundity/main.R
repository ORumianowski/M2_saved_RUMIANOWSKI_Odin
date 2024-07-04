


# Information sur le fichier ----------------------------------------------

# Le fichier appelle les données nécessaires au modèle pour chaque période et applique le modèle. 
# Il produit des résultats indépendamment pour chaque période.

# -------------------------------------------------------------------------



library(jagsUI)
library(tidyverse)


# Dataset -----------------------------------------------------------------

setwd("C:/Users/33763/Desktop/Odin_RUMIANOWSKI_M2_saved/modele_du_rapport")


# Period 1 ----------------------------------------------------------------

load(file = "1_formatage/real_dataset/period_1234/B_p1_2605.Rda")
load(file = "1_formatage/real_dataset/period_1234/CH_p1_2605.Rda")
load(file = "1_formatage/real_dataset/period_1234/marr_p1_2605.Rda")
load(file = "1_formatage/real_dataset/period_1234/fecundity_1_4.Rda")

# load(file = "/lustre/rumianowskio/real_dataset/period_123/B_p2_1605.Rda")
# load(file = "/lustre/rumianowskio/real_dataset/period_123/CH_p2_1805.Rda")
# load(file = "/lustre/rumianowskio/real_dataset/period_123/marr_p2_1805.Rda")

source("2_models/2_models_for_real/v16_fecundity/preprocess.R")

out1 <- jags(jags.data, inits, parameters, "2_models/2_models_for_real/v16_fecundity/models/v16.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)


save(out1, file = "/lustre/rumianowskio/out1_v16_p1_4.Rda")

# Period 2 ----------------------------------------------------------------

load(file = "1_formatage/real_dataset/period_1234/B_p2_4_2705.Rda")
load(file = "1_formatage/real_dataset/period_1234/CH_p2_4_2705.Rda")
load(file = "1_formatage/real_dataset/period_1234/marr_p2_4_2705.Rda")
load(file = "1_formatage/real_dataset/period_1234/fecundity_2_4.Rda")

# load(file = "/lustre/rumianowskio/real_dataset/period_123/B_p2_1605.Rda")
# load(file = "/lustre/rumianowskio/real_dataset/period_123/CH_p2_1805.Rda")
# load(file = "/lustre/rumianowskio/real_dataset/period_123/marr_p2_1805.Rda")

source("2_models/2_models_for_real/v16_fecundity/preprocess.R")

out1 <- jags(jags.data, inits, parameters, "2_models/2_models_for_real/v16_fecundity/models/v16.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)


save(out1, file = "/lustre/rumianowskio/out1_v16_p2_4.Rda")
# Period 3 ----------------------------------------------------------------

load(file = "1_formatage/real_dataset/period_1234/B_p3_4_2805.Rda")
load(file = "1_formatage/real_dataset/period_1234/CH_p3_4_2805.Rda")
load(file = "1_formatage/real_dataset/period_1234/marr_p3_4_2805.Rda")
load(file = "1_formatage/real_dataset/period_1234/fecundity_3_4.Rda")

# load(file = "/lustre/rumianowskio/real_dataset/period_123/B_p2_1605.Rda")
# load(file = "/lustre/rumianowskio/real_dataset/period_123/CH_p2_1805.Rda")
# load(file = "/lustre/rumianowskio/real_dataset/period_123/marr_p2_1805.Rda")

source("2_models/2_models_for_real/v16_fecundity/preprocess.R")

out1 <- jags(jags.data, inits, parameters, "2_models/2_models_for_real/v16_fecundity/models/v16.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)


save(out1, file = "/lustre/rumianowskio/out1_v16_p3_4.Rda")

# Period 4 ----------------------------------------------------------------


load(file = "1_formatage/real_dataset/period_1234/B_p4_4_2805.Rda")
load(file = "1_formatage/real_dataset/period_1234/CH_p4_4_2805.Rda")
load(file = "1_formatage/real_dataset/period_1234/marr_p4_4_2805.Rda")
load(file = "1_formatage/real_dataset/period_1234/fecundity_4_4.Rda")

# load(file = "/lustre/rumianowskio/real_dataset/period_123/B_p2_1605.Rda")
# load(file = "/lustre/rumianowskio/real_dataset/period_123/CH_p2_1805.Rda")
# load(file = "/lustre/rumianowskio/real_dataset/period_123/marr_p2_1805.Rda")

source("2_models/2_models_for_real/v16_fecundity/preprocess.R")

out1 <- jags(jags.data, inits, parameters, "2_models/2_models_for_real/v16_fecundity/models/v16.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)


save(out1, file = "/lustre/rumianowskio/out1_v16_p4_4.Rda")





