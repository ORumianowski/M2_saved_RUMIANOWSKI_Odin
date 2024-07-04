

# Information sur le fichier ----------------------------------------------

# Préparation des variables d'entrées au modèle pour Jags. Préparation identique à chaque période.



# Bundle data  ------------------------------------------------------------

n.colony = nrow(B) # number of colonies in this survey
n.years = ncol(B)  # number of years in this survey

n.age.class = 3 # number of age classes in this model

survey_data = B # survey dataset - number of breeders reported 
rel = rowSums(marr) # number of released birds at each occasions

ns=n.colony*n.age.class # number of state in this model 
nest.states = (1:n.colony)+0*n.colony # nestling states 
breed.states = (1:n.colony)+1*n.colony # breeder state
prebr.states = (1:n.colony)+2*n.colony # prebreeder state

col2colclass = c(1, rep(2, (n.colony-1))) # correspondence from colony to colony type i.e. La Ronze ou satellite colony
state2col = rep(1:n.colony, time = n.age.class) # correspondence from state to colony number
state2colclass = rep(col2colclass, n.age.class) # correspondence from state to colony type i.e. La Ronze ou satellite colony
state2ageclass = rep(1:n.age.class, each = n.colony) # correspondence from state to age class 


# Numbers of prebreeders and breeders for the priors, the first year
repro_ratio = c(0.3/(1-0.3), rep(0.2/(1-0.2),n.colony-1))  # Age ratio estimated with a formula from Kery
prior_incert = 0.80 # Uncertainty about first-year numbers

pop_init = data.frame(estim = B[,1]) %>%
  mutate(B_inf = estim*0.2)%>%
  mutate(B_sup = estim*1.8+1)%>%
  mutate(N_inf = estim*(repro_ratio)*0.2)%>%
  mutate(N_sup = estim*(repro_ratio)*1.8+1) %>%
  round() %>%
  as.matrix()


# Dirichlet prior parameters for dispersion
dirich_param <- rep(1, n.colony)

# Where (row) and when (column) are no reproduction ?
E = ifelse(survey_data == 0, 0, 1) %>%
  replace(is.na(.), 1)


# Reading effort
reading_effort = CH %>% 
  as_tibble() %>% 
  pivot_longer(everything()) %>% 
  mutate(value = as.factor(value)) %>% 
  group_by(name, value) %>% 
  count() %>% 
  mutate(name = as.numeric(stringr::str_extract(name, "\\d+"))) %>% 
  arrange(name) %>% 
  pivot_wider(names_from = name, values_from = n) %>% 
  arrange(., value)

reading_effort = reading_effort %>%
  ungroup() %>% 
  slice(c((n.colony+2):(2*n.colony))) %>% 
  select(-1) %>% 
  as.matrix()

# The probability of recapture is set at 0 in years when there has been no observation pressure for ring readings
# Detection matrix
detection = ifelse(is.na(reading_effort), 0, 1)%>%
  as.data.frame() %>% 
  select(2:ncol(.))  # no detection on first year

# All indices
all_col_year = cbind(rep((n.colony+1):(2*n.colony), each = (n.years-1)), rep(1:(n.years-1), time = n.colony)) %>% as.data.frame()
colnames(all_col_year) = c("state","year")

# Indices with detection
detection_index = which(detection == 1, arr.ind = TRUE) %>% as.data.frame()
colnames(detection_index) = c("state", "year")
detection_index[,c("state")] = detection_index[,c("state")] + n.colony
nrow_detection_index = nrow(detection_index)

# Indices without detection
no_detection_index = anti_join(all_col_year, detection_index, by = c("state","year"))
nrow_no_detection_index = nrow(no_detection_index)

# Conversion to matrix
detection_index = detection_index %>% as.matrix()
no_detection_index = no_detection_index %>% as.matrix()


# Active fecundity

active_fecundity  = fecundity

# 0 : no reproduction
# 1 : some chicks observed
# 2 : no data


# Built list of data and constant for jags
jags.data <- list(C=survey_data,
                  marr = marr, rel=rel,
                  
                  ns=ns,
                  zero=matrix(0, ncol=ns, nrow=ns),
                  ones=diag(ns),
                  
                  n.colony=n.colony,
                  n.years=n.years,
                  pop_init=pop_init,
                  dirich_param=dirich_param,
                  
                  E=E,
                  active_fecundity = active_fecundity,
                  
                  detection_index = detection_index,
                  nrow_detection_index = nrow_detection_index,
                  
                  no_detection_index = no_detection_index,
                  nrow_no_detection_index = nrow_no_detection_index,
                  
                  nest.states = nest.states,
                  breed.states = breed.states,
                  prebr.states = prebr.states,
                  col2colclass = col2colclass,
                  state2col = state2col,
                  state2ageclass = state2ageclass,
                  state2colclass = state2colclass) 




# Initial values
inits <- function(){
  return(list())
}

# Parameters monitored
parameters <- c("phi", "mean_phi", "sigma_phi",
                "natalfidelity", "breedingfidelity",
                "eta", "nu",
                "mean_p", "sigma_p",
                "rho",
                "rho_LR1","rho_LR2", "rho_SAT1", "rho_SAT2",
                "sigma",
                "po",
                "N", "B"
)

# MCMC settings
#ni <- 150000; nb <- 50000; nc <- 3; nt <- 100; na <- 3000
ni <- 60000; nb <- 10000; nc <- 3; nt <- 20; na <- 4000






