
library(jagsUI)
library(tidyverse)


# Dataset -----------------------------------------------------------------


setwd("C:/Users/33763/Desktop/Odin_RUMIANOWSKI_M2_saved/modele_du_rapport")


load(file = "1_formatage/real_dataset/period_1234/B_p3_4_2805.Rda")
load(file = "1_formatage/real_dataset/period_1234/CH_p3_4_2805.Rda")
load(file = "1_formatage/real_dataset/period_1234/marr_p3_4_2805.Rda")

# load(file = "/lustre/rumianowskio/real_dataset/period_123/B_p2_1605.Rda")
# load(file = "/lustre/rumianowskio/real_dataset/period_123/CH_p2_1805.Rda")
# load(file = "/lustre/rumianowskio/real_dataset/period_123/marr_p2_1805.Rda")


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



# Write JAGS model file
cat(file = "2_models/2_models_for_real/v15/models/v15_5.txt", "
model {
 # -------------------------------------------------
  # Stages:
  # N: not-yet recruited individuals
  # B: breeders
  # Parameters:
  # phi[age]: survival probability
  # eta_t[departure site, arrival site, year]: natal dispersal
  # nu_t[departure site, arrival site, year]: breeding dispersal
  # kappa: recruitment probability 
  # rho[site]: productivity - two classes (La Ronze or not)
  # p[site, year]: recapture probability 
  # -------------------------------------------------
  # Priors 
  
  # Productivity - La Ronze
  rho[1] ~ dunif(0, 1) 
  # Productivity - Satellite colonies
  rho[2] ~ dunif(0, 1) 

  # Recruitment
  kappa <- 1

  # Survival with its random effects parameters
  # Survival probability mean - Adult
  mean_phi[1] ~ dunif(0, 1)
  mu_phi[1] <- log(mean_phi[1] / (1 - mean_phi[1]))
  # Survival probability precision - Adult
  sigma_phi[1] ~ dunif(0.001, 1)
  tau_phi[1] <- (1 / (sigma_phi[1] * sigma_phi[1]))

  # Survival probability mean - Juvenile
  mean_phi[2] ~ dunif(0, 1)
  mu_phi[2] <- log(mean_phi[2] / (1 - mean_phi[2]))
  # Survival probability precision - Juvenile
  sigma_phi[2] ~ dunif(0.001, 1)
  tau_phi[2] <- (1 / (sigma_phi[2] * sigma_phi[2]))


  for (t in 1:(n.years)){

    lphi[1,t]  ~ dnorm(mu_phi[1], tau_phi[1])
    phi[1,t] <- (exp(lphi[1,t]) / (1 + exp(lphi[1,t])))

    lphi[2,t]  ~ dnorm(mu_phi[2], tau_phi[2])
    phi[2,t] <- (exp(lphi[2,t]) / (1 + exp(lphi[2,t])))
  }

  
  # eta : dispersion parameters which is defined with a prior 
  # eta_ext depends partly on time. It is set to eta by default and updated in the event of extinction
  # eta_t is the normalized and updated version of eta_ext - This is the one used in the process equation.
  
for (dep in 1:n.colony) {
    eta[dep, 1:n.colony]  ~ ddirch(dirich_param) # natal dispersal
    nu[dep, 1:n.colony]  ~ ddirch(dirich_param)  # breeding dispersal
  }

  for (t in 1:(n.years-1)){
    for (dep in 1:n.colony){
      for (arr in 1:n.colony){
          eta_ext[dep, arr, t] <-  eta[dep, arr]  * (1-equals(E[arr,(t+1)],0)) # The value is 0 if you are moving towards a colony that has disappeared
          nu_ext[dep, arr, t] <- nu[dep, arr] * (1-equals(E[arr,(t+1)],0)) # The value is 0 if you are moving towards a colony that has disappeared
      }
    }
  }
  
  for (t in 1:(n.years-1)){
    for (dep in 1:n.colony){
      for (arr in 1:n.colony){
          eta_t[dep, arr, t] <-  (eta_ext[dep, arr, t] / sum(eta_ext[dep, , t]))  
          nu_t[dep, arr, t] <- (nu_ext[dep, arr, t] / sum(nu_ext[dep, , t]))   
      }
    }
  }

  for (colony in 1:n.colony){
    natalfidelity[colony] <- eta[colony,colony]
    breedingfidelity[colony] <- nu[colony,colony]
  }
   
  # Population count data (state-space model)

  # Models for the initial population size: uniform priors
  for (col in 1:n.colony){
    B[col,1] ~ dunif(pop_init[col,2], pop_init[col,3])
    N[col,1] ~ dunif(pop_init[col,4], pop_init[col,5])
  }

   # Process model over time: our model of population dynamics
  for (t in 1:(n.years-1)){
    for (s in 1:n.colony){

    N[s,t+1] <- B[s,t] * rho[col2colclass[s]] * phi[1,t] +
                N[s,t] * phi[2,t] * sum(eta_t[s,1:n.colony,t] * (1-kappa))

    B[s,t+1] <- sum(N[1:n.colony,t] * phi[2,t] * eta_t[1:n.colony,s,t] * kappa ) +
                sum(B[1:n.colony,t] * phi[2,t] * nu_t[1:n.colony,s,t])
    }
  }

  # Residual (observation) error

  sigma[1] ~ dunif(0,0.3) # La Ronze
  sigma[2] ~ dunif(0,0.15) # Satellite colonies

  # Survey observation model
  for (t in 1:(n.years)){
    for (s in 1:n.colony){

    R[s,t] <- sigma[col2colclass[s]] * B[s,t]* (1-equals(E[s,t],0)) + 1E-9
    tau[s,t] <- pow(R[s,t], -2)

      C[s,t] ~ dnorm(B[s,t],tau[s,t])
    }
  }


  
  # Define state-transition - CMR multistate
  for (t in 1:(n.years-1)){
    # Nestling
    for (prev.state in nest.states){
      # nestlings may become pre breeders
      for (next_state in prebr.states){
        psi[prev.state,next_state,t] <- phi[1,t] * equals(state2col[next_state], state2col[prev.state]) 
      }
      # nestlings do not become nestling or breeders
      for (next_state in c(nest.states,breed.states)){
        psi[prev.state,next_state,t] <- 0
      }
    }
    # Breeders
    for (prev.state in breed.states){
      # Breeders may stay breeders
      for (next_state in breed.states){
        psi[prev.state,next_state,t] <- phi[2,t] * nu_t[state2col[prev.state],state2col[next_state],t]
      }
      # Breeders cannot become nestling or prebreeders
          for (next_state in c(nest.states, prebr.states)){
        psi[prev.state,next_state,t] <- 0
      }
    }
    # Pre-Breeders
    for (prev.state in prebr.states){
     # Prebreeders cannot become nestling
      for (next_state in nest.states){
        psi[prev.state,next_state,t] <- 0
      }
      
    # Prebreeders may become breeders
    for (next_state in breed.states){
        psi[prev.state,next_state,t] <- phi[2,t] * kappa * eta_t[state2col[prev.state],state2col[next_state],t]
    }
    
    # Prebreeders may stay prebreeders
    for (next_state in prebr.states){
          psi[prev.state,next_state,t] <- equals(state2col[next_state], state2col[prev.state]) 
          * phi[2,t] * sum(eta_t[state2col[prev.state],1:n.colony,t] * (1-kappa))
        
      }  
    }
  }
  

  # Detections with its random effects parameters
  for (col in 1:(n.colony-1)){

    # Detection probability mean
    mean_p[col] ~ dunif(0, 1)
    mup[col]<- log(mean_p[col] / (1 - mean_p[col]))

    # Detection probability precision
    sigma_p[col] ~ dunif(0.01, 100)
    taup[col] <- (1 / (sigma_p[col] * sigma_p[col]))
  }

  #  Define re-encounter probabilities
  for (t in 1:(n.years - 1)) {
  # Not detected states - Nestlings & Prebreeders
    for (state in c(nest.states, prebr.states)) {
      po[state, t] <- 0
      }
  }
  # Not Surveyed colonies/years
  for (k in 1:nrow_no_detection_index) {
    po[no_detection_index[k, 1] ,no_detection_index[k, 2]]  <-  0 
  }
  # Surveyed colonies/years - breeders
  for (k in 1:nrow_detection_index) {
    lpo[detection_index[k, 1], detection_index[k, 2]] ~ dnorm(mup[detection_index[k, 1]-n.colony], taup[detection_index[k, 1]-n.colony])
    po[detection_index[k, 1], detection_index[k, 2]] <- (exp(lpo[detection_index[k, 1], detection_index[k, 2]]) / (1 + exp(lpo[detection_index[k, 1], detection_index[k, 2]])))
  }


    # Calculate probability of non-encounter (dq) and reshape the array for the encounter
    # probabilities
  for (t in 1:(n.years-1)){  
    for (s in 1:ns){
      dp[s,t,s] <- po[s,t]
      dq[s,t,s] <- 1-po[s,t]
    } #s
    for (s in 1:(ns-1)){
      for (m in (s+1):ns){
        dp[s,t,m] <- 0
        dq[s,t,m] <- 0
      } #m
    } #s
    for (s in 2:ns){
      for (m in 1:(s-1)){
        dp[s,t,m] <- 0
        dq[s,t,m] <- 0
      } #m
    } #s
  } #t
  
  # Define the cell probabilities of the multistate m-array
    for (t in 1:(n.years-2)){
      U[(t-1)*ns+(1:ns), (t-1)*ns+(1:ns)] <- ones
      for (j in (t+1):(n.years-1)){
        U[(t-1)*ns+(1:ns), (j-1)*ns+(1:ns)] <- U[(t-1)*ns+(1:ns), (j-2)*ns+(1:ns)] %*% psi[,,j] %*% dq[,t,]
    } #j
  } #t
  
  U[(n.years-2)*ns+(1:ns), (n.years-2)*ns+(1:ns)] <- ones
  
  # Diagonal
  for (t in 1:(n.years-2)){
    pr[(t-1)*ns+(1:ns),(t-1)*ns+(1:ns)] <- U[(t-1)*ns+(1:ns),(t-1)*ns+(1:ns)] %*% psi[,,t] %*% dp[,t,]
  # Above main diagonal
  for (j in (t+1):(n.years-1)){
    pr[(t-1)*ns+(1:ns), (j-1)*ns+(1:ns)] <- U[(t-1)*ns+(1:ns), (j-1)*ns+(1:ns)] %*% psi[,,j] %*% dp[,j,]
    } #j
  } #t
  
  pr[(n.years-2)*ns+(1:ns), (n.years-2)*ns+(1:ns)] <- psi[,,n.years-1] %*% dp[,n.years-1,] 
  
  # Below main diagonal
  for (t in 2:(n.years-1)){
    for (j in 1:(t-1)){
      pr[(t-1)*ns+(1:ns),(j-1)*ns+(1:ns)] <- zero
    } #j
  } #t
  
  # Last column: probability of non-recapture
  for (t in 1:((n.years-1)*ns)){
    pr[t,(n.years*ns-(ns-1))] <- 1-sum(pr[t,1:((n.years-1)*ns)])
  } #t
    
  # Define the multinomial likelihood
  for (t in 1:((n.years-1)*ns)){
     marr[t,1:(n.years*ns-(ns-1))] ~ dmulti(pr[t,], rel[t])
  }
}
")


# Initial values
inits <- function(){
  return(list())
}

# Parameters monitored
parameters <- c("phi", "mean_phi", "sigma_phi",
                "natalfidelity", "breedingfidelity",
                "eta", "nu",
                "mean_p", "sigma_p",
                "rho", "sigma",
                "po",
                "N", "B"
)

# MCMC settings
#ni <- 150000; nb <- 50000; nc <- 3; nt <- 100; na <- 3000
ni <- 60000; nb <- 10000; nc <- 3; nt <- 20; na <- 4000


# Call JAGS from R and check convergence
out1 <- jags(jags.data, inits, parameters, "2_models/2_models_for_real/v15/models/v15_5.txt",
             n.iter=ni, n.burnin=nb, n.chains=nc, n.thin=nt, n.adapt=na,
             parallel=TRUE)


save(out1, file = "/lustre/rumianowskio/out1_p1_v15_5_p3_4.Rda")





