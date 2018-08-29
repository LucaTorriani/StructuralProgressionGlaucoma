library(rjags)
load("interp_lista_pazienti.Rdata")
source('function.R')

# MODEL 7 in chapter for Springer book 

###########################################
################## STEP 2 #################
###########################################

nome_risposta = "Horz_integrated_rim_width__area_"
nomi_covariate_X = c("Sex", "yearofglaucoma","family_history", "IOP", "Diabetes", "MD")

nomi_covariate_Z = c("mean_OPP")

dataset = map (interp_lista_pazienti, create_dataset, nome_risposta=nome_risposta, nomi_covariate_X=nomi_covariate_X,
               nomi_covariate_Z=nomi_covariate_Z)

## Fix age to baseline
get_first_Age = function(pat) {
  first_age = pat$Age[1]
  return (first_age)
}

unique_ages = map_dbl (interp_lista_pazienti, get_first_Age)
n_obs_pat = map_dbl (interp_lista_pazienti, nrow)

fix_age = function(pat){
  pat$X[,"Age"] = rep(pat$X[1,"Age"], dim(pat$X)[1])
  return(pat)
}

dataset = map(dataset, fix_age)

X = NULL
Y = NULL
Z = NULL
for (i in 1: length(dataset)) {
  X = rbind (X, as.data.frame(dataset[[i]]$X)) 
  Y = c (Y, as.numeric(as.matrix(dataset[[i]]$Y)))
  Z = rbind (Z, dataset[[i]]$Z)
}
# X is a matrix (sum(ni)*p) con i =1...N
# Y is a vector (sum(ni)) con i =1...N
# Z is a matrix (N*q)

X_binary = dplyr::select(X,c("Sex", "family_history", "Diabetes"))
X_numeric = dplyr::select(X,c("IOP", "MD","yearofglaucoma"))
age_unique = scale(unique_ages)
X_numeric = scale(X_numeric)
X = data.frame(X_numeric, Age = rep(age_unique, n_obs_pat), X_binary)

Z_numeric = Z[,1]
Z_numeric = scale (Z_numeric)
Z = cbind(Z_numeric)

# General
N = length(dataset) # Number of patients
n = as.numeric(map_dbl (interp_lista_pazienti, nrow)) # Vector of the number of observations per patient
n_cum = c(0, cumsum(n))

p = 8 # (intercept included)
q = 1 # (intercept included)
M = 40

# Beta
mu0 = rep(0, p);
SigmaInv0 = 0.01 * diag(p);

# Tau
alpha0 = 2.01
lambda0 = 1.01

# DP
alpha = 0.5

data = list(Sex = X$Sex, yearofglaucoma = X$yearofglaucoma,Diabetes = X$Diabetes, 
            family_history = X$family_history, IOP = X$IOP, MD = X$MD, 
            mean_OPP = Z[,1], age = X$Age, Y = Y, N = N, n_cum= n_cum,  M=M, mu0 = mu0,
            alpha0 = alpha0, lambda0 = lambda0, SigmaInv0 = SigmaInv0, alpha = alpha)

beta_init = rep (0, p)
inits = function() {list(beta = beta_init)} 
model = jags.model ("Model_HIRWA.bug", data = data, inits = inits, n.adapt = 1000, n.chains = 1)
update (model, n.iter = 22000)
# save(model, file = "model.dat")
# load("model.dat")
variable.names = c("beta", "b", "tau", "mu")

library(coda)
n.iter = 100000
thin = 25
output_modelHIRWA = coda.samples(model = model, variable.names = variable.names, n.iter = n.iter, thin = thin)
save(output_modelHIRWA, file = "output_modelHIRWA.Rdata")
