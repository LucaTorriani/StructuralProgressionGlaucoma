library(rjags)
load("interp_lista_pazienti.Rdata")
source('function.R')

# MODEL 8 in chapter for Springer book 


###########################################
################## STEP 2 #################
###########################################
get_time = function(patient, time){
  return(patient[,time])
}


fix_time = function (time_vector) {
  for (i in 1: length(time_vector)) {
    if (is.na(time_vector[i])) {
      time_vector[i] = time_vector[i-1]+0.5 
    }
  }
  return (time_vector)
}

time_list = map(interp_lista_pazienti, get_time, "visit2")

time = NULL
for(i in 1:length(time_list)){
  time = c(time, time_list[[i]])
}

time = fix_time(time)



nome_risposta = "MD"
nomi_covariate_X = c("Horz_integrated_rim_width__area_", "IOP")

nomi_covariate_Z = NULL
dataset = map (interp_lista_pazienti, create_dataset, nome_risposta=nome_risposta, nomi_covariate_X=nomi_covariate_X,
               nomi_covariate_Z=nomi_covariate_Z)


X = NULL
Y = NULL
# Z = NULL
for (i in 1: length(dataset)) {
  X = rbind (X, as.data.frame(dataset[[i]]$X)) 
  Y = c (Y, as.numeric(as.matrix(dataset[[i]]$Y)))
  # Z = rbind (Z, dataset[[i]]$Z)
}
X = cbind (X, time = time)
# X is a matrix (sum(ni)*p) con i =1...N
# Y is a vector (sum(ni)) con i =1...N
# Z is a matrix (N*q)

X_numeric = dplyr::select(X,c("time", "Horz_integrated_rim_width__area_", "IOP"))
X_numeric = scale(X_numeric)
X = data.frame(X_numeric)

# Z_numeric = Z
# Z_numeric = scale (Z_numeric)
# Z = cbind(Z_numeric)


# General
N = length(interp_lista_pazienti) # Number of patients
n = as.numeric(map_dbl (interp_lista_pazienti, nrow)) # Vector of the number of observations per patients
n_cum = c(0, cumsum(n))

p = 4 # (intercept included)


# Beta
mu0 = rep(0, p);
SigmaInv0 = 0.01 * diag(p); 

# Tau
alpha0 = 2.01
lambda0 = 1.01

data = list( time = X$time, Horz_integrated_rim_width__area_ = X$Horz_integrated_rim_width__area_,
           IOP = X$IOP,
             Y = Y, N = N, n_cum= n_cum, mu0 = mu0,
             SigmaInv0 = SigmaInv0, alpha0 = alpha0, lambda0 = lambda0)

beta_init = rep (0, p)
inits = function() {list(beta = beta_init)} 
model = jags.model ("Model_MD.bug", data = data, inits = inits, n.adapt = 1000, n.chains = 1)
update (model, n.iter = 22000)
# save(model, file = "model.dat")
# load("model.dat")
variable.names = c("beta")

library(coda)
n.iter = 100000
thin = 25
output_modelMD = coda.samples(model = model, variable.names = variable.names, n.iter = n.iter, thin = thin)
save(output_modelMD, file = "output_modelMD.Rdata")
