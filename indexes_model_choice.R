load("output_modelHIRWA.Rdata") 
#load("output_modelMD.Rdata") 

output = output_modelHIRWA

library(coda)
data= as.matrix(output)
data1= as.data.frame(data)

nomi = names(data1)
length(nomi)

tau = data[, which(nomi == "tau")] 
mu_matrix = as.matrix(data[,which(grepl("^mu.",nomi))])

load("interp_lista_pazienti.Rdata")
source("function.R")
response_variable = map (interp_lista_pazienti, extract_variable, "Horz_integrated_rim_width__area_")
Y = NULL
for (i in 1: length(response_variable)) {
  Y = c (Y, as.numeric(as.matrix(response_variable[[i]])))
}
# Y = vettore risposta

# sum_ln_up = function(A,B){
#     return(log(A)+log(B))
# }

# sum_up = function(A,B){
#   return(A+B)
# }


############
### LPML ###
############

compute_CPO_i = function(mu_vector_iter, y_i, tau_vector_iter){ # mu per lo stesso paziente al variare delle iterazioni
  tmp = 1/dnorm(y_i, mu_vector_iter, 1/sqrt(tau_vector_iter))
  CPO_i = 1/(mean(tmp))
  return(CPO_i)
}


compute_LPML = function(Y, mu_matrix, tau){
  mu_list = split(mu_matrix, rep(1:ncol(mu_matrix), each = nrow(mu_matrix)))
  Y_list = as.list(Y)
  CPO_collection = map2(mu_list, Y_list,compute_CPO_i, tau)
  
  CPO_vector = unlist(CPO_collection)
  log_CPO_vector = log(CPO_vector)
  LPML = sum (log_CPO_vector)
  # LPML = reduce(CPO_collection, sum_ln_up)
  return (LPML)
}


LPML = compute_LPML(Y, mu_matrix, tau)

############
### WAIC ###
############

compute_mean_likelihood_i = function(mu_vector_iter, y_i, tau_vector_iter){ # mu per lo stesso paziente al variare delle iterazioni
  tmp = dnorm(y_i, mu_vector_iter, 1/sqrt(tau_vector_iter))
  mean_like = (mean(tmp))
  return(mean_like)
}

compute_lppd = function(Y, mu_matrix, tau){
  mu_list = split(mu_matrix, rep(1:ncol(mu_matrix), each = nrow(mu_matrix)))
  Y_list = as.list(Y)
  mean_likelihood_collection = map2(mu_list, Y_list,compute_mean_likelihood_i, tau)
  
  mean_likelihood_vector = unlist(mean_likelihood_collection)
  log_mean_likelihood_vector = log(mean_likelihood_vector)
  lppd = sum (log_mean_likelihood_vector)
  # lppd = reduce(mean_likelihood_collection, sum_ln_up)
  return(lppd)
}

difference_ll_mean = function(mu_vector_iter, y_i, tau_vector_iter){
  log_like = log(dnorm(y_i, mu_vector_iter, 1/sqrt(tau_vector_iter)))
  media = mean(log_like)
  diff_square = (log_like - media)^2
  return(sum(diff_square)/(length(diff_square)-1))
}

compute_WAIC = function(Y, mu_matrix, tau){
  mu_list = split(mu_matrix, rep(1:ncol(mu_matrix), each = nrow(mu_matrix)))
  Y_list = as.list(Y)
  lppd = compute_lppd(Y, mu_matrix, tau)
  
  pwaic = sum(unlist(map2(mu_list, Y_list,difference_ll_mean, tau)))
  # pwaic = reduce(map2(mu_list, Y_list,difference_ll_mean, tau), sum_up)
  return (lppd-pwaic)
}

WAIC = compute_WAIC(Y, mu_matrix, tau)


