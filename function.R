library(dplyr)
library(purrr)

# Count total NA
count_na_variable = function(patient,name_var){
  return(sum(is.na(patient[,name_var])))
}

where_na_variable = function (patient,name_var) {
  lunghezza= length(patient[,name_var])
  out = NULL
  for (i in 1:lunghezza) {
    if (is.na(patient[i,name_var]))
      out[i]="X"
    else
      out[i]="_"
  }
  return (out)
}

# Transform char in binary

transform_to_binary = function(dataset,name_var){
  n = count_na_variable(dataset,name_var)
  n_a = which(is.na(dataset[,name_var]))
  
  dataset[,name_var] = as.numeric(is.element(dataset[,name_var], c ("Yes", "yes","Male"))) 
  if(n!=0) {
    for(i in n_a)
      dataset[i,name_var] = NA
  }
  
  return(dataset)
}

codify_race = function(data){
  data$Race <- (as.numeric(data$Race))
  return(data)
  
}

codify_glaucoma = function(data){
  tmp = (!is.element(data$Type_of_glaucoma, c ("NTG", "POAG","CACG","NAG")))
  data$Type_of_glaucoma[tmp] = "POAG"
  exclude = levels(data$Type_of_glaucoma)[(which(table(as.factor((data$Type_of_glaucoma))) == 0))]
  data$Type_of_glaucoma = as.numeric(factor(data$Type_of_glaucoma,exclude = exclude))
  return(data)
  
}


determine_glaucoma = function (paziente){
  type = unique (paziente[,'Type_of_glaucoma'])
  return (as.vector(type))
}


visits_are_conseq = function (paziente) {
  lunghezza = length(paziente$visit2) 
  visit_wise = (paziente$visit2 == seq(0, by = 0.5, length.out = lunghezza))
  return (sum(visit_wise)==lunghezza)
}


interp_variable = function (variable) {
  lunghezza = length(variable)
  
  if (lunghezza>1 & sum(!is.na(variable))<2) {
    print(variable)
    stop ("Remove patient")
  }
  i=1
  while ( i <= lunghezza ) {
    if (is.na(variable[i])) {
      if (i!=lunghezza) {
        next_non_na = i+1
        while (is.na(variable[next_non_na]) & next_non_na < lunghezza) {
          next_non_na = next_non_na + 1
        }
      }
      if ( i == lunghezza ||
           (next_non_na == lunghezza & is.na(variable[next_non_na])) ) {
        # variable[(i-2):lunghezza] = approx (x= c(i-2,i-1), y=c(variable[i-2], variable[i-1]), 
        #                                     xout = seq(from=(i-2), to=lunghezza, by=1), 
        #                                     method = "linear")$y 
        delta_y = variable[i-1] - variable[i-2]
        for (j in 0: (lunghezza-i)) {
          variable[(i+j)] = variable[(i+j-1)] + delta_y
        }
      }
      else {
        if (i == 1) {
          delta_y = variable[next_non_na+1] - variable[next_non_na]
          for (j in (next_non_na-1): i) {
            variable[j] = variable[j+1] + delta_y
          }
        }
        else {
          variable[(i-1):(next_non_na)] = approx (c(variable[(i-1)], variable[next_non_na]),  
                                                  method = "linear", n= (next_non_na-i+2))$y
        }
      }
      if (i != lunghezza) {
        i = next_non_na
      }
    }
    i =i+1
  }
  return(variable)
}

interpolation = function (patient, nomi) {
  for (i in 1 : length(nomi)) {
    interpolated_variable = interp_variable(patient[,nomi[i]])
    patient[,nomi[i]] = interpolated_variable
  }
  return (patient)
}

regress_variable = function (variable, plot_graph = FALSE) {
  if (sum(is.na(variable)) != 0) {
    idx_missing = which(is.na(variable))
    idx_valid = which(!is.na(variable))
    
    y = as.numeric(na.omit(variable))
    x = idx_valid
    reg_line = lm (y ~ x)
    fitted_values = predict.lm (reg_line, newdata = data.frame(x = idx_missing))
    variable[idx_missing]= fitted_values
    fit_everywhere = predict.lm (reg_line, newdata = data.frame(x = 1:length(variable)))
    
    if (plot_graph) {
      quartz()
      plot (idx_valid, variable[idx_valid], pch = 16, main = "NA imputation", xlab = "Index", ylab = "HIRW")
      lines(variable, col='black', lty =2)
      points(idx_missing, variable[idx_missing], col='red', pch =4)
      lines(fit_everywhere, col="red")
    }
  }
  
  return (variable)
}

regression = function (patient, nomi) {
  for (i in 1 : length(nomi)) {
    reg_variable = regress_variable(patient[,nomi[i]])
    patient[,nomi[i]] = reg_variable
  }
  return (patient)
}

# Debugging: paziente=lista_pazienti[[14]]
insert_na = function(paziente){
  index = paziente$visit2 
  lunghezza = length(index)
  max = max(paziente$visit2)
  sequence = seq(0,max, by = 0.5)
  buchi = which(is.element(sequence, index) == FALSE)
  # fix_col = paziente[1,2:4]
  if (length(buchi) == 0){
    return(paziente)
  }
  else{
    paziente_completo = as.data.frame(matrix(nrow = length(sequence), ncol = dim(paziente)[2]))
    j = 1
    for(i in 1:(max*2+1)){
      if(!is.element(i, buchi)){
        paziente_completo[i,] = as.data.frame(paziente[j,])
        j = j+1
      }
      else{
        paziente_completo[i,] = rep(NA,dim(paziente)[2] )
      }
    }
    # paziente_completo[,2:4] = rep(fix_col,max*2+1)
    names(paziente_completo) = names(paziente)
    return (as.data.frame(paziente_completo))
  }
  
}





##################################################################################

nr_non_na = function (patient, variable) {
  return (sum(!is.na(patient[,variable])))
}

find_pat_to_delete = function (patient, pat_to_delete) {
  return (is.element(patient$Patient[1], pat_to_delete))
}

copying = function (patient, nomi){
  for (i in 1 : length(nomi)) {
    copied_variable = copying_variable(patient[,nomi[i]])
    patient[,nomi[i]] = copied_variable
  }
  return (patient)
}

copying_variable = function(variable) {
  elem = variable[1]
  i=1
  while (is.na(elem)) {
    i = i+1
    elem = variable[i]
  }
  variable = rep(elem, length(variable))
  return (variable)
}

fix_year_gl = function (patient){
  lag = seq (from = 0, by = 0.5, length.out = dim(patient)[1])
  patient$yearofglaucoma = patient$yearofglaucoma[1] + lag 
  return (patient)
}


extract_variable = function (Patient, nome_variabile) {
  variable = dplyr::select(Patient, nome_variabile)
  return (variable)
}

create_dataset = function (Patient, nome_risposta, nomi_covariate_X, nomi_covariate_Z=NULL) {
  Y = dplyr::select(Patient, nome_risposta)
  X = dplyr::select(Patient, nomi_covariate_X)
  if (!is.null(nomi_covariate_Z)) {
    Z = NULL
    for (i in 1:length(nomi_covariate_Z)) {
      if (nomi_covariate_Z[i]=="Onset_age") {
        Z = c(Z, Patient$Age[1] - Patient$yearofglaucoma[1]) 
      }
      else if (nomi_covariate_Z[i]=="mean_OPP") {
        Z = c(Z, mean(Patient$OPP)) 
      }
      else if (nomi_covariate_Z[i]=="mean_VIRAV"){
        Z = c(Z, mean(Patient$Vert_integrated_rim_area__vol_)) 
      }
      else if (nomi_covariate_Z[i]=="mean_MD"){
        Z = c(Z, mean(Patient$MD)) 
      }
      else if (nomi_covariate_Z[i]=="family_history") {
        Z = c(Z, Patient$family_history[1]) 
      }
      else {
        stop("Covariata non riconosciuta")
      }
    }
    return (list (X = X, Y = Y, Z = Z))
  }
  return (list (X = X, Y = Y))
}

diff_nr_vis = function(new_pat, pat) {
  return (dim(new_pat)[1]-dim(pat)[1])
}

