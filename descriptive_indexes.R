library(dplyr)
library(MASS)
library(readr)
library(purrr)

source('function.R')
load("interp_lista_pazienti.Rdata")
load("data108.RData")

final_patient_list = interp_lista_pazienti

# Helper function
totale_visite = function(lista_patient){
  totale = rep(0,12)
  for (i in 1:length(lista_patient)){
    temp = rep(0,12)
    temp[1:nrow(lista_patient[[i]])] = 1
    totale = totale + temp
  }
  return(totale)
}


## YOG
get_YoG = function(patient) {
  return (patient[1,"yearofglaucoma"])
}

YoG = map_dbl(final_patient_list, get_YoG)
YoG_mean = mean(YoG)
YoG_sd = sd(YoG)
mean_sd_YoG = cbind(YoG_mean, YoG_sd)

## SEX
get_sex = function(patient) {
  return (patient[1,"Sex"])
}

sex = map_dbl(final_patient_list, get_sex)
tab = table(sex) 
perc_F = tab/108 # 61.1% female
names(perc_F) = c("Female", "Male")

##  AGE at baseline
get_age = function(patient) {
  return (patient[1,"Age"])
}

age = map_dbl(final_patient_list, get_age)
age_mean = mean(age)
age_sd = sd(age)
age_table = cbind(age_mean, age_sd)

## Cardiovascular dz
get_card_dz = function(patient) {
  return (patient[1,"Cardiovascular_Dz"])
}

card_dz = map_dbl(final_patient_list, get_card_dz)
p_card_dz = mean(card_dz)
var_card_dz = p_card_dz*(1- p_card_dz)
card_dz = cbind(p_card_dz, var_card_dz)


## Diabetes
get_diabetes = function(patient) {
  return (patient[1,"Diabetes"])
}

diabetes = map_dbl(final_patient_list, get_diabetes)
p_diabetes = mean(diabetes)
var_diabetes = p_diabetes*(1- p_diabetes)
diabetes_table = cbind(p_diabetes, var_diabetes)

## family_history
get_family_history = function(patient) {
  return (patient[1,"family_history"])
}

family_history = map_dbl(final_patient_list, get_family_history)
p_family_history = mean(family_history)
var_family_history = p_family_history*(1- p_family_history)
fh_table = cbind(p_family_history, var_family_history)

## Hypertension
get_Hypertension = function(patient) {
  return (patient[1,"Hypertension"])
}

Hypertension = map_dbl(final_patient_list, get_Hypertension)
p_Hypertension = mean(Hypertension)
var_Hypertension = p_Hypertension*(1- p_Hypertension)
Hypertension_table = cbind(p_Hypertension, var_Hypertension)

## Race
get_Race = function(patient) {
  return (patient[1,"Race"])
}

Race = map_dbl(final_patient_list, get_Race)
race_table = table(Race)
names(race_table) = c("Asia","Black", "Hispanic", "White")


## RNFL

# Medione + sd
great_mean_RNFL = mean(na.omit(data108$mean_RNFL_thickness))
sd_RNFL = sd(na.omit(data108$mean_RNFL_thickness))
RNFL = cbind(great_mean_RNFL, sd_RNFL)

# Media per visite + sd 

compute_sd_mean_per_visit_RNFL = function(lista_patient){
  result = c()
  for(i in 1:12){
    value = c()
    for(j in 1:length(lista_patient)){
      if(i<= nrow(lista_patient[[j]]) ){
        value = c(value,lista_patient[[j]]$mean_RNFL_thickness[i])  
      }
    }
    result = rbind(result, c(mean(na.omit(value)),sd(na.omit(value))))
  }
  return(result)
}

mean_sd_RNFL = compute_sd_mean_per_visit_RNFL(final_patient_list)
colnames(mean_sd_RNFL) = c("mean_rnfl", "sd_RNFL")



## MD

# Medione + sd
great_mean_MD = mean(na.omit(data108$MD))
sd_MD = sd(na.omit(data108$MD))
MD = cbind(great_mean_MD, sd_MD)

# Media per visite + sd 
# Helper function
compute_sd_mean_per_visit_MD = function(lista_patient){
  result = c()
  for(i in 1:12){
    value = c()
    for(j in 1:length(lista_patient)){
      if(i<= nrow(lista_patient[[j]]) ){
        value = c(value,lista_patient[[j]]$MD[i])  
      }
    }
    result = rbind(result, c(mean(na.omit(value)),sd(na.omit(value))))
  }
  return(result)
}

mean_sd_MD = compute_sd_mean_per_visit_MD(final_patient_list)
colnames(mean_sd_MD) = c("mean_MD", "sd_MD")



## IOP

# Medione + sd
great_mean_IOP = mean(na.omit(data108$IOP))
sd_IOP = sd(na.omit(data108$IOP))
IOP = cbind(great_mean_IOP, sd_IOP)

# Media per visite + sd 

compute_sd_mean_per_visit_IOP = function(lista_patient){
  result = c()
  for(i in 1:12){
    value = c()
    for(j in 1:length(lista_patient)){
      if(i<= nrow(lista_patient[[j]]) ){
        value = c(value,lista_patient[[j]]$IOP[i])  
      }
    }
    result = rbind(result, c(mean(na.omit(value)),sd(na.omit(value))))
  }
  return(result)
}

mean_sd_IOP = compute_sd_mean_per_visit_IOP(final_patient_list)
colnames(mean_sd_IOP) = c("mean_IOP", "sd_IOP")


## MAP

# Medione + sd
great_mean_MAP = mean(na.omit(data108$MAP))
sd_MAP = sd(na.omit(data108$MAP))
MAP = cbind(great_mean_MAP, sd_MAP)

# Media per visite + sd 

compute_sd_mean_per_visit_MAP = function(lista_patient){
  result = c()
  for(i in 1:12){
    value = c()
    for(j in 1:length(lista_patient)){
      if(i<= nrow(lista_patient[[j]]) ){
        value = c(value,lista_patient[[j]]$MAP[i])  
      }
    }
    result = rbind(result, c(mean(na.omit(value)),sd(na.omit(value))))
  }
  return(result)
}

mean_sd_MAP = compute_sd_mean_per_visit_MAP(final_patient_list)
colnames(mean_sd_MAP) = c("mean_MAP", "sd_MAP")




## OPP

# Medione + sd
great_mean_OPP = mean(na.omit(data108$OPP))
sd_OPP = sd(na.omit(data108$OPP))
OPP = cbind(great_mean_OPP, sd_OPP)

# Media per visite + sd 

compute_sd_mean_per_visit_OPP = function(lista_patient){
  result = c()
  for(i in 1:12){
    value = c()
    for(j in 1:length(lista_patient)){
      if(i<= nrow(lista_patient[[j]]) ){
        value = c(value,lista_patient[[j]]$OPP[i])  
      }
    }
    result = rbind(result, c(mean(na.omit(value)),sd(na.omit(value))))
  }
  return(result)
}

mean_sd_OPP = compute_sd_mean_per_visit_OPP(final_patient_list)
colnames(mean_sd_OPP) = c("mean_OPP", "sd_OPP")



## HIRWA

# Medione + sd
great_mean_HIRWA = mean(na.omit(data108$Horz_integrated_rim_width__area_))
sd_HIRWA = sd(na.omit(data108$Horz_integrated_rim_width__area_))
HIRWA = cbind(great_mean_HIRWA, sd_HIRWA)

# Media per visite + sd 
compute_sd_mean_HIRWA = function(lista_patient){
  result = c()
  for(i in 1:12){
    value = c()
    
    for(j in 1:length(lista_patient)){
      if(i<= nrow(lista_patient[[j]]) ){
        value = c(value,lista_patient[[j]]$Horz_integrated_rim_width__area_[i])  
      }
    }
    result = rbind(result, c(mean(na.omit(value)),sd(na.omit(value))))
    
  }
  
  return(result)
}

mean_sd_HIRWA = compute_sd_mean_HIRWA(final_patient_list)
colnames(mean_sd_HIRWA) = c("mean_HIRWA", "sd_HIRWA")


## VIRA

# Medione + sd
great_mean_VIRA = mean(na.omit(data108$Vert_integrated_rim_area__vol_))
sd_VIRA = sd(na.omit(data108$Vert_integrated_rim_area__vol_))
VIRA = cbind(great_mean_VIRA, sd_VIRA)

# Media per visite + sd 
compute_sd_mean_VIRA = function(lista_patient){
  result = c()
  for(i in 1:12){
    value = c()
    
    for(j in 1:length(lista_patient)){
      if(i<= nrow(lista_patient[[j]]) ){
        value = c(value,lista_patient[[j]]$Vert_integrated_rim_area__vol_[i])  
      }
    }
    result = rbind(result, c(mean(na.omit(value)),sd(na.omit(value))))
    
  }
  
  return(result)
}

mean_sd_VIRA = compute_sd_mean_VIRA(final_patient_list)
colnames(mean_sd_VIRA) = c("mean_VIRA", "sd_VIRA")


sink("D:/User/Desktop/descriptive_indexes.txt")


print("***Sex***")
perc_F

print("***Age***")
age_table 

print("***Cardiovascular_dz***")
card_dz

print("***Fh***")
fh_table

print("***Diabetes***")
diabetes_table

print("***Hypertension***")
Hypertension_table

print("***Race***")
race_table

print("***RNFL***")
RNFL
mean_sd_RNFL

print("***MD***")
MD
mean_sd_MD

print("***IOP***")
IOP
mean_sd_IOP

print("***OPP***")
OPP
mean_sd_OPP


print("***HIRWA***")
HIRWA
mean_sd_HIRWA


print("***VIRA***")
VIRA
mean_sd_VIRA


