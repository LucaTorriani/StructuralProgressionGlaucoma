################################################################################
################################ CREATE DATASET ################################
################################################################################

source('function.R')
data = read.csv("data.csv")

n = dim(data)[1] # 1090
p = dim(data)[2] # 212

# Remove variables not considered
data = data[,-c(2,3, 8, 13, 15:17, 19:24, 26:38,  40:44, 50:51, 55:58, 60:67,
                71:98, 100:107, 110:113, 116:122, 133:134, 136, 149:159, 161:194, 196, 198:208, 209:212
                )]   
data = data[, -c(7,11,12,13,15,16,17,20,22,23,25,29:52,54,56)]

# Transform from char to int 
data = transform_to_binary(data, 'Hypertension')
data = transform_to_binary(data, 'Diabetes')
data = transform_to_binary(data, 'Cardiovascular_Dz')
data = transform_to_binary(data, 'family_history')
data = transform_to_binary(data, 'Sex')

data = codify_race(data)
data = codify_glaucoma(data)

patients_to_delete = c(502, 526, 541, 571, 582, 590, 600)
data108 = data
for (i in patients_to_delete){
  data108 = data108[!(data108$Patient == i),]
}

save (data108, file = "data108.Rdata")


lista_pazienti = split(data, as.factor(data$Patient))

new_lista_pazienti = map(lista_pazienti, insert_na)


# Remove visits
new_lista_pazienti$`543` = as.data.frame(slice(new_lista_pazienti$`543`, 1:11))
new_lista_pazienti$`557` = as.data.frame(slice(new_lista_pazienti$`557`, 1))
new_lista_pazienti$`577` = as.data.frame(slice(new_lista_pazienti$`577`, 1))
new_lista_pazienti$`588` = as.data.frame(slice(new_lista_pazienti$`588`, 1:10))
new_lista_pazienti$`549` = as.data.frame(slice(new_lista_pazienti$`549`, 1:4))

# Remove patients
patients_to_delete = c(502, 526, 541, 571, 582, 590, 600) 
idx_patients_to_delete = as.numeric(which(map_int (new_lista_pazienti, find_pat_to_delete, pat_to_delete=patients_to_delete)==1))
new_lista_pazienti = new_lista_pazienti[-idx_patients_to_delete]

# Interpolation of longitudinal data (var_to_interp) and filling NA of constant variables (var_to_copy)
var_to_interp = c("IOP", "MAP", "OPP", "HR", "MD", "Vert_integrated_rim_area__vol_", "Horz_integrated_rim_width__area_",
                  "mean_RNFL_thickness", "Age")
var_to_copy = c("Sex", "Race", "Type_of_glaucoma", "Hypertension", "Diabetes", "Cardiovascular_Dz", "family_history")

interp_lista_pazienti = map(new_lista_pazienti, regression, nomi = var_to_interp) # Regressione
interp_lista_pazienti = map(interp_lista_pazienti, copying, nomi = var_to_copy)
interp_lista_pazienti =  map(interp_lista_pazienti, fix_year_gl)

save (interp_lista_pazienti, file = "interp_lista_pazienti.Rdata")








