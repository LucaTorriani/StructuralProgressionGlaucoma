
library(dplyr)
library(MASS)
library(readr)
library(purrr)

source('function.R')
data = read.csv("data.csv")
n = dim(data)[1] # 1090
p = dim(data)[2] # 212

data = dplyr::select (data, - c(Visit,Field2,final,nsl_sup, nsl_inf,nasal,tmp_sup, tmp_inf, temporal,Moorfields_global,GHT,Medication_changes_, Comments, PressuresComments, HRT3comments, QuestionnaireComments, CDIcomments,
                                OCTcomments, SpectralisComments, MissingDataExplanation, Use_this_value_,
                                FieldsComment, Added_Med_1, Added_Med_2,  Stopped_Med_1, Stopped_Med_2,
                                Allergies, Meds_according_to_Pt, Ocular_Meds_according_to_Pt, Other_systemic_dz_1, Studyeye))



data = transform_to_binary(data, 'Hypertension')
data = transform_to_binary(data, 'HyperLipidemia')
data = transform_to_binary(data, 'Diabetes')
data = transform_to_binary(data, 'Cardiovascular_Dz')
data = transform_to_binary(data, 'family_history')
data = transform_to_binary(data, 'Sex')
data = transform_to_binary(data, 'Smoking')

data = codify_race(data)
data = codify_glaucoma(data)


patients_to_delete = c(502, 526, 541, 571, 582, 590, 600)
data108 = data
for (i in patients_to_delete){
  data108 = data108[!(data108$Patient == i),]
}

lista_pazienti_tot = split(data, as.factor(data$Patient))
final_patient_list115 = map(lista_pazienti_tot, insert_na)
data_final115 =  map_df(final_patient_list115, rbind)

lista_pazienti = split(data108, as.factor(data108$Patient))
final_patient_list = map(lista_pazienti, insert_na)
data_final = map_df(final_patient_list, rbind)

data = dplyr::select(data, c("Patient", "Sex", "Race", "yearofglaucoma", "Hypertension", "Diabetes",
                              "Cardiovascular_Dz", "family_history","IOP","MAP", "MD",
                              "Vert_integrated_rim_area__vol_", "Horz_integrated_rim_width__area_",
                              "mean_RNFL_thickness", "Age", "OPP"))

data_final115 = dplyr::select(data_final115, c("Patient", "Sex", "Race", "yearofglaucoma", "Hypertension", "Diabetes",
                                               "Cardiovascular_Dz", "family_history","IOP","MAP", "MD",
                                               "Vert_integrated_rim_area__vol_", "Horz_integrated_rim_width__area_",
                                               "mean_RNFL_thickness", "Age", "OPP"))


data108 = dplyr::select(data108, c("Patient", "Sex", "Race", "yearofglaucoma", "Hypertension", "Diabetes",
                             "Cardiovascular_Dz", "family_history","IOP","MAP", "MD",
                             "Vert_integrated_rim_area__vol_", "Horz_integrated_rim_width__area_",
                             "mean_RNFL_thickness", "Age", "OPP"))

data_final = dplyr::select(data_final, c("Patient", "Sex", "Race", "yearofglaucoma", "Hypertension", "Diabetes",
                             "Cardiovascular_Dz", "family_history","IOP","MAP", "MD",
                             "Vert_integrated_rim_area__vol_", "Horz_integrated_rim_width__area_",
                             "mean_RNFL_thickness", "Age", "OPP"))





visits = map(final_patient_list, nrow)
visits_vec = do.call(rbind,visits)

########################################################
## count tot na 

# 115 pazienti, without NA of missing visits
nr_na_115_or = sum(is.na(data))/(dim(data)[1]*dim(data)[2])

# 115 pazienti, with NA of missing visits
nr_na_115_new = sum(is.na(data_final115))/(dim(data_final115)[1]*dim(data_final115)[2])


# 108 pazienti, without NA of missing visits
nr_na_108_or = sum(is.na(data108))/(dim(data108)[1]*dim(data108)[2])

# 108 pazienti, with NA of missing visits
nr_na_108_new = sum(is.na(data_final))/(dim(data_final)[1]*dim(data_final)[2])



## count na in MD and HIRWA

# 115 pazienti, without NA of missing visits
na_MD_115_or = sum(is.na(data$MD))/length(data$MD)

# 115 pazienti, with NA of missing visits
na_MD_115_new = sum(is.na(data_final115$MD))/length(data_final115$MD)

# 108 pazienti, without NA of missing visits
na_MD_108_or = sum(is.na(data108$MD))/length(data108$MD)

# 108 pazienti, with NA of missing visits
na_MD_108_new = sum(is.na(data_final$MD))/length(data_final$MD)


# 115 pazienti, without NA of missing visits
na_HIRWA_115_or = sum(is.na(data$Horz_integrated_rim_width__area_))/length(data$Horz_integrated_rim_width__area_)

# 115 pazienti, with NA of missing visits
na_HIRWA_115_new = sum(is.na(data_final115$Horz_integrated_rim_width__area_))/length(data_final115$Horz_integrated_rim_width__area_)

# 108 pazienti, without NA of missing visits
na_HIRWA_108_or = sum(is.na(data108$Horz_integrated_rim_width__area_))/length(data108$Horz_integrated_rim_width__area_)

# 108 pazienti, with NA of missing visits
na_HIRWA_108_new = sum(is.na(data_final$Horz_integrated_rim_width__area_))/length(data_final$Horz_integrated_rim_width__area_)




