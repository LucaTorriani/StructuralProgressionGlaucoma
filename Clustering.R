library(coda)

load("output_modelHIRWA.Rdata")
output = output_modelHIRWA
data= as.matrix(output)
data1= as.data.frame(data)
nomi = names(data1)
rm(data)

data_red_row = as.matrix(data1[,1:108])  # Select the b_i
median_cols = apply(data_red_row, 2, quantile, probs=c(0.025,0.5,0.975))

########################################################################
########################## Identify clusters ###########################
########################################################################
idx_pos = (median_cols[1,]>0)
idx_neg = (median_cols[3,]<0)
idx_med = (!idx_pos & !idx_neg)
n_pos = sum(idx_pos)
n_neg = sum(idx_neg)
n_med = sum(idx_med)

########################################################################
###################### Features in the clusters ########################
########################################################################
load("interp_lista_pazienti.RData")
library(purrr)
library (dplyr)

# Longitudinal variables (IOP, MD, HIRWA, mOPP)
interp_lista_pazienti_num = map (interp_lista_pazienti, dplyr::select, c(IOP,MD,Horz_integrated_rim_width__area_))
pat_pos =  interp_lista_pazienti_num[idx_pos]
pat_neg = interp_lista_pazienti_num[idx_neg]
pat_med = interp_lista_pazienti_num[idx_med]
pat_pos_matrix_num = map_df(pat_pos, rbind)
pat_neg_matrix_num = map_df(pat_neg, rbind)
pat_med_matrix_num = map_df(pat_med, rbind)
avg_neg_num = colMeans(pat_neg_matrix_num)
avg_med_num = colMeans(pat_med_matrix_num)
avg_pos_num = colMeans(pat_pos_matrix_num)
big_matrix = rbind(pat_pos_matrix_num,pat_neg_matrix_num,pat_med_matrix_num)
big_matrix = cbind(big_matrix,( c(rep(1,nrow(pat_pos_matrix_num)), rep(2,nrow(pat_neg_matrix_num)),rep(3,nrow(pat_med_matrix_num))) ))

sd(pat_neg_matrix_num[,2])  # sd MD
sd(pat_med_matrix_num[,2])
sd(pat_pos_matrix_num[,2])

interp_lista_pazienti_num_OPP = map (interp_lista_pazienti, dplyr::select, OPP)
interp_lista_pazienti_num_OPP_avg = map_dbl (interp_lista_pazienti_num_OPP, colMeans)
pat_pos_mOPP =  interp_lista_pazienti_num_OPP_avg[idx_pos]
pat_neg_mOPP = interp_lista_pazienti_num_OPP_avg[idx_neg]
pat_med_mOPP = interp_lista_pazienti_num_OPP_avg[idx_med]
avg_pos_mOPP = mean(pat_pos_mOPP)
avg_neg_mOPP = mean(pat_neg_mOPP)
avg_med_mOPP = mean(pat_med_mOPP)
big_matrix_OPP = c(pat_pos_mOPP,pat_neg_mOPP,pat_med_mOPP)
big_matrix_OPP = cbind(big_matrix_OPP,( c(rep(1,length(pat_pos_mOPP)), rep(2,length(pat_neg_mOPP)),rep(3,length(pat_med_mOPP))) ))


# Constant variables (YoG, Age_baseline)
baseline_data = map(interp_lista_pazienti, dplyr::slice, 1)
baseline_data_red = map (baseline_data, dplyr::select, c(Sex, yearofglaucoma, family_history, Diabetes, Age)) 

pat_pos_baseline = baseline_data_red[idx_pos]
pat_neg_baseline = baseline_data_red[idx_neg]
pat_med_baseline = baseline_data_red[idx_med]
pat_pos_matrix_baseline = map_df(pat_pos_baseline, rbind)
pat_neg_matrix_baseline = map_df(pat_neg_baseline, rbind)
pat_med_matrix_baseline = map_df(pat_med_baseline, rbind)

avg_pos_baseline = colMeans(pat_pos_matrix_baseline)
avg_neg_baseline = colMeans(pat_neg_matrix_baseline)
avg_med_baseline = colMeans(pat_med_matrix_baseline)

big_matrix_baseline = rbind(pat_pos_matrix_baseline,pat_neg_matrix_baseline,pat_med_matrix_baseline)
big_matrix_baseline = dplyr::select (big_matrix_baseline, c(yearofglaucoma, Age)) 
big_matrix_baseline = cbind(big_matrix_baseline,( c(rep(1,nrow(pat_pos_matrix_baseline)), rep(2,nrow(pat_neg_matrix_baseline)),rep(3,nrow(pat_med_matrix_baseline))) ))


# # Barplots
# quartz()
# par(mfrow=c(2,4))
# barplot(c(avg_pos_num[1],avg_neg_num[1],avg_med_num[1]), names.arg = c("Pos", "Neg", "Med"), col = c("green", "red", "blue"),
#         main = "IOP") # IOP
# barplot(c(avg_pos_num[2],avg_neg_num[2],avg_med_num[2]), names.arg = c("Pos", "Neg", "Med"), col = c("green", "red", "blue"),
#         main = "MD") # MD
# barplot(c(avg_pos_mOPP,avg_neg_mOPP,avg_med_mOPP), names.arg = c("Pos", "Neg", "Med"), col = c("green", "red", "blue"),
#         main = "meanOPP")
# barplot(c(avg_pos_baseline[1],avg_neg_baseline[1],avg_med_baseline[1]), names.arg = c("Pos", "Neg", "Med"), col = c("green", "red", "blue"),
#         main = "Sex")
# barplot(c(avg_pos_baseline[2],avg_neg_baseline[2],avg_med_baseline[2]), names.arg = c("Pos", "Neg", "Med"), col = c("green", "red", "blue"),
#         main= "YoG_base")
# barplot(c(avg_pos_baseline[3],avg_neg_baseline[3],avg_med_baseline[3]), names.arg = c("Pos", "Neg", "Med"), col = c("green", "red", "blue"),
#         main="fh")
# barplot(c(avg_pos_baseline[4],avg_neg_baseline[4],avg_med_baseline[4]), names.arg = c("Pos", "Neg", "Med"), col = c("green", "red", "blue"),
#         main="Diabetes")
# barplot(c(avg_pos_baseline[5],avg_neg_baseline[5],avg_med_baseline[5]), names.arg = c("Pos", "Neg", "Med"), col = c("green", "red", "blue"),
#         main= "Age_base")

# Boxplots

# quartz()
x11()
boxplot(big_matrix[,3] ~ big_matrix[,4], main = "HIRWA", names = c("P","N","Z"))

# quartz()
x11()
boxplot(big_matrix[,2] ~ big_matrix[,4], main = "MD", names = c("P","N","Z"))

# quartz()
x11()
boxplot(big_matrix[,1] ~ big_matrix[,4], main = "IOP", names = c("P","N","Z"))

# quartz()
x11()
boxplot(big_matrix_OPP[,1] ~ big_matrix_OPP[,2], main = "mOPP", names = c("P","N","Z"))

# quartz()
x11()
boxplot(big_matrix_baseline[,1] ~ big_matrix_baseline[,3], main = "YoG", names = c("P","N","Z"))

# quartz()
x11()
boxplot(big_matrix_baseline[,2] ~ big_matrix_baseline[,3], main = "Age baseline", names = c("P","N","Z"))



########################################################################
################################ Tests #################################
########################################################################

### MD
# P N
wilcox.test(pat_pos_matrix_num[,2],pat_neg_matrix_num[,2], alternative = "two.sided",
            mu=0, paired= FALSE)
# low p_value (6.097e-05) --> Different

# P Z
wilcox.test(pat_pos_matrix_num[,2],pat_med_matrix_num[,2], alternative = "two.sided",
            mu=0, paired= FALSE)
# low p_value (0.0004257) --> Different

# N Z
wilcox.test(pat_neg_matrix_num[,2],pat_med_matrix_num[,2], alternative = "two.sided",
            mu=0, paired= FALSE)
# high p_value (0.4106) --> Equal

### IOP
# P N
wilcox.test(pat_pos_matrix_num[,1],pat_neg_matrix_num[,1], alternative = "two.sided",
            mu=0, paired= FALSE)
# high p_value (0.8155) --> Equal

# P Z
wilcox.test(pat_pos_matrix_num[,1],pat_med_matrix_num[,1], alternative = "two.sided",
            mu=0, paired= FALSE)
# high p_value (0.853) --> Equal

# N Z
wilcox.test(pat_neg_matrix_num[,1],pat_med_matrix_num[,1], alternative = "two.sided",
            mu=0, paired= FALSE)
# high p_value (0.8374) --> Equal

### HIRWA
# P N
wilcox.test(pat_pos_matrix_num[,3],pat_neg_matrix_num[,3], alternative = "two.sided",
            mu=0, paired= FALSE)
# low p_value (0.04434) --> Different

# P Z
wilcox.test(pat_pos_matrix_num[,3],pat_med_matrix_num[,3], alternative = "two.sided",
            mu=0, paired= FALSE)
# high p_value (0.1686) --> Equal

# N Z
wilcox.test(pat_neg_matrix_num[,3],pat_med_matrix_num[,3], alternative = "two.sided",
            mu=0, paired= FALSE)
# high p_value (0.9186) --> Equal

### mOPP
# P N
wilcox.test(as.vector(pat_pos_mOPP),as.vector(pat_neg_mOPP), alternative = "two.sided",
            mu=0, paired= FALSE)
# low p_value (0.006029) --> Different

# P Z
wilcox.test(as.vector(pat_pos_mOPP),as.vector(pat_med_mOPP), alternative = "two.sided",
            mu=0, paired= FALSE)
# low p_value (0.03344) --> Different

# N Z
wilcox.test(as.vector(pat_neg_mOPP),as.vector(pat_med_mOPP), alternative = "two.sided",
            mu=0, paired= FALSE)
# high p_value (0.1854) --> Equal

