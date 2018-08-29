library(plotrix)
library(coda)

###############################################################################
############################## Model HIRWA ####################################
###############################################################################
load("output_modelHIRWA.Rdata")
output = output_modelHIRWA
data= as.matrix(output)
data1= as.data.frame(data)
nomi = names(data1)
rm(data)

####### IC b_i
data_red_row = as.matrix(data1[,1:108])  # Select the b_i
median_cols = apply(data_red_row, 2, quantile, probs=c(0.025,0.5,0.975))

# Vertical plot
quartz()
plotCI(x=seq(1,108),y=median_cols[2,], uiw=(median_cols[3,]-median_cols[2,]) ,liw=(median_cols[2,]-median_cols[1,]), 
       scol=1, ylab = expression("b"[i]), xlab = "Patient i", pt.bg=par("bg"),pch=20)  
abline(h = median(median_cols[2,]), col = "red")

# # Horizonal plot
# quartz()
# plotCI(y=seq(1,108),x=median_cols[2,], uiw=(median_cols[3,]-median_cols[2,]) ,liw=(median_cols[2,]-median_cols[1,]), 
#        scol=1, pt.bg=par("bg"),pch=20, yaxt = "n", ylab = "Patient i", err = "x", xlab= expression(b[i]))
# abline (v= 0, col = "red")


####### IC beta
data_red_row = as.matrix(data1[,110:116])  # Select the betas (without beta0)

n_beta = dim(data_red_row) [2]
median_cols = apply(data_red_row, 2, quantile, probs=c(0.025,0.5,0.975))

etichette= c("gender", "YoG", "fh","IOP", "diab", "MD", "age")

# # Vertical
# quartz()
# par(mar = c(8,6,4,2) + 0.1)
# plotCI(x=seq(1,n_beta),y=median_cols[2,], uiw=(median_cols[3,]-median_cols[2,]) ,liw=(median_cols[2,]-median_cols[1,]), 
#        scol=1, pt.bg=par("bg"),pch=20, xaxt = "n", xlab = "", ylab= expression(beta[i]))
# axis(1, at=1:n_beta, labels = etichette, las = 2)
# abline (h= 0, col = "red")


# Horizontal
quartz()
par(mar = c(5,8,4,2) + 0.1)
plotCI(y=seq(1,n_beta),x=median_cols[2,], uiw=(median_cols[3,]-median_cols[2,]) ,liw=(median_cols[2,]-median_cols[1,]), 
       scol=1, pt.bg=par("bg"),pch=20, yaxt = "n", ylab = "", err = "x", xlab= expression(beta[i]))
axis(2, at=1:n_beta, labels = etichette, las = 2)
abline (v= 0, col = "red")





###############################################################################
################################# Model MD ####################################
###############################################################################
rm(list=ls())
graphics.off()

load("output_modelMD.Rdata")
output = output_modelMD
data= as.matrix(output)
data1= as.data.frame(data)
nomi = names(data1)
rm(data)

##### IC beta
data_red_row = as.matrix(data1[,2:4])  # Select the betas (without beta0)
n_beta = dim(data_red_row) [2]

median_cols = apply(data_red_row, 2, quantile, probs=c(0.025,0.5,0.975))
etichette = c("t", "IOP", "HIRWA")

# # Vertical plot
# quartz()
# par(mar = c(8,6,4,2) + 0.1)
# plotCI(x=seq(1,n_beta),y=median_cols[2,], uiw=(median_cols[3,]-median_cols[2,]) ,liw=(median_cols[2,]-median_cols[1,]), 
#        scol=1, pt.bg=par("bg"),pch=20, xaxt = "n", xlab = "", ylab= expression(beta[i]))
# axis(1, at=1:n_beta, labels = etichette, las = 2)
# abline (h= 0, col = "red")

# Horizontal plot
quartz()
par(mar = c(5,8,4,2) + 0.1)
plotCI(y=seq(1,n_beta),x=median_cols[2,], uiw=(median_cols[3,]-median_cols[2,]) ,liw=(median_cols[2,]-median_cols[1,]), 
       scol=1, pt.bg=par("bg"),pch=20, yaxt = "n", ylab = "", err = "x", xlab= expression(beta[i]))
axis(2, at=1:n_beta, labels = etichette, las = 2)
abline (v= 0, col = "red")



