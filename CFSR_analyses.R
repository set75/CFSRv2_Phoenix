# CFSR_v2 view morning averages & reseize / recenter data 
# Beginning w/ new_data.RData generated from CSFR_format_data.R

rm(list=ls())
setwd("~/Documents/CFSR_Phoenix")
load("new_data.RData")

# Covariance -------------------------------------------------------------
cov_matrix <- cov(new_data[,3:12])
cor_matrix <- cor(new_data[,3:17])

# Data -------------------------------------------------------------------
set.seed(27) # for reproducibility
len <- length(new_data$rainy)
set <- sample.int(n=len, size=2679) # randomized sampling
# read in variables
Rain <- new_data$rainy[set]
previous.rain <- c(0,new_data$rainy[1:(len-1)])
Vap <- new_data$q[set]
SH <- new_data$SH[set]
LE <- new_data$LE[set]
Temp <- new_data$Temp[set]
Uz <- new_data$Uz[set]
EF <- LE/(LE+SH)
DPT <- new_data$DPT[set]
CAPE <- new_data$CAPE[set]
CIN <- new_data$CIN[set]
Ux <- new_data$Ux[set]
Uy <- new_data$Uy[set]
Uh <- sqrt(Ux^2 + Uy^2)
num_day <- new_data$num_day[set]

# truncate EF
l <- length(EF)
for(i in 1:l){
  if(EF[i] < 0){EF[i] = 0}
  if(EF[i] > 1){EF[i] = 1}
}

# log transform CAPE & CIN
logCAPE <- log10(new_data$CAPE + 0.001)
logCIN <- log10(abs(new_data$CIN) + 0.001)

new_data2 <- data.frame(num_day,Uz,LE,SH,Vap,Temp,DPT,CAPE,CIN,EF,Uh,
                        logCAPE,logCIN,Rain,previous.rain)

corr <- cor(new_data2)


# Resize/Recenter ---------------------------------------------------------
# Recenter: Temp, DPT; Resize: All
df.temp <- data.frame(Temp, DPT)
df.temp <- scale(df.temp)
df.norm <- data.frame(CAPE,CIN,EF,LE,SH,Vap,Uh,Uz,logCAPE,logCIN)
df.norm <- scale(df.norm, center=FALSE, scale=apply(df.norm,2,sd,na.rm=TRUE)) 
df.all <- data.frame(num_day, Rain, previous.rain, df.norm, df.temp)
# check that mean=0, sd=1
colMeans(df.all)
apply(df.all,2,sd)

save(df.all, file = "CFSR_scaled.RData")



