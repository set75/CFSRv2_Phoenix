# CFSR Histogram Plots
rm(list=ls())
setwd("~/Documents/CFSR_Phoenix")
load("new_data.RData")

library(ggplot2)
library(dplyr)
library(RColorBrewer)

EF <- new_data$LE/(new_data$LE+new_data$SH)
logLE <- new_data$LE
l <- length(EF)
for(i in 1:l){
  if(EF[i] < 0){EF[i] = 0}
  if(EF[i] > 1){EF[i] = 1}
  if(logLE[i] > 0){logLE[i] = log10(logLE[i])}
}
Uh <- sqrt(new_data$Ux^2 + new_data$Uy^2)
logCAPE <- log10(new_data$CAPE + 0.001)
logCIN <- log10(abs(new_data$CIN) + 0.001)

new_data <- data.frame(new_data,EF,Uh,logCAPE,logCIN,logLE)
df.rain<-new_data[new_data$rainy==1,]

# 1D histogram with Rain scatter overlay
ggplot(data=new_data, aes(x=Temp))+
  geom_density()+
  geom_point(data=df.rain, aes(x=Temp, y=0, col="red"))+
  theme_bw()+
  theme(legend.position="none")+
  xlab("Temperature [K]")

# 2D histograms with Rain scatter overlay
df.rain<-new_data2[new_data2$Rain==1,]
ggplot(NULL,aes(x=CIN, y=DPT) ) +
  geom_bin2d(data=new_data2) + 
  geom_point(data=df.rain, col="red")+
  theme_bw() + xlab("CIN J/kg") +
  ylab("Dew Point Temperature K")

