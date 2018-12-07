# CFSR Logistic Regression & LOO

rm(list=ls())
library(rjags)
library(runjags)
library(statip)
library(boot)
setwd("~/Documents/CFSR_Phoenix")
load("CFSR_scaled.RData")


# Read in data ------------------------------------------------------------

num_day <- df.all$num_day
Rain <- df.all$Rain
Rain <- as.numeric(Rain)
pRain <- df.all$previous.rain
CAPE <- df.all$CAPE
CIN <- df.all$CIN
EF <- df.all$EF
LE <- df.all$LE
SH <- df.all$SH
Vap <- df.all$Vap
Uh <- df.all$Uh
Uz <- df.all$Uz
logCAPE <- df.all$logCAPE
logCIN <- df.all$CIN
Temp <- df.all$Temp
DPT <- df.all$DPT
len <- length(DPT)

# Total model -------------------------------------------------------------
line_data <- list(Rain=Rain,pRain=pRain,CAPE=CAPE,CIN=CIN,EF=EF,LE=LE,SH=SH,
                  Uh=Uh,Uz=Uz,Temp=Temp,DPT=DPT,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1), "b.1"=rnorm(1), "b.2"=rnorm(1),
                              "b.3"=rnorm(1), "b.4"=rnorm(1), "b.5"=rnorm(1),
                              "b.6"=rnorm(1), "b.7"=rnorm(1), "b.8"=rnorm(1),
                              "b.9"=rnorm(1), "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.1","b.2","b.3","b.4","b.5","b.6","b.7","b.8","b.9","b.10")
C <- 3
model <- autorun.jags("CFSR_all.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)

# Total model minus Uz -------------------------------------------------------------
line_data <- list(Rain=Rain,pRain=pRain,CAPE=CAPE,CIN=CIN,EF=EF,LE=LE,SH=SH,
                  Uh=Uh,Temp=Temp,DPT=DPT,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1), "b.1"=rnorm(1), "b.2"=rnorm(1),
                              "b.3"=rnorm(1), "b.4"=rnorm(1), "b.5"=rnorm(1),
                              "b.6"=rnorm(1), "b.7"=rnorm(1), 
                              "b.9"=rnorm(1), "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.1","b.2","b.3","b.4","b.5","b.6","b.7","b.9","b.10")
C <- 3
model <- autorun.jags("CFSR_all-1.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)

# Total model minus Uz,T -------------------------------------------------------------
line_data <- list(Rain=Rain,pRain=pRain,CAPE=CAPE,CIN=CIN,EF=EF,LE=LE,SH=SH,
                  Uh=Uh,DPT=DPT,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1), "b.1"=rnorm(1), "b.2"=rnorm(1),
                              "b.3"=rnorm(1), "b.4"=rnorm(1), "b.5"=rnorm(1),
                              "b.6"=rnorm(1), "b.7"=rnorm(1), 
                              "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.1","b.2","b.3","b.4","b.5","b.6","b.7","b.10")
C <- 3
model <- autorun.jags("CFSR_all-2.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)

# Total model minus Uz,T,Uh -------------------------------------------------------------
line_data <- list(Rain=Rain,pRain=pRain,CAPE=CAPE,CIN=CIN,EF=EF,LE=LE,SH=SH,
                  DPT=DPT,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1), "b.1"=rnorm(1), "b.2"=rnorm(1),
                              "b.3"=rnorm(1), "b.4"=rnorm(1), "b.5"=rnorm(1),
                              "b.6"=rnorm(1), 
                              "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.1","b.2","b.3","b.4","b.5","b.6","b.10")
C <- 3
model <- autorun.jags("CFSR_all-3.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)

# Total model minus Uz,T,Uh,CIN -------------------------------------------------------------
line_data <- list(Rain=Rain,pRain=pRain,CAPE=CAPE,EF=EF,LE=LE,SH=SH,
                  DPT=DPT,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1), "b.1"=rnorm(1), "b.2"=rnorm(1),
                              "b.4"=rnorm(1), "b.5"=rnorm(1),
                              "b.6"=rnorm(1), 
                              "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.1","b.2","b.4","b.5","b.6","b.10")
C <- 3
model <- autorun.jags("CFSR_all-4.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)

# Total model minus Uz,T,Uh,CIN,CAPE -------------------------------------------------------------
line_data <- list(Rain=Rain,pRain=pRain,EF=EF,LE=LE,SH=SH,
                  DPT=DPT,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1), "b.1"=rnorm(1),
                              "b.4"=rnorm(1), "b.5"=rnorm(1),
                              "b.6"=rnorm(1), 
                              "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.1","b.4","b.5","b.6","b.10")
C <- 3
model <- autorun.jags("CFSR_all-5.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)

# Total model minus Uz,T,Uh,CIN,CAPE -------------------------------------------------------------
line_data <- list(Rain=Rain,pRain=pRain,LE=LE,SH=SH,
                  DPT=DPT,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1), "b.1"=rnorm(1),
                              "b.5"=rnorm(1),
                              "b.6"=rnorm(1), 
                              "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.1","b.5","b.6","b.10")
C <- 3
model <- autorun.jags("CFSR_all-6.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)

# Total model minus Uz,T,Uh,CIN,CAPE, previous rain -------------------------------------------------------------
line_data <- list(Rain=Rain,LE=LE,SH=SH,DPT=DPT,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1),
                              "b.5"=rnorm(1),
                              "b.6"=rnorm(1), 
                              "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.5","b.6","b.10")
C <- 3
model <- autorun.jags("CFSR_all-7.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)

# full model w/ log transform CAPe & CIN -------------------------------------------------------------
line_data <- list(Rain=Rain,Vap=Vap,logCAPE=logCAPE,logCIN=logCIN,EF=EF,
                  LE=LE,SH=SH,Uh=Uh,Uz=Uz,Temp=Temp,DPT=DPT,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1), "b.1"=rnorm(1), "b.2"=rnorm(1),
                              "b.3"=rnorm(1), "b.4"=rnorm(1), "b.5"=rnorm(1),
                              "b.6"=rnorm(1), "b.7"=rnorm(1), "b.8"=rnorm(1),
                              "b.9"=rnorm(1), "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.1","b.2","b.3","b.4","b.5","b.6","b.7","b.8","b.9","b.10")
C <- 3
model <- autorun.jags("CFSR_all2.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)

# Comparison model -------------------------------------------------------------
line_data <- list(Rain=Rain,pRain=pRain,EF=EF,LE=LE,SH=SH,
                  Uh=Uh,Uz=Uz,Temp=Temp,DPT=DPT,N=len) # 10 parameters
set.seed(27) # for reproducibility
line_inits <- function(){list("alpha"=rnorm(1), "b.1"=rnorm(1), "b.4"=rnorm(1), "b.5"=rnorm(1),
                              "b.6"=rnorm(1), "b.7"=rnorm(1), "b.8"=rnorm(1),
                              "b.9"=rnorm(1), "b.10"=rnorm(1))} # random gen of initial values
jags.params <- c("alpha","b.1","b.4","b.5","b.6","b.7","b.8","b.9","b.10")
C <- 3
model <- autorun.jags("CFSR_comp.bug.txt", data = line_data, inits = line_inits, n.chains = C,
                      monitor = jags.params, method = "rjags")
summary(model)


# linear model ------------------------------------------------------------
glm.data <- data.frame(pRain,EF,LE,SH,Uh,Uz,Temp,DPT)
frequentist <- glm(Rain ~., family=binomial(link='logit'), data=glm.data)
summary(frequentist)




