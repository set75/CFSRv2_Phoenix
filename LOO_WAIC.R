# LOO & WAIC for multi-predictor model
# after autorun.jags(…)

# 5 designated model parameters ---------------------------------------------------------------------

jags.model <- as.jags(x=model)
S <- 5000
# sample modeled coefficients
sample <- coda.samples(jags.model, variable.names=jags.params, n.iter=S) 

X1 <- LE
X2 <- SH
X3 <- DPT
X4 <- pRain
X5 <- EF
# read in modeled parameters
library(loo)
a <- unlist(sample[1:C][1:S,1]) # modeled alpha
b.1 <- unlist(sample[1:C][1:S,2]) # modeled beta.1
b.2 <- unlist(sample[1:C][1:S,3]) # modeled beta.2
b.3 <- unlist(sample[1:C][1:S,4]) # modeled beta.3
b.4 <- unlist(sample[1:C][1:S,5]) # modeled beta.4
b.5 <- unlist(sample[1:C][1:S,6]) # modeled beta.5
R <- S*C
log_m <- matrix(0, nrow = R, ncol = len)  # initialize log likelihood matrices
prob <- matrix(0, nrow = R, ncol = len) # initialize probability matrix
for (i in 1:R){
  for (j in 1:len){
    prob[i,j] <- inv.logit(a[i] + b.1[i]*X1[j] + b.2[i]*X2[j] + b.3[i]*X3[j] + b.4[i]*X4[j] + 
                             b.5[i]*X5[j]) # probability of rainfall
    log_m[i,j] <- dbern(Rain[j],prob[i,j],log=TRUE) # modeled rainfall as Bernoulli 
    if (log_m[i,j] < -0.1){log_m[i,j] = -0.1} # truncate negative infinite values
    if (log_m[i,j] > 1.1){log_m[i,j] = 1.1} # truncate positive infinite values
  }
}

model_waic <- waic(log_m)
model_loo <- loo(log_m)


# 4 designated model parameters ---------------------------------------------------------------------

jags.model <- as.jags(x=model)
S <- 5000
# sample modeled coefficients
sample <- coda.samples(jags.model, variable.names=jags.params, n.iter=S) 

X1 <- LE
X2 <- SH
X3 <- DPT
X4 <- pRain

# read in modeled parameters
library(loo)
a <- unlist(sample[1:C][1:S,1]) # modeled alpha
b.1 <- unlist(sample[1:C][1:S,2]) # modeled beta.1
b.2 <- unlist(sample[1:C][1:S,3]) # modeled beta.2
b.3 <- unlist(sample[1:C][1:S,4]) # modeled beta.3
b.4 <- unlist(sample[1:C][1:S,5]) # modeled beta.4
R <- S*C
log_m <- matrix(0, nrow = R, ncol = len)  # initialize log likelihood matrices
prob <- matrix(0, nrow = R, ncol = len) # initialize probability matrix
for (i in 1:R){
  for (j in 1:len){
    prob[i,j] <- inv.logit(a[i] + b.1[i]*X1[j] + b.2[i]*X2[j] + b.3[i]*X3[j] + b.4[i]*X4[j]) # probability of rainfall
    log_m[i,j] <- dbern(Rain[j],prob[i,j],log=TRUE) # modeled rainfall as Bernoulli 
    if (log_m[i,j] < -0.1){log_m[i,j] = -0.1} # truncate negative infinite values
    if (log_m[i,j] > 1.1){log_m[i,j] = 1.1} # truncate positive infinite values
  }
}

model_waic <- waic(log_m)
model_loo <- loo(log_m)

# 3 designated model parameters ---------------------------------------------------------------------

jags.model <- as.jags(x=model)
S <- 5000
# sample modeled coefficients
sample <- coda.samples(jags.model, variable.names=jags.params, n.iter=S) 

X1 <- LE
X2 <- SH
X3 <- DPT

# read in modeled parameters
library(loo)
a <- unlist(sample[1:C][1:S,1]) # modeled alpha
b.1 <- unlist(sample[1:C][1:S,2]) # modeled beta.1
b.2 <- unlist(sample[1:C][1:S,3]) # modeled beta.2
b.3 <- unlist(sample[1:C][1:S,4]) # modeled beta.3

R <- S*C
log_m <- matrix(0, nrow = R, ncol = len)  # initialize log likelihood matrices
prob <- matrix(0, nrow = R, ncol = len) # initialize probability matrix
for (i in 1:R){
  for (j in 1:len){
    prob[i,j] <- inv.logit(a[i] + b.1[i]*X1[j] + b.2[i]*X2[j] + b.3[i]*X3[j] ) # probability of rainfall
    log_m[i,j] <- dbern(Rain[j],prob[i,j],log=TRUE) # modeled rainfall as Bernoulli 
    if (log_m[i,j] < -0.1){log_m[i,j] = -0.1} # truncate negative infinite values
    if (log_m[i,j] > 1.1){log_m[i,j] = 1.1} # truncate positive infinite values
  }
}

model_waic <- waic(log_m)
model_loo <- loo(log_m)




