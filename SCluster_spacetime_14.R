SCluster_spacetime_14 <- nimbleCode({
  
  for(i in 1:N_barrios){
    
    for (t in 1:7){
      
      y[i,t] ~ dbin(p[i,t],N[i,t])
      
      logit(p[i,t]) <- log(theta[i]) + 
                      (z_trend[i]==1)*(0*t) +
                      (z_trend[i]==2)*(beta_dec*(t-1)) + 
                      (z_trend[i]==3)*(beta_inc*(t-1)) +
                      (z_trend[i]==4)*(delta1[t])
      
    }
    
    theta[i] <- eta[z[i]]
    z[i] ~ dcat(prlevels[1:k])
    z_trend[i] ~ dcat(prlevels_trend[1:k_trend])
    
  }
  
  # Increasing-decreasing trends
  
  beta_inc ~ T(dnorm(0,0.001), 0, 400)
  beta_dec ~ T(dnorm(0,0.001), -400, 0)
  
  prlevels[1:k] ~ ddirch(alfa[1:k])
  prlevels_trend[1:k_trend] ~ ddirch(alfa_trend[1:k_trend])
  
  eta[1] <- increta[1] 
  for(j in 2:k){
    eta[j] <- eta[j-1] + increta[j] 
  }
  
  for(j in 1:k){
    increta[j] ~ dgamma(1,1)
  }
  
  # RW1 prior
  
  delta1[1] <- 0
  sigma2.delta1 ~ dgamma(1,0.5)
  tau.delta1 <- 1/sigma2.delta1
  for (j in 2:7){
    delta1[j] ~ dnorm(delta1[j-1],tau.delta1)
  }  
  
})
