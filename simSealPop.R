
simSeals <- function(nyears=50, seals=1000, 
                     f=0.88, S0=0.35, S1=0.8, S2F=0.94, S2M = S2F*0.90,
                     changepar = "none", changetype="percentage", changeyear=11, 
                     changevec = c(0, 0, 0.5, 0.5, 0), nreps=30){
  
  # purpose: simulation of a harbor seal population 
  # inputs:
    # nyears: how many years to run the simulation, default = 50
    # seals: starting no. of seals in the population, default = 1000
    # f: fecundity per female, default = 0.88
    # S0: pup survival, default = 0.35
    # S1: juvenile survival, default = 0.8
    # S2F and S2M: adult survival, where males default = 0.9*S2F, 
    # nreps: number of simulations to run, default = 1000
    # changetype: "none", "percentage", "exact"
    # changepar: vital rate(s) (S0, S1, S2F, S2M, f) to modify, default = "none"
    # changeyear: year when vital rate changepar changes, default = 11
    # changevec: vector of length 5. Reductions or exact rates to simulate from changeyear, 
    #   corresponding to vital rates as in changepar and type as in changetype
    # 
  # outputs: 
    # out: a list of length 2
      # Ntot: a matrix of pop sizes (dim nyears x nrep) 
      # Nobs: a matrix of obs pop sizes (dim nyears x nrep)
  
  out <- list("Ntot" = list(), "Nobs" = list())
  
  # set up matrix of vital rates
  vmat <- matrix(NA, nrow=5, ncol=nyears)
  vmat[1,] <- S0
  vmat[2,] <- S1
  vmat[3,] <- S2M
  vmat[4,] <- S2F
  vmat[5,] <- f 
  
  # construct the Leslie matrix for initial conditions
  L <- matrix(nrow=10, ncol=10, data=0)
  L[1,10] <- f*S2F/2
  L[6,10] <- f*S2F/2
  L[2,1] <- S0
  L[3,2] <- S1
  L[4,3] <- S1
  L[5,4] <- S1
  L[5,5] <- S2M
  L[7,6] <- S0
  L[8,7] <- S1
  L[9,8] <- S1
  L[10,9] <- S1
  L[10,10] <- S2F
  
  first.evector <- Re(eigen(L)$vectors[,1])
  agestructure <- first.evector/(sum(first.evector))
  
  # vector of haulout probabilities for each age/sex
  pHo <- c(0.1, 0.5, 0.5, 0.5, 0.6, 0.1, 0.5, 0.5, 0.5, 0.5)
  
  if(changepar == "none"){vmat <- vmat} else {
    
    # modify the vital rates matrix according to changepar and p
    if("S0" %in% changepar){
      if(changetype == "percentage"){vmat[1, (changeyear):nyears] <- vmat[1, (changeyear):nyears]*changevec[1]}
      if(changetype == "exact"){vmat[1, (changeyear):nyears] <- changevec[1]}}
      
    if("S1" %in% changepar){
      if(changetype == "percentage"){vmat[2, (changeyear):nyears] <- vmat[2, (changeyear):nyears]*changevec[2]} 
      if(changetype == "exact"){vmat[2, (changeyear):nyears] <- changevec[2]}}
    
    if("S2M" %in% changepar){
      if(changetype == "percentage"){vmat[3, (changeyear):nyears] <- vmat[3, (changeyear):nyears]*changevec[3]} 
      if(changetype == "exact"){vmat[3, (changeyear):nyears] <- changevec[3]}} 
      
    if("S2F" %in% changepar){
      if(changetype == "percentage"){vmat[4, (changeyear):nyears] <- vmat[4, (changeyear):nyears]*changevec[4]}
      if(changetype == "exact"){vmat[4, (changeyear):nyears] <- changevec[4]}}
      
    if("f" %in% changepar){
      if(changetype == "percentage"){vmat[5, (changeyear):nyears] <- vmat[5, (changeyear):nyears]*changevec[5]}
      if(changetype == "exact"){vmat[5, (changeyear):nyears] <- changevec[5]}}
    } # end changepar

    
    # set up matrix to hold total population size in each year for each iteration
    Ntot <- matrix(nrow=nreps, ncol=nyears)
    
    # set up matrix to hold observed population size in each year for each iteration
    O <- matrix(nrow=nreps, ncol=nyears)
    
    # run nreps population models forward
    for (i in 1:nreps){
      
      # initialize population and observation matrices
      N <- matrix(nrow=10, ncol=nyears)
      N[,1] <- round(seals*agestructure)
      obs <- rep(NA, length(nyears))
      
      for (t in 2:nyears){
        # population process model  
        
        # Males
        N[2, t] <- rbinom(n = 1, size = N[1, t-1], prob = vmat[1, t-1])
        N[3, t] <- rbinom(n = 1, size = N[2, t-1], prob = vmat[2, t-1])
        N[4, t] <- rbinom(n = 1, size = N[3, t-1], prob = vmat[2, t-1])
        N[5, t] <- rbinom(n = 1, size = N[4, t-1], prob = vmat[2, t-1]) +
          rbinom(n = 1, size = N[5, t-1], prob = vmat[3, t-1])
        
        # Females
        N[7, t] <- rbinom(n = 1, size = N[6, t-1], prob = vmat[1, t-1])
        N[8, t] <- rbinom(n = 1, size = N[7, t-1], prob = vmat[2, t-1])
        N[9, t] <- rbinom(n = 1, size = N[8, t-1], prob = vmat[2, t-1])
        N[10, t] <- rbinom(n = 1, size = N[9, t-1], prob = vmat[2, t-1]) +
          rbinom(n = 1, size = N[10, t-1], prob = vmat[4, t-1])
        
        # Pups 
        N[1, t] <- rbinom(n = 1, size = round(N[10, t-1]/2), prob = vmat[5, t-1])
        N[6, t] <- rbinom(n = 1, size = round(N[10, t-1]/2), prob = vmat[5, t-1]) 
        
        # observational model 
        
        obs[t] <- sum(rbinom(n = 10, size = N[,t], prob = pHo))
        
      } # end t in 1:nyears
      
      Ntot[i, ] <- colSums(N)
      
      # fill in the first count, since t was from 2:t
      obs[1] <- sum(rbinom(n = 10, size = N[,1], prob = pHo))
      O[i, ] <- obs
      
    } # end i in 1:nrep
    
    # store output matrices for nreps of pvec[p] change
    out$Ntot <- Ntot
    out$Nobs <- O
    out$RoI <- mean(apply(out$Ntot, 1, function(x) mean(diff(log(x[11:35])) )))
    
  return(out)
  
} # end function
