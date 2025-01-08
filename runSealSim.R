
# Code illustrating how to run and plot seal simulations
#DJFR: adjusted to add separate male and female adult survival rate (in previous version, one value s2, and then in sim code male survival 0.9 x s2)

# nyears: how many years to run the simulation, default = 50
# seals: starting no. of seals in the population, default = 1000
# f: fecundity per female, default = 0.7
# S0: pup survival, default = 0.5
# S1: juvenile survival, default = 0.7
# S2F: adult female survival
# S2M: adult male survival (default = 0.9*S2F) 
# nreps: number of simulations to run, default = 1000
# changeyear: year where vital rate changes, default = 15
# changepar: which vital rate to change, defaults to "none"
# pvec: multiplier for changed vital rate (i.e., 0.9 = 10% reduction), defaults to 0.5


source("simSealPop.R")
source("plotSimSealPop.R")

out1 <- simSeals(nyears=35, seals=4500, 
                   f=0.88, S0=0.35, S1=0.80, S2F=0.94, S2M=0.89,
                   changeyear = 16, changepar = "S2", pvec = 0.8,nreps=100)

plotSim(out1, nreps = 100, plotObs=TRUE)
