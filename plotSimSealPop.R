#DJFR: changed code to include nreps as an input, otherwise code fails if any other number but 1000 specified in sim
# EKJ changed code to add Orkney data in if plotObs = TRUE

plotSim <- function(out, nreps=1000, plotObs=TRUE){
  
  # purpose: simulation of a harbor seal population 
  # requires: tidyr and ggplot2
  # inputs:
    # out: a list of length 2 resulting from function simSeals 
    # plotObs: whether to plot expected no. of hauled out ind. default = TRUE
  # outputs: 
    # a ggplot of simulated seal population trajectories
  
  library(tidyr)
  library(ggplot2)
  library(readxl)
  library(dplyr)
  
  data <- read_excel("CountData.xlsx") %>%
    pivot_longer(cols = 2:32, names_to = "Year", values_to = "Count") %>%
    mutate(Year = as.numeric(Year) - 1988) %>%
    select(-`Site Area`) %>%
    na.omit() 
    
  N <- as.data.frame(out$Ntot)
  names(N) <- 1:ncol(N)
  N$Rep <- 1:nreps
  N <- gather(N, key = "Year", value="N", -Rep)
  
  O <- as.data.frame(out$Nobs)
  names(O) <- 1:ncol(O)
  O$Rep <- 1:nreps
  O <- gather(O, key = "Year", value="Nobs", -Rep)
  
  RoI <- out$RoI
  
  if(plotObs==TRUE){
  ggplot() +
    geom_line(data=N, aes(x=as.numeric(Year), y = N, group = Rep), color="gray60") +
    geom_violin(data = O, aes(x = as.numeric(Year) , y = Nobs, group = Year), 
                fill = "black") +
    geom_point(data = data, aes(x=as.numeric(Year), y = Count), color = "red",size=3) +
    theme_bw()+
    ylab("Number of seals") +
    xlab("Year") +
    geom_vline(xintercept = 11) +
    ggtitle(paste("Mean Rate of Increase", round(RoI, digits = 2), "Per Year from Year 11 to 35"))+
    theme(axis.title=element_text(size=14,face="bold"),axis.text = element_text(size = 12))+
    ylim(c(0, max(N$N)))}
  
  else 
    
    ggplot() +
    geom_line(data=N, aes(x=as.numeric(Year), y = N, group = Rep), color="gray60") +
    geom_violin(data = O, aes(x = as.numeric(Year), y = Nobs, group = Year), 
                fill = "black") +
    theme_bw()+
    ylab("Number of seals") +
    xlab("Year") +
    geom_vline(xintercept = 11) +
    ggtitle(paste("Mean Rate of Increase", round(RoI, digits = 2), "Per Year from Year 11 to 35"))+
    theme(axis.title=element_text(size=14,face="bold"),axis.text = element_text(size = 12))+
    ylim(c(0, max(N$N)))
  
}

