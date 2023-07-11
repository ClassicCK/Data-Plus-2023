library(readr)
library(glue)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(ggpubr)
library(deSolve)
library(purrr)
library(reshape2) # library for reshaping data (tall-narrow <-> short-wide)

library(minpack.lm)

variables <- read.csv("Data/variableData.csv") #Data about the conditions of each run

data <- read.csv("Data/transposedData.csv") #Data about the OD of each run. File is transposed and manually changed to minutes

dataLong <- data %>% gather("Time","OD",-Variable) #Converts to long

dataLong$Time <- substring(dataLong$Time,2,nchar(dataLong$Time)) #Removes the X in front of each time value


variables$condition[variables$condition != "glu01"] <- "no glu01"

toAppend <- variables
for (i in 2:((length(dataLong$Variable)/200))){ #Repeats the variable data to be as long as dataLong
  toAppend <- rbind(toAppend,variables) #Appends variable data to toAppend N times
}

dataLong <- cbind(dataLong,toAppend) #Appends toAppend to dataLong to create a large long format dataframe

dataLong <- dataLong[order(dataLong$Variable),] #orders by variable, probably unecesarry
dataLong$Time <- as.numeric(dataLong$Time) #VERY NECESARRY 

strains <- list("HV35","HV187","HV208","HV284","HV285","HV286","HV287","HV289","HV290") #List of names
conditions <- list("no glu01","glu01")
models <- list("Linear","Exponential","Logistic","Gompertz","Richards","Stannard")

#Defines equations of fit
logistic_equation <- function(time, state, parameters) {

    r = parameters$r
    K = parameters$K
    
    # Define the differential equation
    dOD <- r * state * (1 - state / K)
    
    # Return the derivative
    return(list(dOD))
}



r2 <- c()
for (i in strains){ #For each strain:
  print(i) #prints name for reference 
  strain_unfiltered <- dataLong %>% filter(dataLong$strain == i)
  
  for(j in conditions){
    
    print(j)
    strain_runs <- strain_unfiltered %>% filter(strain_unfiltered$condition == j) #selects the strain from strains
    
    for (k in unique(strain_runs$Run)){
      
      print(k)
      strain <- strain_runs %>% filter(strain_runs$Variable == k)
    
      strain$Variable <- as.factor(strain$Variable) #factors the Variable column soR does not create a bar graph 
      strain$techrep <- as.factor(strain$techrep)
      strain$biorep <- as.factor(strain$biorep)
      
      colnames(strain)[1] = "Run" #Renames "Variable" to "Run"
      
      strain$Time <- as.integer(strain$Time)/(60*60) #Convert to Hours
      #Convert to Hours
      
      ssq = function(parms){
      
        initial <- c(strain$OD[1])
        t <- seq(min(strain$Time), max(strain$Time))
        
        parms <- list(r=0.01,K=1)
        out <- ode(y=initial,times=t,func=logistic_equation,parms=parms)
        plot(out)
        
        outdf=data.frame(out)
        outdf=outdf[outdf$time %in% strain$Time,]
        # Evaluate predicted vs experimental residual
        preddf=melt(outdf,id.var="time",variable.name="Run",value.name="OD")
        expdf=melt(strain,id.var="Time",variable.name="Run",value.name="OD")
        
        preddf <- preddf[complete.cases(preddf), ]  # Remove rows with NAs
        expdf <- expdf[complete.cases(expdf), ]
        
        preddf$OD <- as.numeric(preddf$OD)  # Convert OD column to numeric
        expdf$OD <- as.numeric(expdf$OD)  # Convert OD column to numeric
        
        
        ssqres=preddf$OD-expdf$OD
        # return predicted vs experimental residual
        return(ssqres)
      }
      
      parms <- c(r=0.01,K=0.5)
      # fitting
      fitval <- nls.lm(par=parms,fn=ssq)
      summary(fitval)
    
    
    } 
  }
}



