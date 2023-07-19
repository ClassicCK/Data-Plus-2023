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
      
      ssq <- function(parms) {
        initial <- c(strain$OD[1])
        t <- seq(min(strain$Time), max(strain$Time))
        
        ode_equation <- function(time, state, parameters) {
          r <- parameters["r"]
          K <- parameters["K"]
          
          # Define the differential equation
          dOD <- r * state * (1 - state / K)
          
          # Return the derivative
          return(list(dOD))
        }
        
        parms <- c(r = parms[1], K = parms[2])
        out <- lsoda(y = initial, times = t, func = ode_equation, parms = parms)
        
        # Check if out contains any missing or NaN values
        if (any(is.na(out)) || any(is.nan(out))) {
          ssqres <- rep(1e6, length(strain$OD))  # Return a large sum of squared residuals
        } else {
          # Extract full predicted values
          pred_OD <- out[, -1]
          pred_times <- out[, 1]
          
          # Interpolate predicted values at the specific time points
          interp_OD <- approx(pred_times, pred_OD, xout = strain$Time)$y
          
          # Calculate the sum of squared residuals
          ssqres <- interp_OD - strain$OD
        }
        
        return(ssqres)
      }
      
      parms <- c(r=0.01,K=0.5)
      # fitting
      fitval <- nls.lm(par=parms,fn=ssq)
      summary(fitval)
    
    
    } 
  }
}



