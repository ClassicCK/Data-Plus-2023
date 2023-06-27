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



r_squareds <- data.frame(c(0))

k <- 1
for (i in strains){
  for(j in conditions){
    r_squareds <- rbind(r_squareds,data.frame(c(0)))
    rownames(r_squareds)[k] = paste(i," with ", j, sep="")
    k <- k+1
  }
}

r2 <- c()
for (i in strains){ #For each strain:
  print(i) #prints name for reference 
  strain_unfiltered <- dataLong %>% filter(dataLong$strain == i)
  
  for(j in conditions){
    
    print(j)
    strain_runs <- strain_unfiltered %>% filter(strain_unfiltered$condition == j) #selects the strain from strains
    
    for (k in unique(strain_runs$Variable)){
      
      print(k)
      strain <- strain_runs %>% filter(strain_runs$Variable == k)
      
      strain$Variable <- as.factor(strain$Variable) #factors the Variable column soR does not create a bar graph 
      strain$techrep <- as.factor(strain$techrep)
      strain$biorep <- as.factor(strain$biorep)
      
      colnames(strain)[1] = "Run" #Renames "Variable" to "Run"
      
      strain$Time <- as.integer(strain$Time)/(60*60) #Convert to Hours
      #Convert to Hours
      
      strain[c("Time","OD")]
      

      AR <- arima(strain$OD, order = c(1,0,0))
      print(AR)
      AR_fit <- strain$OD - residuals(AR)

      plot_data <- data.frame(Time = strain$Time, OD = strain$OD, AR_fit = AR_fit)
      # Convert the "Time" column to numeric (if it's not already)
      plot_data$Time <- as.numeric(plot_data$Time)
      
      # Plot strain and AR_fit using ggplot2
      strainGraph <- ggplot(plot_data, aes(x = Time)) +
        geom_line(aes(y = OD, color = "Strain")) +
        geom_line(aes(y = AR_fit, color = "AR Fit"), linetype = "dashed") +
        labs(x = "Time", y = "Optical Density", title = paste("Time series plot for strain: ",i, " with ", j, "run:", k, sep="")) + theme_twoseventyeight
      
      ggsave(filename = paste("Figures/",i," with ", j, " run: ", k, ".png",sep=""),
             plot = strainGraph, units = "px", width = 3200, height = 1800, dpi = 300)
 
    } 
  }
}



