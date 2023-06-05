library(readr)
library(glue)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

variables <- read.csv("Data/20181214_meta_trmB_all.csv") #Data about the conditions of each run

data <- read.csv("Data/allDataCompatible.csv") #Data about the OD of each run

# Convert the "Time" a duration object, because values go beyond 24 hours 
data$Time <- as.numeric(hms(data$Time))



blank <- variables %>% filter(variables$strain == "blank") #Seperates the runs by strain
HV35 <-variables %>% filter(variables$strain == "HV35")
HV187 <-variables %>% filter(variables$strain == "HV187")
HV208 <-variables %>% filter(variables$strain == "HV208")
HV284 <-variables %>% filter(variables$strain == "HV284")
HV285 <-variables %>% filter(variables$strain == "HV285")
HV286 <-variables %>% filter(variables$strain == "HV286")
HV287 <-variables %>% filter(variables$strain == "HV287")
HV289 <-variables %>% filter(variables$strain == "HV289")
HV290 <-variables %>% filter(variables$strain == "HV290")

strains <- list(blank,HV35,HV187,HV208,HV284,HV285,HV286,HV287,HV289,HV290) #List of all strains
strainNames <-list("blank","HV35","HV187","HV208","HV284","HV285","HV286","HV287","HV289","HV290")


for (i in 1:length(strains)){ #For each strain:
  strain <- strains[[i]]
  
  print(strainNames[i])
  runsOfStrain <- data.frame(data$Time) #Create a new dataframe with the Time column

  for(run in strain$variable){ #For each numbered run of that strain:
    runData <-data[, (as.integer(run)+2)] 
    runsOfStrain <- cbind(runsOfStrain,runData) #Appends that numbered run from the data
    colnames(runsOfStrain)[colnames(runsOfStrain) == "runData"] = run #Renames the column

  }
  
  runsOfStrain_long <- gather(runsOfStrain, key = "Strain", value = "OD", -data.Time)
  
  strainGraph <- ggplot(runsOfStrain_long, aes(x = data.Time, y = OD, color = Strain)) +
    geom_line() + 
    labs(x = "Time (Seconds)", y = "Optical Density", title = paste("Time series plot for strain: ",strainNames[i],sep="")) + theme_twoseventyeight
  
  ggsave(filename = paste("Figures/",strainNames[i],".png",sep=""),
         plot = strainGraph, units = "px", width = 3200, height = 1800, dpi = 300)
    
}
