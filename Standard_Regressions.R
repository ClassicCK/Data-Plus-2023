library(readr)
library(glue)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(ggpubr)

variables <- read.csv("Data/variableData.csv") #Data about the conditions of each run

data <- read.csv("Data/transposedData.csv") #Data about the OD of each run. File is transposed and manually changed to minutes

dataLong <- data %>% gather("Time","OD",-Variable) #Converts to long

dataLong$Time <- substring(dataLong$Time,2,nchar(dataLong$Time)) #Removes the X in front of each time value

toAppend <- variables
for (i in 2:((length(dataLong$Variable)/200))){ #Repeats the variable data to be as long as dataLong
  toAppend <- rbind(toAppend,variables) #Appends variable data to toAppend N times
}

dataLong <- cbind(dataLong,toAppend) #Appends toAppend to dataLong to create a large long format dataframe

dataLong <- dataLong[order(dataLong$Variable),] #orders by variable, probably unecesarry
dataLong$Time <- as.numeric(dataLong$Time) #VERY NECESARRY 

blank <- dataLong %>% filter(dataLong$strain == "blank") #Seperates the runs by strain, can't seem to do this with the for loop below somehow
HV35 <-dataLong %>% filter(dataLong$strain == "HV35")
HV187 <-dataLong %>% filter(dataLong$strain == "HV187")
HV208 <-dataLong %>% filter(dataLong$strain == "HV208")
HV284 <-dataLong %>% filter(dataLong$strain == "HV284")
HV285 <-dataLong %>% filter(dataLong$strain == "HV285")
HV286 <-dataLong %>% filter(dataLong$strain == "HV286")
HV287 <-dataLong %>% filter(dataLong$strain == "HV287")
HV289 <-dataLong %>% filter(dataLong$strain == "HV289")
HV290 <-dataLong %>% filter(dataLong$strain == "HV290")

#for(strain in strainNames){
#  print(strain)
#  strainData <- dataLong %>% filter(dataLong$strain == strain)
#  print(strainData)
#}

strains <- list(HV35,HV187,HV208,HV284,HV285,HV286,HV287,HV289,HV290) #List of all strains
strainNames <- list("HV35","HV187","HV208","HV284","HV285","HV286","HV287","HV289","HV290") #List of names


linear <- as.formula(OD~Time)
exponential <- as.formula(OD~exp(Time))
logistic <- as.formula(OD ~ a/(1+exp(b-c * Time)))
gompertz <- as.formula(OD ~ a*exp(-exp(b-c*Time)))
richards <- as.formula(OD ~ a*(1+v*exp(k*(t-Time)))^(-1/v))
stannard <- as.formula(OD ~ a*(1+exp(-(l+k*Time)/p))^(-p))


for (i in 1:length(strains)){ #For each strain:
  strain <- strains[[i]] #selects the strain from strains
  
  print(strainNames[i]) #prints name for reference 
  
  strain$Variable <- as.factor(strain$Variable) #factors the Variable column soR does not create a bar graph 
  strain$techrep <- as.factor(strain$techrep)
  strain$biorep <- as.factor(strain$biorep)
  
  colnames(strain)[1] = "Run" #Renames "Variable" to "Run"
  
  strain$Time <- as.integer(strain$Time)/(60*60) #Convert to Hours
  #Convert to Hours
  
 
  #model <- lm(formula = linear, data = strain)
  
  #model <- glm(formula = exponential, data = strain)
  
  #model <- nls(formula = linear, data = strain, start = list(a = 1, b = 1, c = 1))
  
  model <- nls(formula = gompertz, data = strain, start = list(a = 1, b = 1, c = 1))
  
  
  print(model)
  
  r_squared <- summary(model)$r.squared
  
  print(r_squared)
  
  
}



