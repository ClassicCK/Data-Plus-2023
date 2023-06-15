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


variables$condition[variables$condition != "glu01"] <- "no glu01"

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
conditions <- list("no glu01","glu01")

#Defines equations of fit
linear <- as.formula(OD~Time)
exponential <- as.formula(OD~exp(Time))
logistic <- as.formula(OD ~ a/(1+exp(b-c * Time)))
gompertz <- as.formula(OD ~ a*exp(-exp(b-c*Time)))
richards <- as.formula(OD ~ a*(1+v*exp(k*(t-Time)))^(-1/v))
stannard <- as.formula(OD ~ a*(1+exp(-(l+k*Time)/p))^(-p))

for (i in 1:length(strains)){ #For each strain:
  for(j in conditions){
    
    print(j)
    strain <- strains[[i]] %>% filter(strains[[i]]$condition == j) #selects the strain from strains
    
    print(strain)
    
    print(strainNames[i]) #prints name for reference 
    
    strain$Variable <- as.factor(strain$Variable) #factors the Variable column soR does not create a bar graph 
    strain$techrep <- as.factor(strain$techrep)
    strain$biorep <- as.factor(strain$biorep)
    
    colnames(strain)[1] = "Run" #Renames "Variable" to "Run"
    
    strain$Time <- as.integer(strain$Time)/(60*60) #Convert to Hours
    #Convert to Hours
    
    
    #Makes a model based on the formulas above
    model <- lm(formula = linear, data = strain)
    #model <- glm(formula = exponential, data = strain)
    #model <- nls(formula = logistic, data = strain, start = list(a = 1, b = 1, c = 1))
    #model <- nls(formula = gompertz, data = strain, start = list(a = 1, b = 1, c = 1))
    
    print(model)
    
    strain$Predicted <- predict(model) #Makes predictions based on the model
    
    #Makes graph. Linetype is split by growth condition right now. Change "condition" to see other graphs
    
    strainGraph <- ggplot(strain, aes(x = Time, y = OD, color = Run, linetype = condition)) +
      
    geom_point(size = 0.2) +
    
    geom_line(aes(y = Predicted), color = "blue") +
    
    stat_regline_equation(label.y = 1, aes(label = ..rr.label..)) +
    
    #facet_grid(rows = vars(biorep),cols=vars(techrep),switch='y',labeller = label_both) +
    labs(x = "Time (Hours)", y = "Optical Density", title = paste("Time series plot for strain: ",strainNames[i],sep="")) + theme_twoseventyeight
  
    r_squared <- summary(model)$r.squared
    
    print(r_squared)
    
    #Saves graph.
    ggsave(filename = paste("Figures/",strainNames[i]," with ", j, ".png",sep=""),
           plot = strainGraph, units = "px", width = 3200, height = 1800, dpi = 300)
  }
}



