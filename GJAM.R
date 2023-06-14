library(glue)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyverse)
library(gjam)


variables <- read.csv("Data/20181214_meta_trmB_all.csv") #Data about the conditions of each run

data <- read.csv("Data/allDataCompatible.csv") #Data about the OD of each run

# Convert the "Time" a duration object, because values go beyond 24 hours 
data$Time <- as.numeric(hms(data$Time))



data_long <- data %>% 
  pivot_longer(
    cols = -Time,  # Exclude the Time column
    names_prefix = "X",
    names_to = "Run",  # Column names go into the 'Run' column
    values_to = "OD"  # Corresponding values go into the 'OD' column
  )

data_long$Run <- as.numeric(data_long$Run)
merged_data <- merge(data_long, variables[c("variable", "strain", "condition","techrep","biorep")], by.x = "Run", by.y = "variable", all.x = TRUE)

data_wide <- merged_data %>%
  pivot_wider(names_from = strain, values_from = OD)

head(data_wide)

strain_names <- names(data_wide)[-c(1:5, ncol(data_wide))]
print(strain_names)



modelList <- list(
  ng = 2500,  # number of MCMC generations
  burnin = 500,  # burn-in period
  typeNames = rep("DA", length(strain_names)),  # model type for each response variable
  reductList = list(r = 8, N = 20)  # dimension reduction parameters
)

#for (strainName in strain_names) {
  #print(strainName)
  
  # Prepare data
  yData <- as.matrix(data_wide[, strain_names]) # using the strain_names vector to select response variables
  # Create xData as a data frame and convert columns to correct data types
  xData <- cbind(Time = as.numeric(data_wide$Time),
                 Run = as.numeric(data_wide$Run),
                 condition = as.numeric(as.factor(data_wide$condition))) # converting factors to numeric
  
  # Convert xData to a matrix
  xData <- as.matrix(xData)
  

  
  formula <- as.formula( ~ Time + condition + Run)
  
  # Apply GJAM
  gjamFit <- gjam(formula, xdata = xData, ydata = yData, modelList = modelList)
  
  # Check the result
  print(summary(gjamFit))
  
  # Create diagnostic plots
  par(mfrow = c(2,2))
  gjamPlot(output = gjamFit, plotPars = list(GRIDPLOTS = TRUE))
  
 # }

