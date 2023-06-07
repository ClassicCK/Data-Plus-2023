library(readr)
library(glue)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyverse)


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

merged_data$logOD <- log(merged_data$OD)
merged_data$condition[merged_data$condition == ""] <- 'No condition'


unique_strains <- unique(variables$strain)

for (strainName in unique_strains) { #For each strain:
  
  print(strainName)
  
  # Filter the merged_data for the current strain only
  current_strain_data <- merged_data %>% filter(strain == strainName)
  
  # Check if there are any non-NA values in the columns used for the model
  if (any(!is.na(current_strain_data$logOD)) & any(!is.na(current_strain_data$Time))) {
    # Fit the model only if there are any non-NA values
    model <- lm(logOD ~ Time, data = current_strain_data)
    
    # Generate a data frame for predictions
    newdata <- data.frame(Time = seq(min(current_strain_data$Time), max(current_strain_data$Time), length.out = 100))
    
    # Add the predicted values to the newdata
    newdata$logOD <- predict(model, newdata)
    
    # Transform back to original scale for plotting
    current_strain_data$OD <- exp(current_strain_data$logOD)
    newdata$OD <- exp(newdata$logOD)
    
    strainGraph <- ggplot(current_strain_data, aes(x = Time, y = OD, color = as.factor(Run), linetype = condition)) +
      geom_line() + 
      geom_smooth(method='lm') +
      labs(x = "Time (Seconds)", y = "Optical Density", title = paste("Time series plot for strain: ", strainName, sep="")) + 
      theme_twoseventyeight +
      
      facet_grid(rows = vars(biorep), cols = vars(techrep), switch = 'y', labeller = label_both)
    
    ggsave(filename = paste("Figures/", strainName, ".png", sep = ""),
           plot = strainGraph, units = "px", width = 3200, height = 1800, dpi = 300)
  } else {
    print(paste("Skipping strain", strainName, "due to no non-NA values"))
  }
}
