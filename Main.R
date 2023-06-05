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
merged_data <- merge(data_long, variables[c("variable", "strain", "condition")], by.x = "Run", by.y = "variable", all.x = TRUE)


unique_strains <- unique(variables$strain)

for (strainName in unique_strains) { #For each strain:
  
  print(strainName)
  
  # Filter the merged_data for the current strain only
  current_strain_data <- merged_data %>% filter(strain == strainNames[i])
  
  strainGraph <- ggplot(current_strain_data, aes(x = Time, y = OD, color = as.factor(Run))) +
    geom_line() + 
    labs(x = "Time (Seconds)", y = "Optical Density", title = paste("Time series plot for strain: ",strainNames[i],sep="")) + 
    theme_twoseventyeight +
    facet_wrap(~condition)
  
  ggsave(filename = paste("Figures/",strainNames[i],".png",sep=""),
         plot = strainGraph, units = "px", width = 3200, height = 1800, dpi = 300)
}

