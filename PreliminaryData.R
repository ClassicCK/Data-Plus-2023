library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# Read the CSV files
data <- read.csv("~/Desktop/6-2-23/Data/allDataCompatible.csv")
label_data <- read.csv("~/Desktop/6-2-23/Data/20181214_meta_trmB_all.csv")

# Convert the "Time" a duration object, because values go beyond 24 hours 
data$Time <- as.numeric(hms(data$Time))

# Melt the data to get it into long format. Exclude the "Time" and "Blank" columns
melted_data <- data %>% gather(variable, Value, -Time, -Blank)

# Check if there's a match between variable and Label columns
if(!all(melted_data$variable %in% label_data$Label)) {
  stop("Mismatch between variable and Label columns")
}

# Merge labels with the melted data by "variable"
merged_data <- left_join(melted_data, label_data, by = c("variable" = "Label"))

# Get the list of unique strains
strains <- unique(merged_data$strain)

# Loop over strains to create plots
for(strain in strains){
  
  # Filter the data for the current strain
  strain_data <- merged_data %>% filter(strain == strain)
  
  # Check if there are multiple variables for this strain
  if(n_distinct(strain_data$variable) > 1){
    # Create the ggplot with color aesthetic
    co2 <- ggplot(strain_data, aes(x = Time, y = Value, colour = variable)) +
      geom_line() +
      labs(x = "Time", y = "Value", title = paste0("Time Series Plot for Strain ", strain)) +
      theme_minimal()
  } else {
    # Create the ggplot without color aesthetic
    co2 <- ggplot(strain_data, aes(x = Time, y = Value)) +
      geom_line() +
      labs(x = "Time", y = "Value", title = paste0("Time Series Plot for Strain ", strain)) +
      theme_minimal()
  }
  
  # Save the plot
  ggsave(filename = paste0("~/Desktop/6-2-23/Figures/", strain, ".png"),
         plot = co2, units = "px", width = 3200, height = 1800, dpi = 300)
}