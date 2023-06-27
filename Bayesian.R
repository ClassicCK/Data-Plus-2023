library(glue)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyverse)
library(brms)  


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


unique_strains <- unique(variables$strain)

for (strainName in unique_strains) { #For each strain:
  
  print(strainName)
  
  # Filter the merged_data for the current strain only
  current_strain_data <- merged_data %>% filter(strain == strainName)
  
  if(all(is.na(current_strain_data$biorep)) & all(is.na(current_strain_data$techrep))) { next }
  
  # Define the non-linear formula for brm
  f <- bf(OD ~ A + (B - A)/(1 + exp((log(Time) - xmid)/scal)), 
          A ~ 1, B ~ 1, xmid ~ 1, scal ~ 1,
          nl = TRUE)
  
  # Fit the Bayesian non-linear model
  fit <- brm(f, data = current_strain_data, family = gaussian(),
             prior = c(set_prior("normal(0,10)", nlpar = "A"), 
                       set_prior("normal(0,10)", nlpar = "B"), 
                       set_prior("normal(0,10)", nlpar = "xmid"), 
                       set_prior("normal(0,10)", nlpar = "scal")),
             chains = 2, cores = 2, control = list(max_treedepth = 15))
  
  # save model for later use or inspection
  saveRDS(fit, paste0("Models/", strainName, ".rds"))
  
  # Predict and visualize the fit
  pred_data <- add_predicted_draws(current_strain_data, fit)
  pred_data$Time <- as.POSIXct(pred_data$Time, origin = "1970-01-01")
  
  strainGraph <- ggplot(pred_data, aes(x = Time)) +
    geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = condition), alpha = 0.6) +
    geom_line(aes(y = .prediction, color = condition), size = 1.1) +
    geom_point(aes(y = OD), size = 0.2, color = "black") +
    labs(x = "Time (Seconds)", y = "Optical Density", title = paste("Time series plot for strain: ",strainName,sep="")) + 
    theme_bw() +
    facet_grid(rows = vars(biorep),cols=vars(techrep),switch='y',labeller = label_both)
  
  ggsave(filename = paste("Figures/",strainName,"_Bayesian.png",sep=""),
         plot = strainGraph, units = "px", width = 3200, height = 1800, dpi = 300)
}

