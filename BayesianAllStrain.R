library(glue)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyverse)
library(brms)  
library(rstan)
library(tidybayes)

options(brms.backend = "cmdstanr")


variables <- read.csv("Data/20181214_meta_trmB_all.csv") #Data about the conditions of each run

data <- read.csv("Data/20181214_trmB_all_data.csv") #Data about the OD of each run

# Convert the "Time" a duration object, because values go beyond 24 hours 
#data$Time <- as.numeric(hms(data$Time))



data_long <- data %>% 
  pivot_longer(
    cols = -Time,  # Exclude the Time column
    names_prefix = "X",
    names_to = "Run",  # Column names go into the 'Run' column
    values_to = "OD"  # Corresponding values go into the 'OD' column
  )

data_long$Run <- as.numeric(data_long$Run)
merged_data <- merge(data_long, variables[c("variable", "strain", "condition","techrep","biorep")], by.x = "Run", by.y = "variable", all.x = TRUE)

glu01_data <- merged_data %>% filter(condition == "glu01")


  
  # Filter the merged_data for the current strain only

glu01_data <- glu01_data %>%
  filter(!(strain %in% c("blank", "")))


set.seed(123)

# Now, randomly select 70% of the data for training
train_data <- glu01_data %>% sample_frac(.7)

  # Define the non-linear formula for brm
f <- bf(
  OD ~ A + (B - A)/(1 + exp((log(Time) - xmid)/scal)), 
  A ~ 1 + (1|biorep) + (1|techrep) + (1|strain), 
  B ~ 1 + (1|biorep) + (1|techrep) + (1|strain), 
  xmid ~ 1 + (1|biorep) + (1|techrep) + (1|strain), 
  scal ~ 1 + (1|biorep) + (1|techrep) + (1|strain), 
  nl = TRUE
)

# Fit the Bayesian non-linear model
fit <- brm(f, data = glu01_data, family = gaussian(),
           prior = c(set_prior("normal(0,10)", nlpar = "A"), 
                     set_prior("normal(0,10)", nlpar = "B"), 
                     set_prior("normal(0,10)", nlpar = "xmid"), 
                     set_prior("normal(0,10)", nlpar = "scal")),
           chains = 5, cores = 2, control = list(max_treedepth = 15))

# save model for later use or inspection
saveRDS(fit, "Models/AllStrains70.rds")

# Predict and visualize the fit
pred_data <- add_predicted_draws(glu01_data, fit)
pred_data$Time <- as.POSIXct(pred_data$Time, origin = "1970-01-01")

# Compute .lower and .upper columns (e.g., for a 90% prediction interval)
pred_summary <- pred_data %>%
  group_by(condition, .draw, Time, OD) %>%
  summarise(
    across(.prediction, list(median = median, lower = ~quantile(., prob = 0.05), upper = ~quantile(., prob = 0.95)), .names = "{.col}.{.fn}")
  ) %>%
  ungroup()

graph <- ggplot(pred_summary, aes(x = Time)) +
  geom_ribbon(aes(ymin = .prediction.lower, ymax = .prediction.upper, fill = condition), alpha = 0.6) +
  geom_line(aes(y = .prediction.median, color = condition), linewidth = 1.1) +
  geom_point(aes(y = OD), size = 0.2, color = "black") +
  labs(x = "Time (Seconds)", y = "Optical Density", title = "Time series plot for all strains") + 
  theme_bw()

ggsave(filename = "Figures/AllStrains_Bayesian.png",
       plot = graph, units = "px", width = 3200, height = 1800, dpi = 300)

