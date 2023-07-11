fit <- readRDS("Models/AllStrains.rds")

# Subset your data for a particular strain
run_to_predict <- 123  # replace 123 with the run you want to predict for
single_run_data <- glu01_data %>% filter(Run == run_to_predict)

# Predict and visualize the fit
pred_data <- add_predicted_draws(single_strain_data, fit)
pred_data$Time <- as.POSIXct(pred_data$Time, origin = "1970-01-01")

# Compute .lower and .upper columns (e.g., for a 90% prediction interval)
pred_summary <- pred_data %>%
  group_by(condition, .draw, Time, OD) %>%
  summarise(
    across(.prediction, list(median = median, lower = ~quantile(., prob = 0.05), upper = ~quantile(., prob = 0.95)), .names = "{.col}.{.fn}")
  ) %>%
  ungroup()

# Plot
graph <- ggplot(pred_summary, aes(x = Time)) +
  geom_ribbon(aes(ymin = .prediction.lower, ymax = .prediction.upper, fill = condition), alpha = 0.6) +
  geom_line(aes(y = .prediction.median, color = condition), linewidth = 1.1) +
  geom_point(aes(y = OD), size = 0.2, color = "black") +
  labs(x = "Time (Seconds)", y = "Optical Density", title = paste("Time series plot for strain: ", strain_to_predict)) + 
  theme_bw()

ggsave(filename = paste("Figures/", run_to_predict, "_Bayesian.png", sep = ""),
       plot = graph, units = "px", width = 3200, height = 1800, dpi = 300)