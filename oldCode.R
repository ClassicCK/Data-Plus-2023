library(ggplot2)
library(lubridate)
library(readr)

# Read the CSV file
data <- read.csv("Data/allDataCompatible.csv")

# Convert the 'Time' column
data$Time <- as.numeric(hms(data$Time))

# Make sure the columns are read as numbers
data$Column4 <- as.numeric(as.character(data[, 4]))

# Create the ggplot
co2 <- ggplot(data, aes(x = Time, y = Column4)) +
  geom_line() +
  labs(x = "Time (Seconds)", y = "Value", title = "Time Series Plot") +
  theme_minimal()

# Save the plot
ggsave(filename = "Figures/2.png",
       plot = co2, units = "px", width = 3200, height = 1800, dpi = 300)