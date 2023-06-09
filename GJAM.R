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
  ng = 10,  # number of MCMC generations
  burnin = 5,  # burn-in period
  typeNames = rep("DA", length(strain_names)),  # model type for each response variable
  REDUCT = FALSE  # dimension reduction parameters
)

#for (strainName in strain_names) {
#print(strainName)

# Prepare data
yData <- as.matrix(data_wide[, strain_names]) # using the strain_names vector to select response variables
# Create xData as a data frame and convert columns to correct data types
xData <- cbind(Time = as.numeric(data_wide$Time),
               Run = as.factor(data_wide$Run),
               condition = as.factor(data_wide$condition)) # converting factors to numeric

# Convert xData to a matrix
xData <- as.matrix(xData)

set.seed(111)

train_index <- sample(1:nrow(xData), size = 0.7 * nrow(xData))

# Split the data into training and testing sets
xtrain <- xData[train_index, ]
xtest <- xData[-train_index, ]

ytrain <- yData[train_index, ]
ytest <- yData[-train_index, ]


formula <- as.formula( ~ Time + condition + Run)

xtrain <- as.data.frame(xtrain)
xtest <- as.data.frame(xtest)

complete_rows <- complete.cases(xtrain)
complete_rowsT <- complete.cases(xtest)


# Subset both xtrain and ytrain using this vector
xtrain <- xtrain[complete_rows, ]
ytrain <- ytrain[complete_rows, ]

xtest <- xtest[complete_rowsT, ]
ytest <- ytest[complete_rowsT, ]

# Apply GJAM
gjamFit <- gjam(formula, xdata = xtrain, ydata = ytrain, modelList = modelList)

# Check the result
print(summary(gjamFit))

# Create diagnostic plots
par(mfrow = c(2,2))
gjamPlot(output = gjamFit, plotPars = list(GRIDPLOTS = TRUE))




newdata <- list(xdata = xtest, nsim = 200)
predictions <- gjamPredict(gjamFit, newdata = newdata)

# Base ggplot
base <- ggplot()

# make plot for first strain
strain1 <- base +
  geom_point(aes(x = yData[,1],  # observed values for first strain
                 y = predictions$muY[,1])) +  # predicted values for first strain
  labs(x = "Observed", y = "Predicted", title = paste0(strain_names[1]))

# make plot for second strain
strain2 <- base +
  geom_point(aes(x = yData[,2],  # observed values for second strain
                 y = predictions$muY[,2])) +  # predicted values for second strain
  labs(x = "Observed", y = "Predicted", title = paste0(strain_names[2]))

# Use the ggarrange function from the ggpubr package to arrange the two plots side by side
ggarrange(strain1, strain2, ncol = 2)




# }

