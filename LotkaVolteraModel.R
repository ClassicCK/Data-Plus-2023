# Load Packages
library(tidyr)
library(dplyr)
library(FME)
library(readr)

# Set your Computer to run certain amount of iterations to solve
options(expressions = 50000)

# Load Data
variables <- read.csv("Data/variableData.csv") #Data about the conditions of each run

Volume <- rep(1,200)

variables <- cbind(variables, Volume)

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



strains <- list("HV35","HV187","HV208","HV284","HV285","HV286","HV287","HV289","HV290") #List of names
conditions <- list("no glu01","glu01")

# Model #
model <- function (pars, N_0 = 10, M_0 = 14231) {
  derivs <- function(time, y, pars) {
    with (as.list(c(pars, y)), {
      dN <- a * M^omega * N * (1 - (N / (b * M * beta)) - (m * N))
      dM <- (e * b * M^beta / N) - c * M^delta + sigma^2 * h^2 * ((a * M^(-1 - beta + omega) * (N * (1) * (beta - omega) + M^beta * omega)) / M)
      return(list(c(dN,dM)))
    }
    )
  }
  # Initial conditions
  y <- c(N = N_0, M = M_0)
  times <- c(seq(0, 14, 0.01)) 	# Need to set the correct time as 0 - # of Days
  out <- ode(y = y, parms = pars, times = times, func = derivs, maxsteps = 1e9, atol = 1e-9, rtol = 1e-9)
  as.data.frame(out)
}

cost_nm_model <- function (pars) {
  out <- model(pars)
  cost <- modCost(model = out, obs = data, x = "time", err = "sd")
  return(modCost(model = out, obs = trait.data, x = "time", err = "sd", cost = cost))
}

costmodel<- function(Npars){cost_nm_model(c(Npars, a=1.23*10^-4,b=8.22e+06))} # These are just random beginning set parameters for a and b, can caluclate by solving for intrinsic growth rate

# Define Boundaries for Variables #
upper_bounds <- list(c = Inf, e = Inf, omega = Inf, beta = Inf, delta = Inf, sigma = 1, h = 1, m = 1)
lower_bounds <- c(m = 0)

# Indiviudal Runs #
# Control
data <- dataLong %>%
  filter() %>% # determine what you may filter by
  group_by(Time) %>% # determine what you want to group by
  summarize(Density = mean(OD, na.rm = TRUE), SD = sd(OD, na.rm=TRUE)) # Set the correct column here for density

trait.data <- data %>%
  filter() %>% # determine what you may filter by
  group_by(Time) %>% # determine what you want to group by
  summarize(Size = mean(Volume, na.rm = TRUE), CV = sd(Volume, na.rm=TRUE)/mean(Volume, na.rm = TRUE), SD = sd(Volume, na.rm=TRUE), MIN=min(Volume, na.rm=TRUE)) # Will need to create a volume category in your data

data <- cbind(time = data$Time, N = data$Density, sd = rep(0.45, length(trait.data$SD)))
trait.data <- cbind(time = trait.data$Time, M = trait.data$Size, sd = rep(0.45, length(trait.data$SD)))



pars <- c(e = 24, omega = 0.937, c = 0.9, delta = 1.1, sigma = 0, h = 1, beta = -0.70281446, m = 1) # set initial parameters
fit <- modFit(f = costmodel, p = pars, method = "Port")#, upper = upper_bounds)#, lower = lower_bounds) # May or may not need to include bounds
final <- model(pars = c(coef(fit), a=1.23*10^-4,b=8.22e+06))
