library(readr)

variables <- read.csv("Data/20181214_meta_trmB_all.csv")

data <- read.csv("Data/20181214_trmB_all_data.csv")
  
  
strain <- variables$strain

blank <-c()
HV35 <-c()
HV187 <-c()
HV208 <-c()
HV284 <-c()
HV285 <-c()
HV286 <-c()
HV287 <-c()
HV289 <-c()
HV290 <-c()

for (i in length(strain)){
  assign(strain[i],variables$variable[i])
}