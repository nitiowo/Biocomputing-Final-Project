## load functions from supportingFunctions.R
source("supportingFunctions.R")

# run txt_to_csv script with countryY as the directory and space as the delimiter
txt_to_csv("countryY", "space")

# run compile script with countryX as the directory
compile("countryX")

# run summary script with allData.csv as the datafile
summary("allData.csv")

#### question 1: in which country did the disease outbreak begin? ----

# load tidyverse for ggplot
library(tidyverse)
# read in dataset
alldata <- read.csv("allData.csv")
# subset data
positive <- alldata[apply(alldata[, 3:12], 1, sum) > 0, ]
# plot the data of only positive individuals
ggplot(data = positive, aes(x = country, y = dayofYear, color = country)) +
  geom_jitter()

#### question 1 answer ----
# 