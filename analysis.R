# clearing environment
rm(list = ls(all.names = TRUE))

# set workng directory & save path
setwd("C:/Users/student/Desktop/Notre Dame/Fall 23/Biocomp/Biocomputing-Final-Project")
directpath = getwd()

# loading functions
source("supportingFunctions.R")

# loading necessary packages
library(tidyverse)
library(gridExtra)

# converting txt files to csv files
txt2csv(directpath)

# compiling txt files into one
mergecsv(directpath)

# performing data summary
datasummary(allcsv)

# creating plots
countryplots(allcsv)


## 1. The outbreak is likely to have started in Country X as the incidence of cases started
##    earlier than Country Y (Day 120 vs. Day 140). 

## 2. A vaccine developed in Country Y is likely to not be effective for Country X. As shown
##    in the plots of the occurrences of the markers for each country, markers 1-5 are more
##    prevalent for Country X whereas markers 6-10 are more common for Country Y. Thus, the
##    vaccine developed in Country Y wouldn't target the same markers that are present in
##    Country X. 