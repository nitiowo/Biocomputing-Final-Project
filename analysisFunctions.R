#2 analysis.R

# Load supporting functions
source("supportingFunctions.R")

# Convert delimited files to CSV
convertToCSV("path/to/delimited/files")

# Compile CSV files into a single file
compileCSV("path/to/csv/files", removeNA=TRUE)

# Read compiled data
compiledData <- read.csv("path/to/csv/files/compiled_data.csv")

# Summarize the data
summary <- summarizeData(compiledData)

# Question 1
# Plot the number of infected individuals over time for all countries in the dataset
# Custom function found in supportingFunctions.R
plotNumberOfInfectedForAllCountries("path/to/csv/files/compiled_data.csv")

# The infection started in Country X. When graphed, we see that Country X had more infected on day 120 then Country Y, thus we can presume that the infection began in Country X.

# Question 2
# Determine Vaccine compatability by comparing marker frequencies
# Custom function found in supportingFunctions.R
compareMarkerFrequencies("path/to/csv/files/compiled_data.csv")

#The vaccine for Country Y will not work for Country X as they do not have the same markers, thus the immune system will not react the same. 

# Stephan Lukashev

#This project assumes the installation of ggplot2 and reshape2
