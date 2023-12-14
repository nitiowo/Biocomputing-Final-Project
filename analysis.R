# Load in the functions
source("supportingFunctions.R")

# Call the function to convert files to CSV
ConvertToCSV("C:/Users/nbark/Documents/Rproject/countryY")

# Call the function to compile the dataset and add a row for country and day of the year
CompileAndWriteCSV("C:/Users/nbark/Documents/Rproject", "compiled_data.csv", NA_option = "keep" )

# Summarize statistics from the compiled data 
SummarizeCompiledData("compiled_data.csv")

# Read in compiled data for analysis
data <- read.csv("compiled_data.csv")

# Load required packages
library(ggplot2)
library(dplyr)

# Make a new column that interprets the marker data and spits out an indication of the infection status of each individual
for (i in 1:length(data$gender)) {
  if (sum(data[i, 3:12]) > 0) {
    data$infection_status[i] = 1
  } else {
    data$infection_status[i] = 0
  }
}


# Get summary statistics for each country by day
epidemicdata <- data %>%
  group_by(country, dayofYear) %>%
  summarise(incidence = sum(infection_status))

# Plot data for question 1
ggplot(epidemicdata, aes(x=dayofYear, y=incidence)) +
  geom_line(aes(color = country, linetype = country)) +
  labs(x ="Day", y = "Disease Incidence") +
  scale_color_manual(values = c("darkred", "steelblue")) +
  theme_bw()

# Get summary statistics for each marker by country
MarkerSum <- data %>%
  group_by(country) %>%
  summarize(
    Count = c(sum(marker01), sum(marker02), sum(marker03), sum(marker04), sum(marker05),
              sum(marker06), sum(marker07), sum(marker08), sum(marker09)),
  )

# Add marker number to summary statistics
Marker <- data.frame(Marker = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9))

MarkerData <- cbind(MarkerSum, Marker)

# Plot data for question 2
ggplot(data=MarkerData, aes(x=Marker, y=Count, fill=country)) +
  geom_bar(stat="identity")+
  scale_x_continuous(breaks = 1:9)+
  theme_bw()

# Question 1: The disease likely originated in country X, because disease incidence appeared first in country 
#X, and then only after 20 days did people in country y start being infected. Based on the available data, 
#it appears that the disease is going to stay in each country for the time being, as both countries are at an
#all time high incidence at the ends of the available data period.

# Question 2: A vaccine that works for country Y is not likely to work for country X. This is because
#the prevalence of disease markers between the two countries is highly varied, with country X having
#a large portion of their population with markers 1-5, and country Y where there is a large portion
#of the population with markers 6-9. Because differences in the markers present indicates differences
#in the proteins within the disease, as well as the immune response to the presence of the disease,
#an infection with different markers will likely require seperate vacinations.

