
#Analysis Script
setwd("~/Desktop/R Biocomputing/Biocomputing-Final-Project")
library(ggplot2)
library(cowplot)
source('Supporting_Functions.R')

# This is the analysis script that calls the supporting functions in order to analyze the given data

# CSV CONVERSION
# First, we must make sure both directories have csv files
# If the files are already in csv format, the code will tell us so.
csv_conversion('CountryX')
csv_conversion('CountryY')

# CSV COMPILE
# We must compile all the csv files from the directories we choose.
# Options regarding NA will pop up for the user
csv_compile(c('countryX','countryY'))

# SUMMARIZE FUNCTION
# We will summarize all the data in the compiled csv file
data <- read.csv('allData.csv', header = TRUE, sep = ',')
countingCases(data)

#Question 1: In which country (X or Y) did the disease outbreak likely begin?
#Based on the summary found above, it is likely that the disease outbreak began in country X. Country X had over double the amount of infected patients than country Y, suggesting that the disease originated from country X and was able to spread very quickly before reaching country Y. The disease itself seems impartial to gender, and effects relatively the same percentage of females as it does males. 
# Import the data
data_country_x <- data[data$country == "X", ]
data_country_y <- data[data$country == "Y", ]

# Counting Individual Markers by iterating through each row
marker_counts_x <- integer(10)
marker_counts_y <- integer(10)

for (marker_index in 1:10) {
  marker_col <- marker_index + 2  # Aligning with the actual column in the dataset, as we are starting two over in the data set =, column 3
  for (row in 1:nrow(data_country_x)) {
    if (data_country_x[row, marker_col] == 1) {
      marker_counts_x[marker_index] <- marker_counts_x[marker_index] + 1
    }
  }
  
  for (row in 1:nrow(data_country_y)) {
    if (data_country_y[row, marker_col] == 1) {
      marker_counts_y[marker_index] <- marker_counts_y[marker_index] + 1
    }
  }
}

#Display the counts for each country
print("Marker counts for country X:")
for (i in 1:length(marker_counts_x)) {
  print(paste("Marker", i,"total is", marker_counts_x[i]))
}
print("Marker counts for country Y:")
for (i in 1:length(marker_counts_y)) {
  print(paste("Marker", i,"total is", marker_counts_y[i]))
}

# Create a data frame from the marker_counts_x and marker_counts_y vectors
marker_data <- data.frame(
  Marker = rep(1:10, times = 2), #Create a repeating sequence for marker number
  Country = factor(rep(c("Country X", "Country Y"), each = 10)), #created variable factors that repeats "Country X" 10 times than Country Y for the next 10
  Count = c(marker_counts_x, marker_counts_y) #Puts the two marker_counts(x and y) into one vector
)

# Use ggplot2 to create the bar plot
ggplot(marker_data, aes(x = factor(Marker), y = Count, fill = Country)) +
  geom_bar(stat = "identity", position = position_dodge()) + #Position_dodge places Country X and Y bars next to each
  scale_fill_manual(values = c("lightblue", "darkred")) + #Filling the country bars in light blue and dark red
  labs(title = "Marker Counts by Country",
       x = "Marker",
       y = "Total Count") +
  theme_minimal()

#Question 2: If country Y develops a vaccine for the disease, is it likely to work for citizens of country x?
  
#  It is likely that if country Y develops a vaccine for the disease, it is less likely for the vaccine to work on country X. This is because most of the markers with a high amount of infected patients in country Y, have a low amount of infected patients in country X. For example, country Y would likely make a vaccine that deals with markers 6,7,8,9,and 10 because there are higher counts of infected patients found in these markers. This is not the case for country X, which has its lowest amounts of infected patients in markers 6,7,8,9, and 10. Instead, country X has its highest counts of infected patients in markers 1,2,3,4,and 5 which is where country Y has their least infected amount of patients. 

