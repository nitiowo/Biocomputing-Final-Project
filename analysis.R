# Introduction to Biocomputing Final Project
# analysis.R
# authors: Qiushi Peng, and Justin Detweiler

# import library
library(ggplot2)

### set up work directory 
# (please use your own work directory)
work_directory <- "/Users/pengqiushi/Documents/Biocomputing/Biocomputing-Final-Project/"
# directories of country X and Y
X_directory <- paste0(work_directory, "countryX")
Y_directory <- paste0(work_directory, "countryY")

setwd(work_directory)
source("./supportingFunctions.R")


### Step1: convert all .txt files in a directory to .csv files
TxtToCsv("./countryY")


### Step2: combine the data from CountryX and CountryY
# allows the user to decide the purpose of the NA rows
# (please run the following codes separately and pay attention to the prompt)
setwd(work_directory)
Combined_CSV(X_directory, Y_directory, work_directory)


### Step3: data summary
setwd(work_directory)
# read in data
allData <- read.csv("./All_Data_Both.csv", header = TRUE) # if you get error here, please rerun the codes in Step2.
Data_Summary(allData)


################################################################################


##################
### Question 1 ###
##################

# add a column which indicates if a person get infected or not
allData$infected = 0

for (i in 1:nrow(allData)) {
  for (j in 3:12) {
    if (allData[i, j] == 1) {
      allData[i,"infected"] = 1
      }
  }
}

## create a new dataframe to record changes in infection rates for each country

# initialize a new dataframe
infection_rate <- data.frame(
  country = character(), 
  dayofYear = numeric(), 
  infectionRate = numeric(), 
  stringsAsFactors = FALSE)

# get all unique country and date combinations
unique_countries <- unique(allData$country)
unique_days <- unique(allData$dayofYear)

# calculate infection rates for each country and date
for (current_country in unique_countries) {
  for (current_day in unique_days) {
    # extract data for specific countries and dates
    subset_allData <- subset(allData, country == current_country & dayofYear == current_day)
    
    # calculate infection rates
    if (nrow(subset_allData) > 0) {
      infectionRate <- sum(subset_allData$infected) / nrow(subset_allData)
      infection_rate <- rbind(infection_rate, data.frame(country = current_country, 
                                                         dayofYear = current_day, 
                                                         infectionRate = infectionRate))
    }
  }
}

# make a plot
ggplot(data = infection_rate, aes(x = dayofYear, y = infectionRate, color = country)) +
  geom_line() +
  labs(
    title = "Each country's infection rate according to day",
    x = "Day of Year",
    y = "Infection Rate",
    color = "Country") +
  theme_bw()

### Answer to Question 1: In "X" country the disease outbreak began.
### Because the infection rate in country "X" is always higher than that in country "Y" on each day.



##################
### Question 2 ###
##################

# get the data from *infected* people
infectedData <- allData[allData$infected == 1,]

# initialize a new dataframe
marker_freq <- data.frame(
  marker = character(), 
  country =  character(),
  frequency = numeric(), 
  stringsAsFactors = FALSE)

# calculate frequency of each marker for each country
for (current_country in unique_countries) {
  for (current_marker in colnames(infectedData)[3:12]) {
    # extract data for specific countries
    subset_infectedData <- subset(infectedData, country == current_country)
    
    # calculate frequency of each marker
    freq <- sum(subset_infectedData[,current_marker]) / nrow(subset_infectedData)
    marker_freq <- rbind(marker_freq, data.frame(marker = current_marker,
                                                 country = current_country, 
                                                 frequency = freq))
  }
}

# make a plot to show frequency of each marker for each country

ggplot(data = marker_freq, aes(x = marker, y = frequency, fill = country)) +
  geom_col(position = position_dodge()) +
  theme_bw() +
  labs(
    title = "Markers found in each country",
    x = "Marker",
    y = "Frequency",
    fill = "Country")
        
### Answer to Question 2: It seems that the vaccine developed in Country Y will 
  # not work for citizens of Country X.
### This assumption is made because we saw significant differences in frequency 
  # of each marker in these two countries.


