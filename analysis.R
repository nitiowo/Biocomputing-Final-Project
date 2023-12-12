### analysis.R
## R Final Project 
# Allie Wu and Emily Rao
setwd("~/Biocomputing-Final-Project/")
library(ggplot2)

## Answers to questions
# Supporting code down below and figures attached
## Question 1: It’s likely that the outbreak started in country X because at the
# beginning of the graph, it’s clear that country X starts having infected 
# patients before country Y. The infections in country X start on day 122, while
# the infections in country Y start around day 140. We can thus conclude that the
# outbreak began in country X.

## Question 2: If Country Y develops a vaccine, it is unlikely that it will work
# for citizens of Country X because the distribution of biomarkers is very 
# different between the two countries. Markers 6-10 are most prevalent in 
# Country Y, with frequencies of around 30%, whereas Country X barely has any 
# markers 6-10. Moreover, Country X has mainly markers 1-5, with frequencies of 
# around 40%, while Country Y has comparably fewer markers 1-5. Since Country X 
# had the outbreak first, and Country Y caught the disease later, the difference
# in genetic markers could be due to the bacteria evolving over time.


# # Load in functions from supportingFunctions
source("supportingFunctions.R")

# Convert txt to csv using first function
# Only did it for country Y because X was already all csv
convertFiles("~/Biocomputing-Final-Project/countryY")

# Combine all csv from country X using second function
compileFiles("~/Biocomputing-Final-Project/countryX")
# Combine all csv from country Y
compileFiles("~/Biocomputing-Final-Project/countryY")

# Combine compiled data from both countries into one big table
countryX_compiled <- read.csv("~/Biocomputing-Final-Project/countryX/compiled_data.csv")
countryY_compiled <- read.csv("~/Biocomputing-Final-Project/countryY/compiled_data.csv")
allData <- rbind(countryX_compiled, countryY_compiled)

# Run summary analysis from third function
summary()
# Number of Screens Run: 39744
# Percent Male: 50.15348 %
# Percent Female: 49.84652 %
# Percent of Infected Patients: 56.55696 %

## Question 1 Supporting Code
# Reads data
allData <- read.csv("allData.csv", header=T)

# Makes a new data frame with 3 columns, day of year, country and infected status where
# 1 is infected and 0 is not infected 
num_infected <- data.frame(matrix(ncol=3, nrow=nrow(allData)))
colNames <- c("dayofYear", "country", "infectedStatus")
colnames(num_infected) <- colNames

# Makes for loop 
for (i in 1:nrow(num_infected)){
  # Adds in data for dayofYear in new df by row
  num_infected$dayofYear[i] <- allData$dayofYear[i]
  # Adds in data for country in new df by row
  num_infected$country[i] <- allData$country[i]
  # Sees if patient is infected then appends 1 or 0 to new infectedStatus column
  if (any(allData[i, 3:12]) == 1){
    num_infected$infectedStatus[i] <- 1
  } else {
    num_infected$infectedStatus[i] <- 0
  }}
num_infected

# Creates list of unique days 
DoY <- unique(num_infected$dayofYear)

# Add up total infected per day for country X
dailyX <- data.frame(matrix(ncol=2, nrow=length(DoY)))
colnames(dailyX) <- c("dayofYear", "sumInfected")
num_infectedX <- num_infected[num_infected$country == "X", c(1,3)]

# For loop                     
for (i in 1:length(DoY)){
  dailyX$dayofYear[i] <- DoY[i]
  dailyX$sumInfected[i] <- sum(num_infectedX[num_infectedX$dayofYear==DoY[i],2])
}

# Add up total infected per day for country Y
dailyY <- data.frame(matrix(ncol=2, nrow=length(DoY)))
colnames(dailyY) <- c("dayofYear", "sumInfected")
num_infectedY <- num_infected[num_infected$country == "Y", c(1,3)]

# For loop                       
for (i in 1:length(DoY)){
dailyY$dayofYear[i] <- DoY[i]
dailyY$sumInfected[i] <- sum(num_infectedY[num_infectedY$dayofYear==DoY[i],2])
}
                         
# dailyY is country Y total infected by day
# dailyX is country X total infected by day

# Makes a plot
Infected_byDay -> ggplot()+
  geom_line(data=dailyX, aes(x=dayofYear, y=sumInfected), color="darkred") +
  geom_line(data=dailyY, aes(x=dayofYear, y=sumInfected), color="darkgreen") + 
  ggtitle("Number Infected per Day by Country", subtitle = "Country X=red, Country Y = green") +
  xlab("Day") +
  ylab("Infected per Day") +  
  scale_x_continuous(breaks = seq(min(c(dailyX$dayofYear, dailyY$dayofYear)), 
                                  max(c(dailyX$dayofYear, dailyY$dayofYear)), 
                                  by = 10))
Infected_byDay

## Question 2 Supporting Code
# Make new dataset with allData and new column called num_infected.infectedStatus
merged_allData <- data.frame(allData, num_infected$infectedStatus)

# Make empty lists
marker <- c()
country <- c()
frequency <- numeric()
markers_frequency <- data.frame()

# Sets conditions for for loop
yes_infected <- merged_allData[merged_allData$num_infected.infectedStatus == 1, ]
country_name <- unique(merged_allData$country)
marker_names <- colnames(subset(yes_infected, select = 3:12))

# For loop to iterate through each country and marker and calculate frequency
for (i in country_name){
  for (j in marker_names){
    infected_bycountry <- subset(yes_infected, country == i)
    frequency <- sum(infected_bycountry[ ,j]) / nrow(infected_bycountry)
    markers_frequency <- rbind(markers_frequency, data.frame(marker = j, country = i, frequency = frequency, stringsAsFactors = FALSE))
  }
}

# Plots histogram 
markers_distr <- ggplot(data = markers_frequency, aes(x = marker, y = frequency, fill = country)) +
  geom_bar(stat="identity", position="dodge") +
  ggtitle("Marker Frequency by Country") +
  xlab("Marker") +
  ylab("Frequency") 

markers_distr




