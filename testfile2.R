setwd("~/Desktop/shell-lesson-data/exercises/Biocomputing-Final-Project")

# read in data
allData<-read.csv("allData.csv", header=TRUE)

## step 1-find number of screens
# find number of screens
numberofScreens<-nrow(allData)

## step 2-find percent infected
# make a variable for infected total
infected = 0
# make a dataset with just marker colums
marker_columns <- allData[, 3:12]
# for each row in dataset
for(i in 1:nrow(marker_columns)){
  # find the rowSum
  rowsum <- rowSums(marker_columns[i, ])
  # if the rowsum is greater than 0, a marker is present
  if(rowsum > 0){
    # increase the infected count by one
    infected = infected + 1
  }else{
    # if the row sum is 0, there are no markers present
    # do not update infected total
    
    #### technically not needed but idk if we should keep this block
    infected = infected + 0
  }
}

# calculate the total infected per screen
percentInfected <- (infected/numberofScreens) * 100

## step 3- find percent male or female

# make empty variables
male = 0
female = 0

# iterate through each row
for(i in 1:nrow(allData)){
  # find the columns that are 'male'
  if(allData$gender[i] == "male"){
    male = male + 1
  # find the columns that are female  
  }else if(allData$gender[i] == "female"){
    female = female + 1
  }
}

# find male percentage
percentMale <- (male/numberofScreens) * 100
percentMale

# find female percentage
percentFemale <- (female/numberofScreens) * 100
percentFemale

## step 4- age distribution of patients

# make a dataframe with the ages of the patients
ageData<- data.frame(allData$age)
# add proper column name
names(ageData) <- 'Patient.Age'
ageData

# load in graphing tools
library(ggplot2)
# make a distribution graph
ageGraph <- ggplot(data=ageData, aes(x=Patient.Age)) +
  geom_histogram(binwidth = 1, fill="purple", color="black") +
  theme_bw() +
  xlab("Patient Age") +
  ylab("Count")
# display graph
ageGraph





## test for each country
#### overall question 1

# make an empty variable
infected_X <- 0

# make a country X dataset
country_X <- allData[allData$country == 'X', ]
# make a count for total screens in country X
count_X <- nrow(country_X)
# make a dataset with just the marker columns
marker_columns_X<- country_X[, 3:12]

# iterate through each row of the marker columns data
for(i in 1:nrow(marker_columns_X)){
  # calculate the total for each row
  rowsum <- rowSums(marker_columns_X[i, ])
  # if the rowsum is greater than 0, there is at least one marker present
  if(rowsum > 0){
    # update the count
    infected_X = infected_X+1
  }else{
    infected_X= infected_X+0
  }
}

# find percent infected for country X
percentinfectedX <- (infected_X/count_X)*100
percentinfectedX

# repeat for country Y
infected_Y <- 0


country_Y <- allData[allData$country == 'Y', ]
count_Y <- nrow(country_Y)

marker_columns_Y <- country_Y[, 3:12]

for(i in 1:nrow(marker_columns_Y)){
  rowsum <- rowSums(marker_columns_Y[i, ])
  if(rowsum > 0){
    infected_Y = infected_Y+1
  }else{
    infected_Y= infected_Y+0
  }
}

# calculate percentage
percentinfectedY <- (infected_Y/count_Y)*100
percentinfectedY



#### final piece
#### question 2

sum_markers_X <- colSums(marker_columns_X)
sum_markers_X

sum_markers_Y <- colSums(marker_columns_Y)
sum_markers_Y


library(ggplot2)
library(reshape2)

# Convert the sum_markers_X and sum_markers_Y into data frames
df_X <- data.frame(marker = names(sum_markers_X), count = sum_markers_X, country = 'X')
df_Y <- data.frame(marker = names(sum_markers_Y), count = sum_markers_Y, country = 'Y')

# Combine the two data frames
combined_df <- rbind(df_X, df_Y)

# Reshape the data into a long format
long_df <- melt(combined_df, id.vars = c("marker", "country"))

# Plot the data
markerGraph<-ggplot(long_df, aes(x = marker, y = value, fill = country)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  xlab("Marker")+
  ylab("Count")+
  ggtitle("Comparison of Marker Counts between Countries X and Y") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle=300, vjust=0.6))

# display graph
markerGraph

othe

house 
