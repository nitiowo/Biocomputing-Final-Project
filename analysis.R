source("supportingFunctions.R")

# use functions from supportingFunctions.R to create and summarize the compiledData.csv
fileConverter("c:/Users/evelasco/Documents/GitHub/Biocomputing-Final-Project/countryY")
compiledCSV("c:/Users/evelasco/Documents/GitHub/Biocomputing-Final-Project")
dataSummary("c:/Users/evelasco/Documents/GitHub/Biocomputing-Final-Project", "compiledData.csv")


# Question 1
# In which country (X or Y) did the disease outbreak likely begin?
# We determined this by finding the country that had the higher number of infected individuals per day at the beginning of screening
# We visualized this data in a plot of each country's total infections over time

setwd("c:/Users/evelasco/Documents/GitHub/Biocomputing-Final-Project")
allData<-read.csv("compiledData.csv", header=TRUE)

# make data sets for each individual country
countryX<-allData[allData$Country=="countryX",]
countryY<-allData[allData$Country=='countryY',]

# find the list of unique days of screening
uniqueDays<-unique(allData$dayOfYear)

# make an empty data frame in which to inout the number of infected individuals per country over time
countTable<-data.frame(matrix(nrow=length(uniqueDays), ncol=3))
# make a vector with the proper column names
colnamescountries<-c("dayOfYear","countryX", "countryY")
# add the column names to the data frame
colnames(countTable)<-colnamescountries

# enter the unique days into the data frame
countTable$dayOfYear<-uniqueDays

# set the number of the infected individuals for country X to 0 
infectedX=0

# for country X
# loop through each row of the countTable
for(x in 1:nrow(countTable)){
  # loop through each row of the countryX dataframe
  for(i in 1:nrow(countryX)){
    # check if the day of year i in country X matches day of year x in Count table
    if(countryX$dayOfYear[i]==countTable$dayOfYear[x]){
      # find the sum of the row for the marker columns
      rowsum<-rowSums(countryX[i,3:12])
      # if the marker has a present, the rowsum > 0 and therefore the individual is infected
      if(rowsum > 0){
        # update infection total
        infectedX<-infectedX + 1
      }
    }
  }
  # add the total of infected individuals for the day to row x for country X 
  countTable$countryX[x]<-infectedX
}

# repeat for country Y
# set total infected individuals in country Y to 0
infectedY<- 0
# loop through each row in countTable
for(z in 1:nrow(countTable)){
  # loop through each row of country Y data set
  for(w in 1:nrow(countryY)){
    # check if the day of year w in country Y matches day of year z in Count table
    if(countryY$dayOfYear[w]==countTable$dayOfYear[z]){
      # find the sum of the row for the marker columns
      rowsum<-rowSums(countryY[w,3:12])
      # if the marker has a present, the rowsum > 0 and therefore the individual is infected
      if(rowsum > 0){
        # update infection total
        infectedY<-infectedY + 1
      }
    }
  }
  # add the total of infected individuals for the day to row z for country Y 
  countTable$countryY[z]<-infectedY
}


# make a plot that displays the linear trends for infection totals over time for country Y and Country X
library(ggplot2)
infectedGraph<-ggplot()+
  geom_line(data =countTable, aes(x=dayOfYear, y=countryX, color= "CountryX"), linewidth =1.5, color ='plum') +
  geom_line(data= countTable, aes(x=dayOfYear, y=countryY, color= "CountryY"), linewidth =1.5, color='skyblue') +
  xlab("Day of Year")+
  ylab("Number of Infected Individuals")+
  ggtitle("Total Infected Per Day by Country", subtitle="plum = country X, skyblue = country Y")+
  theme_minimal()+
  guides(color='none')

# display the graph
infectedGraph

# Answer 1
# The disease outbreak likely started in country X because they started having infections earlier than country Y as shown in the graph


# Question 2
# If Country Y develops a vaccine for the disease, is it likely to work for citizen of Country X?

# Making data frames with just the marker columns for Country X and Country Y
marker_columns_X <- countryX[, 3:12]
marker_columns_Y <- countryY[, 3:12]

# sum the column values to find how many of each marker is present in each country
sum_markers_X <- colSums(marker_columns_X)
sum_markers_X

sum_markers_Y <- colSums(marker_columns_Y)
sum_markers_Y


# make a new data frame to combine the marker sum counts for country X
df2<-data.frame(matrix(nrow=10, ncol=3))
# create a vector for column names
df2colnames<-c("marker", "country", "value")
# add column names to data frame
colnames(df2)<-df2colnames
# make a vector with marker names
markers<-colnames(marker_columns_x)
# add marker names to the data frame
df2$marker<-markers

# add the country name to the data frame
for(row in 1:nrow(df2)){
  df2$country[row]<-'X'
}

# add the marker counts to the data frame
df2$value<-sum_markers_X

# make a new data frame to combine the marker sum counts for country Y
df3<-data.frame(matrix(nrow=10, ncol=3))
# create a vector for column names
df3colnames<-c("marker", "country", "value")
# add column names to data frame
colnames(df3)<-df3colnames
# make a vector with marker names
markers<-colnames(marker_columns_Y)
# add marker names to the data frame
df3$marker<-markers

# add the country name to the data frame
for(row2 in 1:nrow(df3)){
  df3$country[row2]<-'Y'
}

# add the marker counts to the data frame
df3$value<-sum_markers_Y

# combine the data frames
totalDF<- rbind(df2, df3)

# load packages
library(ggplot2)

# graph the marker counts by Country
graph2<-ggplot(totalDF, aes(x=marker, y =value, fill=country)) +
  geom_bar(stat = 'identity', position='dodge')+
  theme_bw()+
  ggtitle('Marker Counts by Country')+
  xlab("Markers")+
  ylab("Count")

# Display Graph
graph2

# Answer 2
# A vaccine developed by Country Y would not likely work for Country X because the disease markers in each country
# because the markers are present in different quantities in each country

# in Country X, the primary markers present are markers 01-05.
# in Country Y, the primary markers present are markers 06-10.