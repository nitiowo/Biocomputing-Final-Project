#James Magas
#2023-03-12
#Biocomputing Final Project
#analysis portion

#use source() function to load
#functions defined in supportingFunctions.R
#compile all data into single .csv file
#process data included in the entire data set
#to answer the two questions
#1. in what country (X or Y) did the disease outbreak
#likely begin?
#and
#2. If country Y develops a vaccine for the disease, is
#it likely to work for citizens of country X?
#think about we are on day 175 and now will this work in other country
#disease progression over time in each country
#since spans from 55 days ago to today
#all 10 are associated with disease, so even just 1
#pressense or absense of those
#two patients with similar microsatelites probably same or close
#if very different probably different strain
#screens run on day 120
#day 121 there is another file for that, etc
#country y is in different format
#all.data allows us to make graph etc. and gives us an output example

#First I will initiate ggplot and load in the "allData.csv" that was created in the first part

#clear previous contents
rm(list=ls())

# load a package every time you wish to use it
library(ggplot2)
library(cowplot)

#load the combined data from all countries and name it "data"
data<-read.csv("allData.csv")
head(data)


#now i can create a dataframe for Country X and Country Y with the applicable Titles and empty rows
dfcx <- data.frame("Marker_01_X" = integer(), "Marker_02_X" = integer(), "Marker_03_X" = integer(), "Marker_04_X" = integer(), "Marker_05_X" = integer(), "Marker_06_X" = integer(), "Marker_07_X" = integer(), "Marker_08_X" = integer(), "Marker_09_X" = integer(), "Marker_10_X" = integer(), "Cumulative_Pos_X" = integer(), "DayofYear_X" = integer(), stringsAsFactors = FALSE)
dfcx

dfcy <- data.frame("Marker_01_Y" = integer(), "Marker_02_Y" = integer(), "Marker_03_Y" = integer(), "Marker_04_Y" = integer(), "Marker_05_Y" = integer(), "Marker_06_Y" = integer(), "Marker_07_Y" = integer(), "Marker_08_Y" = integer(), "Marker_09_Y" = integer(), "Marker_10_Y" = integer(), "Cumulative_Pos_Y" = integer(), "DayofYear_Y" = integer(), stringsAsFactors = FALSE)
dfcy
head(data)


#Since I will not need gender or age columns to answer the following questions I can remove them. I can always append them back in later if needed for other statistical analysis.
#I can remove them using grepl() or "grep logic"
#I could also use
#data$gender = NULL
#data$age = NULL

data = data[,!grepl("age", names(data))]
data = data[,!grepl("gender", names(data))]
head(data)


#now I will use loops to look through each row in "data"
#I will be using these loops to place any positive cases from country X into a data frame
#And I will be placing any positive cases from country Y into a separate data frame
#I will do this by first checking the row to see what country the data pertains to
#Then I will look through each marker in that row to determine if there is a positive marker
#As soon as a positive marker is detected, the nested for loop will "break", the data will be placed into the next row in the correspoding countries dataframe
#And the next row will be looked at by the starting for loop
for (j in 1:length(data$country)){
  if (data$country[j] == "X"){
    for (i in 1:(length(colnames(data))-2)){
      if (data[j,i] == 1){
        dfcx[nrow(dfcx) + 1,] <- data[j,]
        break
      }
    }
  }
  else{
    for (i in 1:(length(colnames(data))-2)){
      if (data[j,i] == 1){
        dfcy[nrow(dfcy) + 1,] <- data[j,]
        break
      }
    }
  }
}
head(dfcx)
tail(dfcx)
head(dfcy)
tail(dfcy)

#Now that I have all the positive cases from country X and country Y in separate data frames, I can
#Analyze the total number of markers and subjects that were listed for each day of the year
#I will end up with one row for each day of the year, that will include the cumulative total up through that day for both the individual markers and the subjects
#I will create new data files for each country (dfcxcum and dfcycum)

#First I will get the range of days over which testing occured to determine the size of the blank data frames

#determine what the first day of the year testing was performed
first_day = min(data$dayofYear)

#determine what the last day of the year testing was performed
last_day = max(data$dayofYear)

#create a vector that is the length of the range (this should match with the DayofYear column in dfcxcum and dfcycum)
#since we are comparing x and y we will want both to have the same number of days, even if nobody tested positive on one of the days
#days<-c(first_day:last_day)
#days
#so we know that for our analysis we will have a data frame that has a length of rows and length of columns based on the number of days of testing and the number of markers tested

#To get the cumulative values for each column for each day I will first use the cumsum() function on each column and safe these newly created vectors with new vector names
#Once I have created vectors of cumulated sums for each column in dfcx and dfcy I will combine them all together using cbind()

#First processing everything for country X:
mar1cumx <- cumsum(dfcx$Marker_01_X)
mar2cumx <- cumsum(dfcx$Marker_02_X)
mar3cumx <- cumsum(dfcx$Marker_03_X)
mar4cumx <- cumsum(dfcx$Marker_04_X)
mar5cumx <- cumsum(dfcx$Marker_05_X)
mar6cumx <- cumsum(dfcx$Marker_06_X)
mar7cumx <- cumsum(dfcx$Marker_07_X)
mar8cumx <- cumsum(dfcx$Marker_08_X)
mar9cumx <- cumsum(dfcx$Marker_09_X)
mar10cumx <- cumsum(dfcx$Marker_10_X)

#since each subject is positive in the data frame, in order to get the cumulative number of
#positive subjects I will create a new column titled "cum_pos_subjects_X" and fill it with 1's
#then I can apply "cumsum()" to that column

subjcumx <- cumsum(rep(1,length(dfcx$Cumulative_Pos_X)))
day_of_yearx <- dfcx$DayofYear_X


markercumulativex <- data.frame(cbind(mar1cumx,mar2cumx,mar3cumx,mar4cumx,mar5cumx,mar6cumx,mar7cumx,mar8cumx,mar9cumx,mar10cumx,subjcumx,day_of_yearx))
head(markercumulativex)
tail(markercumulativex)


#Now processing everything for country Y:
mar1cumy <- cumsum(dfcy$Marker_01_Y)
mar2cumy <- cumsum(dfcy$Marker_02_Y)
mar3cumy <- cumsum(dfcy$Marker_03_Y)
mar4cumy <- cumsum(dfcy$Marker_04_Y)
mar5cumy <- cumsum(dfcy$Marker_05_Y)
mar6cumy <- cumsum(dfcy$Marker_06_Y)
mar7cumy <- cumsum(dfcy$Marker_07_Y)
mar8cumy <- cumsum(dfcy$Marker_08_Y)
mar9cumy <- cumsum(dfcy$Marker_09_Y)
mar10cumy <- cumsum(dfcy$Marker_10_Y)

#since each subject is positive in the data frame, in order to get the cumulative number of
#positive subjects I will create a new column titled "cum_pos_subjects_X" and fill it with 1's
#then I can apply "cumsum()" to that column

subjcumy <- cumsum(rep(1,length(dfcy$Cumulative_Pos_Y)))
day_of_yeary <- dfcy$DayofYear_Y


markercumulativey <- data.frame(cbind(mar1cumy,mar2cumy,mar3cumy,mar4cumy,mar5cumy,mar6cumy,mar7cumy,mar8cumy,mar9cumy,mar10cumy,subjcumy,day_of_yeary))
head(markercumulativey)
tail(markercumulativey)


#Now I need to obtain the last row for each day in the dataframe "markercumulativex" and "markercumulativey"
#"x_totals_by_day" will be the data frame that will contain a day by day cumulative total for all markers, and for positive subjects for country x,
#"y_totals_by_day" will be the data frame for country y
#I'll check each row and if day_of_yearx increments, I'll save the i-1 row to the new dataframe
#If day_of_yearx increments by more than one I will need to print the i-1 row to the new dataframe



x_totals_by_day <- data.frame("Marker_01_X" = integer(), "Marker_02_X" = integer(), "Marker_03_X" = integer(), "Marker_04_X" = integer(), "Marker_05_X" = integer(), "Marker_06_X" = integer(), "Marker_07_X" = integer(), "Marker_08_X" = integer(), "Marker_09_X" = integer(), "Marker_10_X" = integer(), "Cumulative_Pos_X" = integer(), "DayofYear_X" = integer(), stringsAsFactors = FALSE)
#go from 120-175
#check if it is in day_of_yearx
#or go through each row
#using dayxcurr and dayxprev to know when the day increments. I will then save the row before the increment.
#if increment is >1 then I will save the row before the increment however many times the distance between the two was.
#initial state is set to the first day in "dataset" and all other columns are set to 0 in case there were no cases on the first day of data collection for either of the countries
dayxcurr = data$dayofYear[1]
dayxprev = data$dayofYear[1]
#this will be the initial vector that can be used if there were no positive cases on day 1
#it can be used for both country x and y
initvec <- c(rep(0, 11),first_day)


#for loop will go through each row in markercumulativex
#dayxcurr will be initially saved as whatever the day of the year is for the current row in the loop
#in this case I need to save the row from the previous loop (or as all zeros other than the day if it was the first time through the loop)
#I need to determine how many days were skipped to know how many rows to make
#if not the first time through, then it will take the previous days values and paste them on the next row, since those values should be the same if there were no positive cases for the following day

for (i in 1:length(markercumulativex$day_of_year)){
  if (i == 1){
    dayxprev = data$dayofYear[1]
  }
  else{
    dayxprev = markercumulativex$day_of_yearx[i-1]
  }
  dayxcurr = markercumulativex$day_of_yearx[i]
  if (dayxcurr != dayxprev){
    diff = dayxcurr - dayxprev
    if (i == 1){
      for (j in 1:diff){
        x_totals_by_day[nrow(x_totals_by_day)+1,] <- c(rep(0, 11),(first_day + j - 1))
      }
    }
    else{
      for (j in 1:diff){
        x_totals_by_day[nrow(x_totals_by_day)+1,] <- markercumulativex[i-1,]
      }
    }
  }
  if (i == length(markercumulativex$day_of_year)){
    x_totals_by_day[nrow(x_totals_by_day)+1,] <- tail(markercumulativex,1)
  }
}




y_totals_by_day <- data.frame("Marker_01_Y" = integer(), "Marker_02_Y" = integer(), "Marker_03_Y" = integer(), "Marker_04_Y" = integer(), "Marker_05_Y" = integer(), "Marker_06_Y" = integer(), "Marker_07_Y" = integer(), "Marker_08_Y" = integer(), "Marker_09_Y" = integer(), "Marker_10_Y" = integer(), "Cumulative_Pos_Y" = integer(), "DayofYear_Y" = integer(), stringsAsFactors = FALSE)


#go from 120-175
#check if it is in day_of_yeary
#or go through each row
#using dayycurr and dayyprev to know when the day increments. I will then save the row before the increment.
#if increment is >1 then I will save the row before the increment however many times the distance between the two was.
#initial state is set to the first day in "dataset" and all other columns are set to 0 in case there were no cases on the first day of data collection for either of the countries
dayycurr = data$dayofYear[1]
dayyprev = data$dayofYear[1]

#for loop will go through each row in markercumulativey
#dayycurr will be initially saved as whatever the day of the year is for the current row in the loop
#in this case I need to save the row from the previous loop (or as all zeros other than the day if it was the first time through the loop)
#I need to determine how many days were skipped to know how many rows to make
#if not the first time through, then it will take the previous days values and paste them on the next row, since those values should be the same if there were no positive cases for the following day

for (i in 1:length(markercumulativey$day_of_year)){
  if (i == 1){
    dayyprev = data$dayofYear[1]
  }
  else{
    dayyprev = markercumulativey$day_of_yeary[i-1]
  }
  dayycurr = markercumulativey$day_of_yeary[i]
  if (dayycurr != dayyprev){
    diff = dayycurr - dayyprev
    if (i == 1){
      for (j in 1:diff){
        y_totals_by_day[nrow(y_totals_by_day)+1,] <- c(rep(0, 11),(first_day + j - 1))
      }
    }
    else{
      for (j in 1:diff){
        y_totals_by_day[nrow(y_totals_by_day)+1,] <- markercumulativey[i-1,]
      }
    }
  }
  if (i == length(markercumulativey$day_of_year)){
    y_totals_by_day[nrow(y_totals_by_day)+1,] <- tail(markercumulativey,1)
  }
}

#If there is a gap in day_of_yearx somewhere from 120-175 I need to insert a row in using data from previous row so that there are no gaps in days and so both country x and country y have the same nubmer of rows
tail(x_totals_by_day)
tail(y_totals_by_day)
head(x_totals_by_day)
head(y_totals_by_day)




#there will also be 20 variables created: each of the 10 markers will have a running total for each country
#this way we can get the average number of instances that each marker was positive for each country
#this will allow us to decide if both countrys can use a vaccine made by one country or the other

#It may be most efficient to do this with a separate loop, one that ignores dayofYear column and just looks at country column
#each iteration copies row with country X to 



#also will need to get an average value for the markers for each country
#probably best to display that with ten couples of bar plots to see how similar those numbers are

#strsplit(x,"_")
#split function up into functions
#get a list back, first element is first part with vector of first part
#what i want to split is first argument
#second element is what i want to use to split (essentially removes that second element
#and splits there)

#for part 1 I'm going to want to show how many cases for country x and country y occurred over time, accumulating them
#day to day
#this should show that one country had more cases than the other country first
#this should indicate what country had the disease first


#scores<-rbind(0,scores) - to get a row of 0s

#for graph dayofYear is going to be x-axis and number of positive cases is
#going to be y-axis
#I will have two line graphs that share the same x and y axis
#but y value will change differently for each country
#I will need to create a line of scores vs time for MSU and another for UW
#remove x tick marks
#make graph start at 0 in x-axis and y-axis

ggplot(Positive_Cases, aes(x = dayofYear)) +
  geom_line(aes(y = X), linewidth = 2, color = "darkgreen") +
  geom_line(aes(y = Y), linewidth = 2, color = "purple") +
  xlab("Time (Day of Year)") +
  ylab("Positive Cases") +
  theme(axis.ticks.x = element_blank())

