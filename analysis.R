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



mar1cum <- cumsum(dfcx$Marker_01_X)
mar2cum <- cumsum(dfcx$Marker_02_X)
mar3cum <- cumsum(dfcx$Marker_03_X)
mar4cum <- cumsum(dfcx$Marker_04_X)
mar5cum <- cumsum(dfcx$Marker_05_X)
mar6cum <- cumsum(dfcx$Marker_06_X)
mar7cum <- cumsum(dfcx$Marker_07_X)
mar8cum <- cumsum(dfcx$Marker_08_X)
mar9cum <- cumsum(dfcx$Marker_09_X)
mar10cum <- cumsum(dfcx$Marker_10_X)

#since each subject is positive in the data frame, in order to get the cumulative number of
#positive subjects I will create a new column titled "cum_pos_subjects_X" and fill it with 1's
#then I can apply "cumsum()" to that column

subjcumx <- cumsum(rep(1,length(dfcx$Cumulative_Pos_X)))
day_of_year <- dfcx$DayofYear_X


markercumulative <- cbind(mar1cum,mar2cum,mar3cum,mar4cum,mar5cum,mar6cum,mar7cum,mar8cum,mar9cum,mar10cum,subjcumx,day_of_year)
head(markercumulative)
tail(markercumulative)

#using a for loop to go through each row and determine if the row is for country X or country Y
#this will help determine what set of totals to add the total to
for (j in length(data$"country")){
  if (data[j,data$country[j]] == "X"){
    xdata <- data$country[j]
    }
  else{
    ydata <- data[j,]
  }
}
head(xdata)
head(ydata)




tail(data)
head(data)
#to make the code flexible I will have to create a way to change the number of rows in new output column
for (j in 1:100){
  if (data$"11"[j] == "X"){
    for (i in 1:10){
      if (data[j,i] == 1){
        #from here we know that column i corresponds to x and has a positive marker that corresponds to the i marker
        #append this data onto the new marker name that is "marker" + i
        print("Yes")
        
      }
    }
    if (data[j,i]<1){
      print("positive")
    }
    else{
      print("Negative")
    }
  }
  else{
    print("0")
  }
}

#need the plus 1 since day 120 will be day 0 and day 175 will be day 55

#I want to repdroduce a data frame similar to data. however it will not include gender and age
#First column will consist of the day of year, second column will cosist of the country, third column will consist of the running total of people from that country that are positive, and remaining columns
#will consist of running total of positive markers for each country

#create running total not only for each country that has positive people
#but also running totals for each countrys marker positives





#make vector that is column 3-12 for each row
#write value of each subset 
#write it to a specific row, column will always be the same, but the row will be different
#write it to row corresponding to dayofYear row

#if data$country == x, data#marker01 == 1 and data#country == x increment marker01x by 1

#so our new data frame will have length(days) number of rows and 13 columns, 1 column for the dayofYear, 1 column for country X cumulative total positive cases, 1 column for country Y cumulative total
#positive cases
#each time through the loop it will check if a marker was positive, if there was a positive marker it will check if country x or country y was listed, and
#depending on the country it will increment positive cases for that country


#there will also be 20 variables created: each of the 10 markers will have a running total for each country
#this way we can get the average number of instances that each marker was positive for each country
#this will allow us to decide if both countrys can use a vaccine made by one country or the other

#It may be most efficient to do this with a separate loop, one that ignores dayofYear column and just looks at country column
#each iteration copies row with country X to 

#create a new data frame with the below headings and create empty vectors for each column using the length of Day_of_Year vector
Positive_Cases<-data.frame("Day_of_Year" = numeric(length(data$dayofYear)), "X" = numeric(length(data$dayofYear)), "country_x_pos_cases" = numeric(length(data$dayofYear)),"country_x_pos_cases" = numeric(length(data$dayofYear)), stringsAsFactors = FALSE)
head(Positive_Cases)

#create a variable that can be added to for each country number of cases and that can be incremented
countryxprev = 0
countryxcurr = 0

countryyprev = 0
countryycurr = 0
#create a loop that checks each row in "data" and accumulates positive cases for the correct country for the correct day
#the accumulated cases are added to the correct column and row each time the loop increments
#also will need to get an average value for the markers for each country
#probably best to display that with ten couples of bar plots to see how similar those numbers are
#if case is has a positive marker then check country, if country x, then add to each marker and the number of people that are positive
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

data
head(data)
for(j in 1:length(data$dayofYear)){
  if(data$country[j] == "X"){
    countryxprev = data[j,1]
    countryxcurr = countryxprev + countryxcurr
    #need to determine if row was a positive person, and if so add a value to 1 to runnning total
    
    data$pos_cases[j,1] = uwcurr
    scores[j,2] = msucurr
    scores[j,1] = data[j,1]
  } else{
    msuprev = data[j,3]
    msucurr = msuprev + msucurr
    scores[j,2] = msucurr
    scores[j,3] = uwcurr
    scores[j,1] = data[j,1]
  }}

#create a row of zeroes at the top so that there is a starting time of zero and starting scores of zero
#scores<-rbind(0,scores)
#finally print the data frame "scores" to make sure everything looks correct
scores



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

