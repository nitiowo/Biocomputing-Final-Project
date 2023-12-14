#James Magas and Kate Jackowski
#2023-Dec-13
#Biocomputing Final Project

#####Please read next three blocks of comments before grading#######

#The graphs that support the questions will appear toward the bottom
#I have included the answers here to reduce the amount of searching required to find them
#The directory at line 47, 51, 56, and 67 will need to be properly set before executing the code
#In supportingFunctions.R the two filepaths leading to the all.csv file on line 73 need to be updated
#Then if you hold down ctrl and hold down enter until R gets through all of the code the graphs should appear after about 10-20 seconds
#Main reason for this delay is the for loop around line 100


#Answer to Question 1: Supported with graph produced by code towards bottom
#As shown in the graph to the right, Country X has new cases from the first day of testing until the last day of testing.
#Country Y, however did not have any positive cases until about day 140 of testing.
#This indicates that the disease outbreak likely began in Country X.

#Answer to Question 2: Supported with graph produced by code towards bottom
#As shown by the bar graph, there are more occurrences of marker 4 and 5 for country Y than
#there are for country X, and there are far fewer occurrences of markers 7 - 10 for
#country Y than there are for country X. This indicates that a vaccine developed by country Y
#will most likely be targeting a mutated form of the disease-causing agent, rather than the
#form of the agent afflicting country X. Based on this evidence it is reasonable to say that
#the vaccine is unlikely to work as well (if at all) for the citizens of Country X as it does
#for the citizens of Country Y.



#clear previous contents
rm(list=ls())

#set working directory to the folder containing this file and the supportingFunctions.R file
#setwd("C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project")


#set working directory to the folder where allData.csv is contained
setwd("C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project")

#load a package every time you wish to use it

source("supportingFunctions.R")
library(ggplot2)
library(cowplot)


#Use the functions created in supportingFunctions.R to accomplish data transformation and compilation

###Use custom function convertdirectorytocsv() to convert all .txt files in countryY directory to .csv files
#Usage: convertdirectorytocsv(directory_containing_country_.txt_files)
convertdirectorytocsv("C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project/countryY")

##use custom function to compile all files in country X database
#Usage: many2onecsv(directory_containing_country_.csv_files)
many2onecsv("C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project/countryX/")

#use custom function to compile all files in country Y database and append them to the compiled data created from country X database
#This function can continued to be called, by replacing the directory name with whatever country has a directory of compiled .csv files
#Usage: many2onecsv(directory_containing_country_.csv_files)
many2onecsv("C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project/countryY/")






#####custom function that converts the data in all.csv and creates a data frame for each graph used to support answers to the questions
#data frames produces will be final_df_for_analysis and final_dt_for_bar_plot
#new directory needs to be set before running
#set wd to directory with the all.csv file
setwd("C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project")


#load the combined data from all countries and name it "data"
data<-read.csv("all.csv")
head(data)


#now i can create a dataframe for Country X and Country Y with the applicable Titles and empty rows
dfcx <- data.frame("Marker_01_X" = integer(), "Marker_02_X" = integer(), "Marker_03_X" = integer(), "Marker_04_X" = integer(), "Marker_05_X" = integer(), "Marker_06_X" = integer(), "Marker_07_X" = integer(), "Marker_08_X" = integer(), "Marker_09_X" = integer(), "Marker_10_X" = integer(), "Cumulative_Pos_X" = integer(), "DayofYear_X" = integer(), stringsAsFactors = FALSE)
dfcx

dfcy <- data.frame("Marker_01_Y" = integer(), "Marker_02_Y" = integer(), "Marker_03_Y" = integer(), "Marker_04_Y" = integer(), "Marker_05_Y" = integer(), "Marker_06_Y" = integer(), "Marker_07_Y" = integer(), "Marker_08_Y" = integer(), "Marker_09_Y" = integer(), "Marker_10_Y" = integer(), "Cumulative_Pos_Y" = integer(), "DayofYear_Y" = integer(), stringsAsFactors = FALSE)
dfcy


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
#As soon as a positive marker is detected, the nested for loop will "break", the data will be placed into the next row in the corresponding countries dataframe
#And the next row will be looked at by the starting for loop
for (j in 1:length(data$Country)){
  if (data$Country[j] == "X"){
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

length(colnames(data))
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

#for loop will go through each row in marker cumulatively
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
head(x_totals_by_day)
tail(x_totals_by_day)
head(y_totals_by_day)
tail(y_totals_by_day)

#Cumulative positive cases for country X will be created next
cum_total_x <- c(x_totals_by_day$Cumulative_Pos_X)

#Cumulative positive cases for country Y will be created next
cum_total_y <- c(y_totals_by_day$Cumulative_Pos_Y)


#Now I will create a new column filled with the country designation (X or Y), and of the same length as the columns in x_totals_by_day / y_totals_by_day
#Since the length of the column is going to be the number of days of testing I can create a new vector for the day of the year called "doy"

doy <- c(first_day:last_day)

#Then I will create another data frame for showing why a vaccine developed by country Y might not be as effective for country X
#This data frame will be called marker_summary
#It will consist of 10 columns listing the name of each tested marker and two rows labeled Country X and Country Y
#The data used for each cell will be the total number of positive markers corresponding to that columns marker and the country for which the total was calculated 

vector_x = rep.int("x", length(doy))
vector_y = rep.int("y", length(doy))
doy
x_totals_by_day
#Now I will append these vectors onto their respective countries using cbind

x_totals_by_day <- cbind(x_totals_by_day,vector_x)
head(x_totals_by_day)
y_totals_by_day <- cbind(y_totals_by_day,vector_y)
head(y_totals_by_day)

#Now I will create a final data frame and place the country Y rows after the country X rows using rbind()
#First I will have to rename the columns so they match

colnames(x_totals_by_day) <- c("Marker_01","Marker_02","Marker_03","Marker_04","Marker_05","Marker_06","Marker_07","Marker_08","Marker_09","Marker_10","Cum_Pos","day_of_year","Country")

head(x_totals_by_day)

colnames(y_totals_by_day) <- c("Marker_01","Marker_02","Marker_03","Marker_04","Marker_05","Marker_06","Marker_07","Marker_08","Marker_09","Marker_10","Cum_Pos","day_of_year","Country")

head(y_totals_by_day)

final_df_for_analysis <- rbind(x_totals_by_day, y_totals_by_day)
head(final_df_for_analysis)

#Now I will create the graph to compare the cumulative number of positive cases for each country on each day of testing





#------------------------------------------------------------------------------

#For question 2 I will need to format a data frame of the final day's cumulative counts for all markers for both countries

#I will create a vector with a column containing all markers. This column will be used for the x-axis in my bar plot
marker <- (c("Marker_01", "Marker_02", "Marker_03", "Marker_04", "Marker_05", "Marker_06", "Marker_07", "Marker_08", "Marker_09", "Marker_10"))

#This code obtains the final cumulative counts for both countries and transposes them into 1 column
#It also only keeps the data related to the markers

total_marker_count_x <- head(t(tail(x_totals_by_day,1)),length(marker))
total_marker_count_y <- head(t(tail(y_totals_by_day,1)),length(marker))

#Now I'll rename the column to keep help keep track of the country the counts belong to
colnames(total_marker_count_x) <- "Xcount"
colnames(total_marker_count_y) <- "Ycount"

#Now I'll create a column of Country designation characters that is the length of the number of markers used in the study
country_marker_x <- c(rep("X",length(marker)))
country_marker_y <- c(rep("Y",length(marker)))

#Now I'll turn the the vectors into data frames and then combine the three columns
country_marker_x <- data.frame(country_marker_x)
country_marker_y <- data.frame(country_marker_y)
total_marker_count_x <- data.frame(total_marker_count_x)
total_marker_count_y <- data.frame(total_marker_count_y)
marker <- data.frame(marker, stringsAsFactors = FALSE)



#Combining all three columns

newdtx <- cbind(marker,total_marker_count_x,country_marker_x)
newdty <- cbind(marker,total_marker_count_y,country_marker_y)

#Now I will remove the row names since they are redundant
#And rename the column names so they match when I combine them
rownames(newdtx) <- NULL
rownames(newdty) <- NULL
colnames(newdtx)[2] <- "Count"
colnames(newdtx)[3] <- "Country"
colnames(newdty)[2] <- "Count"
colnames(newdty)[3] <- "Country"


#then I will add the y marker count dataframe below the x marker count dataframe
#then the x axis will be marker number, the y-axis will be marker counts, and i will be able to fill the bar plot by country designation

final_dt_for_bar_plot <- rbind(newdtx,newdty)



#Graph supporting answer to question 1 as stated above:
ggplot(data = final_df_for_analysis,
       aes(x = day_of_year, y = Cum_Pos, color = Country)) +
  geom_point() +
  xlab("Day of Year") +
  ylab("Cumulative Positive") +
  theme_classic()


#Now I will create the graph to support my answer for question 2

ggplot(final_dt_for_bar_plot, aes(x = marker, y = Count, fill = Country)) +
  geom_col(position = "dodge")

