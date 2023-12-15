#Set the Working Directory
setwd("C:/Users/chris/OneDrive/Desktop/R/BioComputing")
source("supportingFunctions.R")

##########
#FUNCTIONS
##########

txt_to_csv("C:/Users/chris/OneDrive/Desktop/R/BioComputing/countryY")

compile_csvs("C:/Users/chris/OneDrive/Desktop/R/BioComputing/countryX", "C:/Users/chris/OneDrive/Desktop/R/BioComputing")
compile_csvs("C:/Users/chris/OneDrive/Desktop/R/BioComputing/countryY", "C:/Users/chris/OneDrive/Desktop/R/BioComputing")
merge_both()

data_summary("C:/Users/chris/OneDrive/Desktop/R/BioComputing")

###########
#Question 1
###########

#1. In which country (X or Y) did the disease outbreak likely begin?
#It looks like the disease originated in Country X, according to the graphed data of infections by country over time.
#Country Y does not have any infections until Country X has a steady amount of infections.

#Read in all the data, load ggplot
allData<-read.csv("allData.csv")
library(tidyverse)

sum_x <-0 #initialize the sum for x
sum_y <- 0 #initialize the sum for y
time_data <- read.csv("allData.csv") #read in the csv

for (i in 1:nrow(time_data)) { #for loop to loop through all the row
  if (time_data$country[i] == "X" & (allData$marker01[i] == "1" | allData$marker02[i] == "1" | allData$marker03[i] == "1" | allData$marker04[i] == "1" | allData$marker05[i] == "1" | allData$marker06[i] == "1" | allData$marker07[i] == "1" | allData$marker08[i] == "1" | allData$marker09[i] == "1" | allData$marker10[i] == "1")) { #Check for infection via marker in patients in country x
    sum_x <- sum_x + 1 #increment the sum counter for X
  } else if (time_data$country[i] == "Y" & (allData$marker01[i] == "1" | allData$marker02[i] == "1" | allData$marker03[i] == "1" | allData$marker04[i] == "1" | allData$marker05[i] == "1" | allData$marker06[i] == "1" | allData$marker07[i] == "1" | allData$marker08[i] == "1" | allData$marker09[i] == "1" | allData$marker10[i] == "1")) { #Check for infection via marker in patients in country y
    sum_y <- sum_y + 1 #increment the sum counter for y
  }
  time_data$sum_x[i] <- sum_x #write the sum to the row in the for loop, in a new column, for country x
  time_data$sum_y[i] <- sum_y #write the sum to the row in the for loop, in a new column, for country y
}

Data_X_time <- time_data[time_data$country == "X",] #Make a new data table of just country X
Data_Y_time <- time_data[time_data$country == "Y",] #Make a new data table of just country Y

ggplot(Data_X_time, aes(x = dayofYear)) + #plot of the infections by day, For country X
  geom_point(aes(y = sum_x, color = "Sum_X")) +
  labs(title = "Total Infections in Country X over Time", y = "Infections") +
  theme_classic()

ggplot(Data_Y_time, aes(x = dayofYear)) + #plot of the infections by day, for country Y
  geom_point(aes(y = sum_y, color = "Sum_y")) +
  labs(title = "Total Infections in Country Y over Time", y = "Infections") +
  theme_classic()

###########
#Question 2
###########
#2. If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
#No. It looks like Country X is mainly comprised of markers 1-5, and country Y is mainly comprised of markers 5-10
#Any vaccine by Country Y will likely utilize markers 5-10, which is an ineffective for Country X.

Data_X <- allData[allData$country == "X",]#Make a new data table of just country X
Data_Y <- allData[allData$country == "Y",]#Make a new data table of just country Y

X=data.frame(markers=c(1:10), count=c(sum(Data_X$marker01), sum(Data_X$marker02), sum(Data_X$marker03), sum(Data_X$marker04), sum(Data_X$marker05), sum(Data_X$marker06), sum(Data_X$marker07), sum(Data_X$marker08), sum(Data_X$marker09), sum(Data_X$marker10))) #Sum the markers individually for country X
Y=data.frame(markers=c(1:10), count=c(sum(Data_Y$marker01), sum(Data_Y$marker02), sum(Data_Y$marker03), sum(Data_Y$marker04), sum(Data_Y$marker05), sum(Data_Y$marker06), sum(Data_Y$marker07), sum(Data_Y$marker08), sum(Data_Y$marker09), sum(Data_Y$marker10))) #Sum the markers individually for country Y
X$country="X" #Assign the value X for country X
Y$country="Y" #Assign the value Y for country Y
markers=rbind(X, Y) #Bind the two data stuctures
ggplot(markers, aes(markers, count, fill=country)) + #Plot the markers, and color them by country
  geom_bar(stat="identity", position="dodge") +
  theme_classic()
