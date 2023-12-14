#analysis.R file

#Use the source() function to load the functions defined in supportingFunctions.R,
 #compile all data into a single .csv file, process the data included in the entire 
 #data set to answer the project's two main questions, and provide graphical evidence 
 #for our answers.
#Use comments in this file to explain the rational and how the graphical evidence 
 #supports our answer to the two questions.

#1. Source() to get the functions
#2. Answer the two questions from the project using code and graphical evidence (#Nitin said that we can use the allData.csv file here and don't need to call any of the functions from the supportingFunctions.R script)
#3. Explain the code/graphics to fully answer the two questions

#Import the functions from the supportingFunctions script
functions <- source("C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/supportingFunctions.R") #import the functions from the supportingFunctions.R script

#Question 1 code (using allData.csv):
combinedData <- "C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/allData.csv" #combined data file (allData.csv)

dayList <- 120:175 #list of the days in these files
dayListLength <- length(dayList) #number of days
CasesList <- rep(x = 0, length(56)) #set up an empty list of cases for each day
XCasesDF <- data.frame(dayList,CasesList) #create a dataframe for the day and number of positive cases for country X
YCasesDF <- data.frame(dayList,CasesList) #create a dataframe for the day and number of positive cases for country Y

for(i in 1:nrow(data)){ #iterate through the rows of the combined data
  if(data$country[i] == "X"){ #if the data belongs to country X
    day <- data$dayofYear[i] #find the day that the data comes from
    markersSum <- 0 #reset the markersSum for each new test
    markersSum <- data$marker01[i] + data$marker02[i] + data$marker03[i] + data$marker04[i] + data$marker05[i] + data$marker06[i] + data$marker07[i] + data$marker08[i] + data$marker09[i] + data$marker10[i] #sum the number of markers
    if(markersSum > 0){ #if there is at least one positive marker on a test
      XCasesDF[(day-119),2] <- XCasesDF[(day-119),2] + 1 #add 1 to the corresponding positive tests in the country X dataframe
    }
  }else if(data$country[i] == "Y"){ #if the data belongs to country Y
    day <- data$dayofYear[i] #find the day that the data comes from
    markersSum <- 0 #reset the markersSum for each new test
    markersSum <- data$marker01[i] + data$marker02[i] + data$marker03[i] + data$marker04[i] + data$marker05[i] + data$marker06[i] + data$marker07[i] + data$marker08[i] + data$marker09[i] + data$marker10[i] #sum the number of markers
    if(markersSum > 0){ #if there is at least one positive marker on a test
      YCasesDF[(day-119),2] <- YCasesDF[(day-119),2] + 1 #add 1 to the corresponding positive tests in the country Y dataframe
    }
  }
}

XCasesByDayPlot <- ggplot(data = XCasesDF,#scatterplot for the number of positive cases for each day in country X
                          aes(x = dayList, y = CasesList)) +  #set the x-axis to the list of days and the y-axis to the number of positive cases
  geom_point() + #make this graph a scatterplot
  xlab("Day") +  #add a x-axis label
  ylab("Number of Positive Cases for Country X") + #add a y-axis label
  theme_classic() #reset the theme

YCasesByDayPlot <- ggplot(data = YCasesDF, #scatterplot for the number of positive cases for each day in country Y
                          aes(x = dayList, y = CasesList)) + 
  geom_point() + #make this graph a scatterplot
  xlab("Day") + #add a x-axis label
  ylab("Number of Positive Cases for Country Y") + #add a y-axis label
  theme_classic() #reset the theme

NumberOfPositiveCasesFigure <- plot_grid(XCasesByDayPlot, YCasesByDayPlot, #put these two graphs into one figure so that they can be compared easily
                                         labels = c("Number of Positive Cases in Country X", "Number of Positive Cases in Country Y"), #label the graphs
                                         rel_widths = c(1, 1), #set the relative widths to be equal to each other
                                         ncol = 2, #arrange these plots to be in two columns
                                         nrow = 1) #arrange these plots to be in one row

print(NumberOfPositiveCasesFigure) #print the figure with both plots

#Response to Question 1:
#Given that the figure with the plots for the number of positive cases for Countries
 #X and Y over time illustrates that the infection was only present in Country X for 
 #the first nearly 20 days before having any cases in country Y, it appears that the
 #disease began in Country X and later spread to Country Y.

#Question 2 code (using allData.csv):
data <- read.csv(combinedData) #read the combined data

markersListX <- rep(0,10) #make an empty markers list for country X
markersListY <- rep(0,10) #make an empty markers lits for country Y

for(i in 1:nrow(data)){ #iterate through each row of the data
  if(data$dayofYear[i] == 175){ #check if the day is 175 (vaccine is developed on this day)
    if(data$country[i] == "X"){ #check if the country is X
      if(data$marker01[i] == 1){ #check if marker01 is positive
        markersListX[1] <- markersListX[1] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker02[i] == 1){ #check if marker02 is positive
        markersListX[2] <- markersListX[2] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker03[i] == 1){ #check if marker03 is positive
        markersListX[3] <- markersListX[3] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker04[i] == 1){ #check if marker04 is positive
        markersListX[4] <- markersListX[4] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker05[i] == 1){ #check if marker05 is positive
        markersListX[5] <- markersListX[5] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker06[i] == 1){ #check if marker06 is positive
        markersListX[6] <- markersListX[6] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker07[i] == 1){ #check if marker07 is positive
        markersListX[7] <- markersListX[7] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker08[i] == 1){ #check if marker08 is positive
        markersListX[8] <- markersListX[8] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker09[i] == 1){ #check if marker09 is positive
        markersListX[9] <- markersListX[9] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker10[i] == 1){ #check if marker10 is positive
        markersListX[10] <- markersListX[10] + 1 #if so, add 1 to the corresponding
      }
    }else if(data$country[i] == "Y"){ #check if the country is Y
      if(data$marker01[i] == 1){ #check if marker01 is positive
        markersListY[1] <- markersListY[1] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker02[i] == 1){ #check if marker02 is positive
        markersListY[2] <- markersListY[2] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker03[i] == 1){ #check if marker03 is positive
        markersListY[3] <- markersListY[3] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker04[i] == 1){ #check if marker04 is positive
        markersListY[4] <- markersListY[4] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker05[i] == 1){ #check if marker05 is positive
        markersListY[5] <- markersListY[5] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker06[i] == 1){ #check if marker06 is positive
        markersListY[6] <- markersListY[6] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker07[i] == 1){ #check if marker07 is positive
        markersListY[7] <- markersListY[7] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker08[i] == 1){ #check if marker08 is positive
        markersListY[8] <- markersListY[8] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker09[i] == 1){ #check if marker09 is positive
        markersListY[9] <- markersListY[9] + 1 #if so, add 1 to the corresponding
      }
      if(data$marker10[i] == 1){ #check if marker10 is positive
        markersListY[10] <- markersListY[10] + 1 #if so, add 1 to the corresponding
      }
    }
  }
}

print("The distribution of markers for Country X on day 175 is: ") #print the marker distribution statement for country X
print(markersListX) #print the marker distribution for country X
print("The distribution of markers for Country Y on day 175 is: ") #print the marker distribution statement for country Y
print(markersListY) #print the marker distribution for country Y

markersList <- c("1","2","3","4","5","6","7","8","9","10") #make a list of the marker numbers
Day175XDF <- data.frame(markersList, list = markersListX) #make the marker distributions and list of marker numbers into a dataframe to be used by the ggplot
Day175YDF <- data.frame(markersList, list = markersListY) #make the marker distributions and list of marker numbers into a dataframe to be used by the ggplot

Day175XMarkersPlot <- ggplot(data = Day175XDF, #make a ggplot with the Day175XDF
                             aes(x = markersList, y = list)) + 
  geom_point() + #make a scatterplot
  xlab("Marker Number") + #add a x-label
  ylab("Number of Positive Cases with Specific Markers for Country X") + #add a y-label
  theme_classic() #change the theme
Day175YMarkersPlot <- ggplot(data = Day175YDF, #make a ggplot with the Day175YDF
                             aes(x = markersList, y = list)) + 
  geom_point() + #make a scatterplot
  xlab("Marker Number") + #add a x-label
  ylab("Number of Positive Cases with Specific Markers for Country Y") + #add a y-label
  theme_classic() #change the theme
MarkersFigure <- plot_grid(Day175XMarkersPlot,Day175YMarkersPlot, #put these two plots into one figure to make it easier to compare them
                           labels = c("Number of Positive Cases in Country X", "Number of Positive Cases in Country Y"), #label the plots
                           rel_widths = c(1, 1), #set the relative widths to be equal to each other
                           ncol = 2, #have them arranged in 2 columns
                           nrow = 1) #have them arrange in 1 row
print(MarkersFigure) #print these plots

#Response to Question 2:
#Given that there's very little similarity between the marker profiles
 #of Countries X and Y, we speculate that a vaccine created on Day 175 by
 #Country Y would be unlikely to be very effective in Country X. Specifically,
 #Country X appears to have significantly higher frequencies of Markers 1-5 and 
 #significantly lower frequencies of Markers 6-10, whereas Country Y seems to 
 #have very low frequencies of Markers 1-5 and very high frequencies of Markers 
 #6-10, which would suggest that the disease protein is different for an average
 #citizen of country X vs. that of an average citizen of Country Y. This 
 #suggests that the vaccine that works well for Country Y will likely not work
 #well for citizens of country X because the citizens of country X will likely
 #be facing a significantly different bacteria.
