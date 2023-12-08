#Analysis
#Carmela D'Antuono and John LeSage

rm(list=ls()) #clear the R memory 
setwd("~/Desktop/Biocomputing-Final-Project") #set the working directory
library(ggplot2) #load in necessary libraries 

#functions from supportingFunctions.R (a script we developed to help the analysis)
source("supportingFunctions.R")

#only using conversion on countryY because countryX's data is already in .csv files
conversion(countryFile = "countryY")

#making the empty data frame to comply with the assumptions of the compileData function
columns = c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", 
            "marker06","marker07", "marker08", "marker09", "marker10", "country", "dayofYear")
DataCompiled <- data.frame(matrix(ncol=14, nrow=0)) 
colnames(DataCompiled) <- columns

#compiling the data from both countryX and countryY and assigning it to the DataCompiled data frame 
DataCompiled <- compileData(countryFile = "countryX", NAOption = "includeWithWarning")
DataCompiled <- compileData(countryFile = "countryY", NAOption = "includeWithWarning")

#writing out the csv file so that it is saved in the folder to be submitted via GitHub
write.csv(DataCompiled, file = "~/Desktop/Biocomputing-Final-Project/DataCompiled.csv", row.names=FALSE)

#summarizing the compiled data 
summarise(dataFile = DataCompiled)

#1 - in which country (X or Y) did the disease outbreak likely begin?

#we want to first summarize the data by seeing which patients are infected and which aren't
#since infection is determined by presence of markers, if a patient has at least 1 marker; they are infected
#we can determine this by seeing if the sum of each patient's markers is at least 1; if it's 0, they're not infected
header <- c("dayofYear", "country", "isInfected")
is_Infected <- data.frame(matrix(ncol=3, nrow=nrow(DataCompiled))) #creates a data frame with 3 columns for the header, and 39744 rows for each patient
colnames(is_Infected) <- header

for(i in 1:nrow(is_Infected)){ #cycles through the 39744 patients
  is_Infected$dayofYear[i] <- DataCompiled$dayofYear[i] #populates the ith row of the dayofYear column with the day each patient was screened
  is_Infected$country[i] <- DataCompiled$country[i] #populates the ith row of the Country column with the country each sample was run in
  if((sum(DataCompiled[i, 3:12]))==0){ #if markers 1 through 10 (3rd thru 12th columns) add up to 0
    is_Infected$isInfected[i] <- 0 #the patient is not infected; we note this with a "0"
  } else { #if markers 1 through 10 add up to 1 or greater
    is_Infected$isInfected[i] <- 1 #the patient is infected; we note this with a "1"
  }}

#splitting the data into Country X and Country Y so we can graph each later
#we also take out the "Country" column (column 2) since we already know which country the patients belong to now
infected_X <- is_Infected[is_Infected$country=="countryX",c(1,3)] #country X data: dayofYear and isInfected
infected_Y <- is_Infected[is_Infected$country=="countryY",c(1,3)] #country Y data: dayofYear and isInfected

#now we are going to create a table that shows the total infected in each country each day

daysofYear <- unique(is_Infected$dayofYear)
#creates a list of the days that screens were run on: 120, 121, 122 ... 175
# 56 unique days total

#total infected each day in Country X
dayBydayX <- data.frame(matrix(ncol=2, nrow=length(daysofYear))) #creates an empty data frame with 2 columns and 56 rows, one for each unique day
colnames(dayBydayX) <- c("Day", "totalInfected") #labels the columns
for(i in 1:length(daysofYear)){ #cycles through each day
  dayBydayX$Day[i] <- daysofYear[i] #populates the "Day" column with each day the screens were run on
  dayBydayX$totalInfected[i] <- sum(infected_X[infected_X$dayofYear==daysofYear[i],2]) #populates the "totalInfected" column with the number of infected people each day
  # we get the total number of infected people every day by summing the number of 1's (column 2) in our infected_X data by each day (the ith value of the daysofYear list)
}

#total infected each day in Country Y
#this block of code does the same thing, but for the other country
dayBydayY <- data.frame(matrix(ncol=2, nrow=length(daysofYear)))
colnames(dayBydayY) <- c("Day", "totalInfected")
for(i in 1:length(daysofYear)){
  dayBydayY$Day[i] <- daysofYear[i]
  dayBydayY$totalInfected[i] <- sum(infected_Y[infected_Y$dayofYear==daysofYear[i],2])
}

#graphs our data
ggplot()+
  geom_line(data=dayBydayX, aes(x=Day, y=totalInfected), color="orange") +
  geom_line(data=dayBydayY, aes(x=Day, y=totalInfected), color="blue") + 
  ylab("Number infected") + ggtitle("Daily Infected by Country", 
                                    subtitle="orange = Country X, blue = Country Y") +
  theme(plot.title = element_text(size=16))

# We see that even at the beginning, there are many infected in Country X, between 75-200 individuals
# On those days, there are 0 people infected in country Y.
# Therefore the data shows us pretty convincingly that the outbreak started in country X, 
#since it didn't start affecting individuals in country Y until almost day 140.

#2 - if country Y develops a vaccine, will it work in country X? day 175

#since the vaccine was created on 175 in country Y, we want to look at the comparison of 
#cases between both countries on day 175. Thus, we will load in the datafiles for the day 175
#screens for both country X and country Y.
countryX_175 <- read.csv("~/Desktop/Biocomputing-Final-Project/countryX/screen_175.csv", header = TRUE, sep = ',')
countryY_175 <- read.csv("~/Desktop/Biocomputing-Final-Project/countryY/screen_175.csv", header = TRUE, sep = ',')

#We want to compare the proteins shown to form an immunological response by the patient
#as differences in which markers are present indicate differences in the protein in the disease 
#and thus different responses by a patient's immune system. Thus, if the composition of proteins
#present is similar between the countries, the proteins in the disease in the different countries
#are likely similar and thus the vaccine will work. If, however, the composition of proteins
#are very different, the proteins in the disease in the different countreis are likely different 
#and we would expect a vaccine developed in one country to not work in the other country.

#We will run a for loop for country X, day 175 to find the sum of each present marker in the
#sampled population. 
for(i in 1:nrow(countryX_175)){
  numMarker_1 <- nrow(countryX_175[countryX_175$marker01 > 0,])
  numMarker_2 <- nrow(countryX_175[countryX_175$marker02 > 0,])
  numMarker_3 <- nrow(countryX_175[countryX_175$marker03 > 0,])
  numMarker_4 <- nrow(countryX_175[countryX_175$marker04 > 0,])
  numMarker_5 <- nrow(countryX_175[countryX_175$marker05 > 0,])
  numMarker_6 <- nrow(countryX_175[countryX_175$marker06 > 0,])
  numMarker_7 <- nrow(countryX_175[countryX_175$marker07 > 0,])
  numMarker_8 <- nrow(countryX_175[countryX_175$marker08 > 0,])
  numMarker_9 <- nrow(countryX_175[countryX_175$marker09 > 0,])
  numMarker_10 <- nrow(countryX_175[countryX_175$marker10 > 0,])
}
#by storing the total number of each marker into a different function, we are able to make 
#a vector describing the protein composition of the diseased population in country X on day 175.
x175 <- c(numMarker_1, numMarker_2, numMarker_3, numMarker_4, numMarker_5,
          numMarker_6, numMarker_7, numMarker_8, numMarker_9, numMarker_10)

#We will run the same for loop for country Y, day 175 to find the sum of each present marker in the
#sampled population. 
for(i in 1:nrow(countryY_175)){
  numMarker_1 <- nrow(countryY_175[countryY_175$marker01 > 0,])
  numMarker_2 <- nrow(countryY_175[countryY_175$marker02 > 0,])
  numMarker_3 <- nrow(countryY_175[countryY_175$marker03 > 0,])
  numMarker_4 <- nrow(countryY_175[countryY_175$marker04 > 0,])
  numMarker_5 <- nrow(countryY_175[countryY_175$marker05 > 0,])
  numMarker_6 <- nrow(countryY_175[countryY_175$marker06 > 0,])
  numMarker_7 <- nrow(countryY_175[countryY_175$marker07 > 0,])
  numMarker_8 <- nrow(countryY_175[countryY_175$marker08 > 0,])
  numMarker_9 <- nrow(countryY_175[countryY_175$marker09 > 0,])
  numMarker_10 <- nrow(countryY_175[countryY_175$marker10 > 0,])
}
#by storing the total number of each marker into a different function, we are able to make 
#a vector describing the protein composition of the diseased population in country X on day 175.
y175 <- c(numMarker_1, numMarker_2, numMarker_3, numMarker_4, numMarker_5,
          numMarker_6, numMarker_7, numMarker_8, numMarker_9, numMarker_10)

#we will create a new data frame with the protein composition of the disease in both countries 
#on day 175 and give this data frame column names to clean up the data processing.
markers <- c('marker01', 'marker02', 'marker03', 'marker04', 'marker05', 'marker06', 
                           'marker07','marker08', 'marker09', 'marker10')
analysisDay175 <- data.frame(markers, x175, y175)
names(analysisDay175) <- c('marker', 'countryX', 'countryY')

#this is the data frame that describes the protein composition of the disease in each 
#country on day 175. It is clear that country X has a large amount of samples with markers 1, 2,
#3, 4, and 5 present and a very limited number of the other markers. Country Y has a large amount
#of samples with markers 6, 7, 8, 9, and 10 present and a very limited number of the other markers.
#Because this information shows that the protein composition of the disease in the countries is 
#very different on day 175, the day in which the vaccine was developed in country Y, we would not expect
#the vaccine to work in country X.
analysisDay175

#to show this information in graphical form: 
xTable175 <- data.frame(markers, x175)
yTable175 <- data.frame(markers, y175)
graphDay175 <- ggplot() +
  geom_point(data = xTable175, aes(x=markers, y=x175), color = "green") +
  geom_point(data = yTable175, aes(x=markers, y=y175), color = "red") +
  ggtitle("Cumulative number of markers in each country on day 175", 
          subtitle = "(green is country X and red is country Y)") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(plot.title = element_text(size=10.8)) + 
  ylab("number of individuals")
graphDay175

#The graph makes it very obvious that the distribution of present markers is extremely different 
#in country X and country Y. Thus, we would NOT expect a vaccine developed in country Y on day 175 
#to also work in country X. A different vaccine would need to be created to target the specific 
#markers present in the main protein in country X.  