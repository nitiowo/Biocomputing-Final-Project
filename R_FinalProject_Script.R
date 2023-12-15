#Final Project 

setwd("Working Directory")

source(file = "R_FinalProject_CustomFunctions.R")
library(ggplot2)

#converting txt files from country Y into csv
convert_txt_to_csv_dir(dirname = "countryY")

#Put the files from both countries into vectors
csvfilesY<- list.files(path = "Path where Counrty Y files are locaated",
                       pattern = ".csv",
                       full.names = TRUE, 
                       recursive = FALSE)

csvfilesX<- list.files(path = "Path where Counrty x files are locaated",
                       pattern = ".csv",
                       full.names = TRUE, 
                       recursive = FALSE)

#Using custom function #3 to compile data from country X and Y
CompileFiles("Path where Counrty Y files are locaated", CountryName = "Y", NameFile = "YCountryData.csv",VectorFileNames = csvfilesY, na="remove")
CompileFiles("Path where Counrty x files are locaated", CountryName = "X", NameFile = "XCountryData.csv",VectorFileNames = csvfilesX,na="remove")


#reading in csv files made from previous steps 
CounrtyX<-read.csv("XCountryData.csv", header=TRUE)
CounrtyY<-read.csv("YCountryData.csv", header=TRUE)

#putting both data sets together and turns it into a csv
allCountryData<-rbind(CounrtyX,CounrtyY)
write.csv(allCountryData, "allCountryData.csv", row.names = FALSE)
read.csv("allCountryData.csv", header=TRUE)

#Plots Summarizing Data
  # table below summarize the all data number of screens run, percent of patients screened that were infected, the percent of patients that identify as male and female, and the age distribution of patients
summarized_all<-data_analyze("allCountryData.csv")

#plot that shows the number of infected over time for each country
infected_plot = ggplot(data = summarized_all, aes(x = DayofYear, y = infected, color = Country)) +
  geom_line() + 
  labs(title = "Number of Infected",
       x = "Day",
       y = "Number",
       color = "country") + 
  theme(plot.title = element_text(hjust = 0.5))
print(infected_plot)
# Question 1: In which country did the outbreak likely begin?
# Answer: The disease outbreak began in country X. According to the summarized data, 
#         we see that country X has 56 infected patients at 120 day_of_year, while country Y
#         got 0. Country Y didn't get infected patients until 139 day_of_year



#reads in csv file
allData = read.csv("allCountryData.csv", header=TRUE)

#empty Matrix to store data 
MarkerTable<- matrix(0, nrow = 2, ncol =10, byrow = TRUE, dimnames = list(c("Counrty X", "Country Y"),
                                                                          c("Marker01", "Marker02", "Marker03", "Marker04", "Marker05","Marker06","Marker07", "Marker08", "Marker09", "Marker010")))
#For loop that goes through all the countries data and puts it in the empty matrix
for(i in 1:nrow(allData)){
  
  if(allData$Country[i]=="X"){
    MarkerTable[1,1]<- MarkerTable[1,1]+allData$marker01[i]
    MarkerTable[1,2]<- MarkerTable[1,2]+allData$marker02[i]
    MarkerTable[1,3]<- MarkerTable[1,3]+allData$marker03[i]
    MarkerTable[1,4]<- MarkerTable[1,4]+allData$marker04[i]
    MarkerTable[1,5]<- MarkerTable[1,5]+allData$marker05[i]
    MarkerTable[1,6]<- MarkerTable[1,6]+allData$marker06[i]
    MarkerTable[1,7]<- MarkerTable[1,7]+allData$marker07[i]
    MarkerTable[1,8]<- MarkerTable[1,8]+allData$marker08[i]
    MarkerTable[1,9]<- MarkerTable[1,9]+allData$marker09[i]
    MarkerTable[1,10]<- MarkerTable[1,10]+allData$marker10[i]
  }
  else if(allData$Country[i]=="Y"){
    MarkerTable[2,1]<- MarkerTable[2,1]+allData$marker01[i]
    MarkerTable[2,2]<- MarkerTable[2,2]+allData$marker02[i]
    MarkerTable[2,3]<- MarkerTable[2,3]+allData$marker03[i]
    MarkerTable[2,4]<- MarkerTable[2,4]+allData$marker04[i]
    MarkerTable[2,5]<- MarkerTable[2,5]+allData$marker05[i]
    MarkerTable[2,6]<- MarkerTable[2,6]+allData$marker06[i]
    MarkerTable[2,7]<- MarkerTable[2,7]+allData$marker07[i]
    MarkerTable[2,8]<- MarkerTable[2,8]+allData$marker08[i]
    MarkerTable[2,9]<- MarkerTable[2,9]+allData$marker09[i]
    MarkerTable[2,10]<- MarkerTable[2,10]+allData$marker10[i]
  }
}

#Table that shows which markers were most prevalent in each country
MarkerTable

#Question 2
# If country Y develops a vaccine it will not work on the citizens of 
#   country X because the most prevalent markers in country Y are markers 
#   6-10 while these are the least prevalent markers in country X.




































































