#Supporting functions 
#Carmela D'Antuono and John LeSage 

setwd("~/Desktop/Biocomputing-Final-Project") #set the working directory
library(ggplot2) #load in necessary libraries 
library(cowplot)

#fxn 1 - converts all files into .csv files
conversion <- function(countryFile){ #country will be the word country and the letter, no spaces
  setwd(paste("~/Desktop/Biocomputing-Final-Project/", countryFile, sep = "")) 
  #makes the assumption that all files are in the Biocoputing-Final-Project folder on the desktop
  listOfFiles <- as.vector(list.files()) 
  for(i in 1:length(listOfFiles)){ #assuming that all files are in titled screen_day.xxx format 
    if(strsplit(listOfFiles[i],"[.]")[[1]][2] == "csv"){ 
      print("files are already in comma separated value files")
    }else if(strsplit(listOfFiles[i],"[.]")[[1]][2] == "txt"){
      table <- read.table(listOfFiles[i], header=TRUE, sep=",") #imput the .txt file into R
      write.csv(table, file = (paste("~/Desktop/Biocomputing-Final-Project/", countryFile, 
                                     "/", strsplit(listOfFiles[i], "[.]")[[1]][1], ".csv", sep = "")) , row.names=FALSE)
      #write out a .csv file with the same name
    }else{
      print("parameters not valid, must be comma or tab separated") #an option for other file types
    }
  }
}
  #usage: conversion(countryFile = "countryA") 
  #where A is any letter that corresponds to the letter the country is

#fxn 2 - compile all from directory into 1 .csv file 
compileData <- function(countryFile, NAOption){ #country will be the word country and the letter, no spaces
  setwd(paste("~/Desktop/Biocomputing-Final-Project/", countryFile, sep = "")) 
  #makes the assumption that all files are in the Biocomputing-Final-Project folder on the desktop
  fileList <- as.vector(list.files(pattern="\\.csv$"))
  for(i in 1:length(fileList)){
    currentFile <- read.csv(fileList[i], header = TRUE)
    country <- as.character(countryFile)
    currentFile$country = country #add a column for country into the individual file
    day <- as.numeric(strsplit(as.character(strsplit(as.character(fileList[i]), "_")[[1]][2]), "[.]")[[1]][1])
    currentFile$dayofYear = day #add a column for day of year into the individual file
    DataCompiled <- rbind(DataCompiled, currentFile) 
 
    if(NAOption == "remove"){ #remove NAs if that's what the user wants
     DataCompiled <- na.omit(DataCompiled)
     }else if(NAOption == "includeWithWarning"){ #warn of NAs if that's what the user wants
    if(sum(is.na(DataCompiled)) != 0){
     print("There are NAs that remain in the dataframe.")
     }
     }else if(NAOption == "includeNoWarning"){ #don't do anything if user wants to include with no warning
     print("")
     }
  }
  return(DataCompiled) #HOW TO MAKE THIS A SAVED DATAFRAME, does the double arrow above work?
  # write.csv(DataCompiled, file = "~/Desktop/Biocomputing-Final-Project", row.names=FALSE)
  #write out the csv file in the Biocomputing-Final-Project Folder to be used for analysis 
}
#usage: DataCompiled <- compileData(countryFile = "countryA", NAOption = "") 
#where A is any letter that corresponds to the letter the country is
#NAOption can be: remove, includeWithWarning, includeNoWarning
#we will run this seprately for countryX and countryY for this specific project

#fxn 3 - summarize: # screens run, % screened that infected, % male/female, #age distribution (table/plot)
summarise <- function(dataFile){
  num_Total <- nrow(dataFile)
  num_Males <- nrow(dataFile[dataFile$gender == "male",])
  num_Females <- nrow(dataFile[dataFile$gender == "female",])
  percent_Males <- round((num_Males/num_Total * 100), 2) #calculate percent female, rounds to 2 decimal points
  percent_Females <- round((num_Females/num_Total * 100), 2) #calculate percent male, rounds to 2 decimal points
  summary <- paste("The data is", percent_Males, "percent male and", percent_Females, "percent female, with",
                   nrow(dataFile), "screens run.")
  ageDistribution <- ggplot(data = dataFile, aes(x=age, color=gender)) +
    geom_histogram() +
    theme_classic() 
  return <- list(dataSummary = summary, graph = ageDistribution)
  return(return)
}

