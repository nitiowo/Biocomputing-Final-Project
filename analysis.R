#analysis.R file

#Use the source() function to load the functions defined in supportingFunctions.R,
 #compile all data into a single .csv file, process the data included in the entire 
 #data set to answer the project's two main questions, and provide graphical evidence 
 #for our answers.
#Use comments in this file to explain the rational and how the graphical evidence 
 #supports our answer to the two questions.

#1. Source() to get the functions
#2. csv conversion for country Y
#3. Combine all of the data in 1 dataframe

#Import the functions from the supportingFunctions script
functions <- source("C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/supportingFunctions.R")

#Inputs for this analysis
countryPaths <- list("C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/countryX","C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/countryY") #list of directory paths
countryList <- list("X","Y") #list of country names
countryPunctuation <- list("comma","space") #list of the delimiter for each country's files

#Convert Country Y from space to comma

#setwd()
#fileList
#for(file in file_list){
  #commaTabtoCSV(file)
#}

csvFileList <- list()
for(i in 1:length(countryPaths)){ #iterate through each country's directory (2)
  setwd(as.character(countryPaths[i])) #set working directory for the country
  fileList <- list.files() #generate a list of files for the directory
  for(j in 1:length(fileList)){ #iterate through each file (56)
    print(fileList[j])
    if(countryPunctuation[i] == "comma"){
      csvFile <- read.csv(fileList[j])
    }else if(countryPunctuation[i] == "space"){
      csvFile <- commaTabtoCSV(countryPunctuation[i], fileList[j])
    }else if(countryPunctuation[i] == "tab"){
      csvFile <- commaTabtoCSV(countryPunctuation[i], fileList[j])
    }else{
      print("Unsupported file type")
    }
    csvFileList <- list(csvFileList,csvFile)
  }
  print(csvFileList)
  return(csvFileList)
}