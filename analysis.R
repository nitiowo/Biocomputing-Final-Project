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

functions <- source("C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/supportingFunctions.R") #import the supportingFunctions script

countryPaths <- list("C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/countryX","C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/countryY") #list of directory paths
countryList <- list("X","Y") #list of country names
countryPunctuation <- list("comma","space")
countryPathsForConversionSpace <- list("C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/countryY") #list of directory paths for files to be converted

#Convert Country Y from space to comma
for(i in 1:length(countryPathsForConversionSpace)){ #iterate through each country's directory (2)
  setwd(as.character(countryPaths[i])) #set working directory for the country
  fileList <- list.files() #generate a list of files for the directory
  for(j in 1:length(fileList)){ #iterate through each file (56)
    if(countryPunctuation[i] == "comma"){
      x <- 1 #use as a statement to get R to pass through this if loop without doing anything
    }else if(countryPunctuation[i] == "space"){
      commaTabtoCSV(countryPunctuation[i], fileList[j])
    }else if(countryPunctuation[i] == "tab"){
      commaTabtoCSV(countryPunctuation[i], fileList[j])
    }else{
      print("Unsupported file type")
    }
    commaTabtoCSV("space",fileList[j])
  }
  return()
}

#Alan's code
##function to combine country information, folder_paths should be a vector containing all the 
##correct paths to be iterated over
appendDataFrames <- function(folder_paths, output_file) {
  # Create an empty data frame to store the combined data
  combined_data <- data.frame()
  
  # Loop through each folder path
  for (folder_path in folder_paths) {
    # List files in the folder
    files <- list.files(path = folder_path, pattern = "//.csv$", full.names = TRUE)

    # Read each file as a data frame and append to combined_data
    for (file in files) {
      df <- read.csv(file)
      combined_data <- rbind(combined_data, df)
    }
  }
  
  # Write the combined data frame to an output file
  write.csv(combined_data, file = output_file, row.names = FALSE)
}

folder_paths <- list("C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/countryX") #list of directory paths
output_file <- "combined_data.csv"

##create output file
appendDataFrames(folder_paths, output_file)

"C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/countryX/screen_120.csv"