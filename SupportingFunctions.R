#SupportingFunctions.R
#Nicholas Buhay, Mike Conde, Connor Hinkes
#Write a function that will convert all files in a directory with a space or 
#tab delimited data (.txt) into comma-separated value files
#Function 1: converts .txt to csv
#How to use: csvconverter("put the path to your .txt directory here", "desired name of the output .csv directory")
csvconverter <- function(txtdirectory, csvdirectory) {
  #Create output directory
  if (!dir.exists(csvdirectory)) {
    dir.create(csvdirectory)
  }
  
  #List out files in the .txt directory
  allfiles <- list.files(txtdirectory)
  
  #Loop through each .txt file to convert it to .csv
  for (file in allfiles) {
    #Ensure converted files are text files
    if (endsWith(file, ".txt")) {
      input_filepath <- file.path(txtdirectory, file)
      output_filepath <- file.path(csvdirectory, gsub(".txt", ".csv", file))
      
      #Read the space- or tab-delimited file and write it to a .csv file
      data <- read.table(input_filepath, header = TRUE, sep = c(" ", "\t"))
      write.csv(data, file = output_filepath, row.names = FALSE)
    }
  }
  
  print("You have converted the directory's .txt files to .csv files.")
}

#Function 2:
#Fuction 2 Part 1:
#Custom Function that adds the country name and the day of testing
#Example usage: addcolumns("pathtothedirectoryyouneed")
addcolumns <- function(input) {
  #Concatenate all the files in the input
  listedfiles <- list.files(input, pattern = "\\.csv$", full.names = TRUE)
  
  #Loop through each file, and add the desired columns
  for (file in listedfiles) {
    data <- read.csv(file, stringsAsFactors = FALSE)
    #If/else statement to give the right country value
    country_value <- ifelse(grepl("x", tolower(input)), "X", "Y")
    #Add a new country column with the the right country value
    data$Country <- country_value
    #taking out the screen_ part of the file name to plug into day of year
    erasingscreen<-gsub("screen_","", basename(file))
    #taking out the .csv part of the filename
    data$dayofyear <- gsub("\\.csv$", "", erasingscreen)
    
    #Write the updated data to the CSV file
    updated<- file.path(input, basename(file))
    write.csv(data, file = updated, row.names = FALSE)
  }
}

#Function 2 Part 2:
#How to use:
#Use compilefiles("pathtodirectory1","pathtodirectory2","outputfilename", "choiceaboutNA")
#Regarding NA, the user can either write "removeNAs", "warnaboutNAs", or "disregardNAs"
compilefiles <- function(directory1, directory2, output, NAchoice) {
  #List out files in the first and second directory
  files_1 <- list.files(directory1, pattern = "\\.csv$", full.names = TRUE)
  files_2 <- list.files(directory2, pattern = "\\.csv$", full.names = TRUE)
  
  #Combine the two lists of files into one variable
  allcsvfiles <- c(files_1, files_2)
  #Make an empty list for storing dataframes
  data_frames <- list()
  
  #Loop through each file and read the data
  for (file in allcsvfiles) {
    data <- read.csv(file, stringsAsFactors = FALSE)
    #Add the data frame to the list
    data_frames <- c(data_frames, list(data))
  }
  #Cat all data frames into one
  compiled_data <- do.call(rbind, data_frames)
  
  #NA Choice
  #remove NAs if specified
  if (NAchoice == "removeNAs") {
    compiled_data[, 3:12] <- lapply(compiled_data[, 3:12], function(x) ifelse(is.na(x), NA, x))
    compiled_data <- na.omit(compiled_data)
    #warn about NAs if specified
  }else if(NAchoice == "warnaboutNAs"){
    if (sum(is.na(compiled_data[, 3:12])) != 0) {
      print("There are NAs in columns 3 to 12.")
    }
    #no warning, keep NAs in
  }else if (NAchoice == "disregardNAs"){
    print("NAs disregarded")
  }
  
  #Write the final, compiled data as a csv
  write.csv(compiled_data, file = output, row.names = FALSE)
  
  print("Compilation completed.")
}

#Function 2: Running both parts
#How to use:
#addcolumnsandcompile("yourfirstdirectory","yourseconddirectory","nameofnewoutputfile", "choiceaboutNA")
#Regarding NA, the user can either write "removeNAs", "warnaboutNAs", or "disregardNAs"
addcolumnsandcompile <- function(input__1,input__2,compiledoutput, choiceaboutNA) {
  addcolumns(input__1)
  addcolumns(input__2)
  compilefiles(input__1,input__2,compiledoutput,choiceaboutNA)
}
#Function 3
#How to use:
#summary("nameofcsvfileyouwanttosummarize")
#Create the function:
summary <- function(inputdata) {
  #Create variable for .csv document containing the input data
  data<-read.csv(inputdata)
  #Write variables for each metric we are tracking:
  total_screens <- nrow(data)
  percent_infected <- mean(apply(data [3:12], 1, function (row) any(row == 1))) * 100
  gender_distribution <- table(data$gender) / total_screens * 100 
  age_distribution <- table(data$age)
  
  #Output the metrics:
  cat("Total screens run:", total_screens, "\n")
  cat("% of patients screened that were infected:", percent_infected, "%\n")
  cat("% of patients that identify as female and male:", gender_distribution, "%\n") 
  cat("Age distribution of patients: \n")
  print(age_distribution)
}
