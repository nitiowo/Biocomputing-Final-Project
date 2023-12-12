#supportingFunctions.R file

#Suggested functions:
#Write a function that converts all files in a directory with space- or tab-delimited 
#data (.txt) into csv files
#See tutorial 7 exercise 1 (that problem is .csv to .txt, but it's the same idea)

commaTabtoCSV <- function(punctuation,file){
  if (punctuation == "space"){
    readFile <- read.table(file=file, sep = " ")
    csvFile <- write.table(readFile, sep = ",")
    return(csvFile)
  }else if(punctuation == "tab"){
    readFile <- read.table(file=file, sep = "\t")
    csvFile <- write.table(readFile, sep = ",")
    return(csvFile)
  }else{
    print("Format is not supported by this function.")
  }
}

#Write a function to compile data from all .csv files in a directory into a single
#.csv file. The compiled data should have the original twelve columns from daily 
#data sheets, but also country and dayofYear columns. The user should be able to
#choose whether they want to remove rows with NA's in any columns, include NAs in
#the compiled data but be warned of their presence, or include NAs in the compiled
#data without a warning. One function that might be helpful here is strsplit(): it
#splits a string of characters at a defined character and allow you to access the 
#sub-strings of characters. For example, if x <- "I love biocomputing", then z <-
#strsplit(x," ") would allow you to access each word in the sentence from the 
#contents of z.
#Process:
#1. Set working directory with setwd() to be in the right folder
#2. Make a list of the files in the directory with list.files() in the working 
#directory
#3. Body of the custom function designed to combine all of this data.
#a. Arguments of the function: list of files, country names, NA argument
#b. Set up an empty data frame for the combined data and final data
#c. Set up a for loop to iterate through this list of files
#d. Obtain the country and day for the 13th and 14th rows
#e. Add the two new columns of data for each row (using strsplit() for the date)
#f. Do the NA option picked by the user (use if/elif statements)
#g. Return the final .csv file
#Reference the final challenge of lecture 26 for this step

countryPaths <- list("C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/countryX") #list of directory paths
countryList <- list("X")
NAChoice <- "no NA"
compileData <- function(countryPaths = countryPaths, countryList = countryList, NAChoice = NAChoice){ #function
  output.csv <- data.frame()
  FinalOutput.csv <- data.frame()
  for(i in 1:length(countryPaths)){ #iterate through each country's directory (2)
    setwd(as.character(countryPaths[i])) #set working directory for the country
    fileList <- list.files() #generate a list of files for the directory
    for(j in 1:length(fileList)){ #iterate through each file (56)
      currentFile <- read.csv(fileList[j], header = TRUE) #read the current file
      country <- as.character(countryList[i]) #get the country for each file
      countryColumn <- rep(x = country, nrow(currentFile)) #make a vector with the length of the number of rows in the file with the country name
      fileStrSplit <- strsplit(fileList[j], split = "_") #get the day number from the file name
      fileStrSplitChar <- fileStrSplit[[1]][2]
      dayStrSplit <- strsplit(fileStrSplitChar, split = "[.]") #split the number.csv into the number and csv; use an escape character for . as split
      day <- as.numeric(dayStrSplit[[1]][1]) #get the day from the split of the number.csv      dayColumn <- rep(x = day, nrow(currentFile)) #make a vector with the length of the number of rows in the file with the day
      dayColumn <- rep(x = day, nrow(currentFile)) #make a vector with the length of the number of rows in the file with the day
      currentFile$dayofYear <- dayColumn #create the row with country and day
      currentFile$country <- countryColumn #create the row with country and day
      if(i*j == 1){
        write.table(currentFile, file = "output.csv", sep = ",", col.names = TRUE, row.names = FALSE) #append = TRUE is not needed
      }else{
        write.table(currentFile, file = "output.csv", append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE) #append = TRUE is needed
      }
    }
  }
  outputRead <- read.csv("output.csv")
  if(NAChoice == "no NA"){   #NA options: if/else if/else loop for the 3 cases
    for(k in 1:nrow(outputRead)){
      if(sum(is.na(outputRead[k,1:14])) == 0){ #Use !is.na() from lecture 22 notes; the sum(is.na()) adds 1 every time there is a NA value (adds 1 for the Boolean True and 0 for the Boolean False)
        if(k != 1){
          write.table(outputRead[k,1:14], file = "FinalOutput.csv", sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE) #col.names = TRUE is not needed for the remaining rows
        }else{
          write.table(outputRead[k,1:14], file = "FinalOutput.csv", sep = ",", append = TRUE, col.names = TRUE, row.names = FALSE) #col.names = TRUE is needed for the first row
        }
      }
    }
  }else if(NAChoice == "NA with warning"){
    print("NA values are included in this data.")
    FinalOutput.csv <- output.csv
  }else if(NAChoice == "keep NA"){
    FinalOutput.csv <- output.csv
  }else{
    print("Please put an accepted argument for NAChoice: no NA, NA with warning, or keep NA")
  }
  return(FinalOutput.csv) #return statement
}

compileData(countryPaths, countryList, NAChoice)