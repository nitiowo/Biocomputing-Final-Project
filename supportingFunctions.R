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
 #3. Compose a custom function to combine all of this data.
  #a. Arguments of the function: list of files, NA argument
  #b. Set up an empty data frame for the combined data
  #c. Set up a for loop to iterate through this list of files.
  #d. Use the paste function paste() to get the proper file path (if needed to read
     #the files).
  #e. Add the two new columns of data for each row (using strsplit() for the date)
  #Reference the final challenge of lecture 26 for this step

countryPaths <- list("C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/countryX") #list of directory paths
countryList <- list("X")
compileData <- function(countryPaths = countryPaths, countryList = countryList){ #function
  output.csv <- data.frame()
  allFiles <- data.frame(matrix(nrow=0,ncol=14)) #empty data frame with correct number of columns
  columns <- c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10", "country", "day") #column names
  colnames(allFiles) <- columns #assign the column names to the data frame
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
      currentFile$day <- dayColumn #create the row with country and day
      currentFile$country <- countryColumn #create the row with country and day
      print(currentFile)
      if(i*j == 1){
        write.table(currentFile, file = "output.csv", sep = ",", col.names = NA, row.names = TRUE) #append = TRUE is not needed
      }else{
        write.table(currentFile, file = "output.csv", append = TRUE, sep = ",", col.names = NA, row.names = TRUE) #append = TRUE is needed
      }
    }
  }
  #NA options: if/else if/else loop for the 3 cases
  #Use !is.na() or na.rm in sums to remove the NA value (give different answers)
  return(output.csv) #return statement
  print(output.csv)
}

compileData(countryPaths, countryList)
