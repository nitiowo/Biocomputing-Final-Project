#supportingFunctions.R file

#Suggested functions:
#Write a function that converts all files in a directory with space- or tab-delimited 
#data (.txt) into csv files.

#Process:
#1. Create a function with a punctuation argument and a file argument.
#2. Set up an if/else if conditional for the space or tab options.
  #i. Read the file with the file's given separator.
  #ii. Write the file with the comma as a separator.
  #iii. Return the file.
#See tutorial 7 exercise 1 (that problem is .csv to .txt, but it's the same idea)

#User instructions for the function:
#The function can be used as follows: commaTabtoCSV(punctuation,file), where punctuation
  #can either be "space" or "tab", and the file argument is the file path.

commaTabtoCSV <- function(punctuation,file){
  if (punctuation == "space"){ #for files with values separated by spaces
    readFile <- read.table(file=file, sep = " ") #read the file
    csvFile <- write.table(readFile, sep = ",") #rewrite the file with commas as separators
    return(csvFile) #return the file as a csv
  }else if(punctuation == "tab"){ #for the files with values separated by tabs
    readFile <- read.table(file=file, sep = "\t") #read the file
    csvFile <- write.table(readFile, sep = ",") #rewrite the file with commas as separators
    return(csvFile) #return the file as a csv
  }else{ #in case an inappropriate punctuation argument is given
    print("Format is not supported by this function.") #error message
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
#1. Set working directory with setwd() to be in the right folder (the instructions say
  #that the function only needs to compile data from all .csv files in a single directory)
#2. Make a list of the files in the directory with list.files() in the working 
#directory
#3. Body of the custom function designed to combine all of this data.
#a. Arguments of the function: list of files, country names, NA argument
#b. Set empty data frames for the combined data and final data
#c. Set up a for loop to iterate through this list of files
  #i. Obtain the country and day for the 13th and 14th rows
  #ii. Add the two new columns of data for each row (using strsplit() for the date)
#d. Perform the NA option picked by the user (use if/elif statements)
#e. Return the final .csv file
#Reference the final challenge of lecture 26 for this step

#User instructions for the function:
#The user can use the function as follows: compileData(countryPaths, countryList, NAChoice),
  #where the countryPaths are a list of the file paths to the directories for each
  #country's data, the countryList is a list of the country names (in the same order as
  #the provided country directories), and a NAChoice, which can be "no NA" (to exclude all
  #NA values), "NA with warning" to keep the NA values in the combined data but to provide
  #a warning to the user, or "keep NA" to keep all NA values without warning the user.

countryPaths <- list("C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/countryX") #list of directory paths; for demonstration purposes, country X's path is provided
countryList <- list("X") #list of the countries; for demonstration purposes, country X's path is provided
NAChoice <- "no NA" #input for the NA Choice; for demonstration purposes, no NA is used
compileData <- function(countryPaths = countryPaths, countryList = countryList, NAChoice = NAChoice){ #function
  output.csv <- data.frame() #create an empty dataframe for the intermediate output
  FinalOutput.csv <- data.frame() #create an empty dataframe for the final output
  for(i in 1:length(countryPaths)){ #iterate through each country's directory
    setwd(as.character(countryPaths[i])) #set working directory for the country
    fileList <- list.files() #generate a list of files for the directory
    for(j in 1:length(fileList)){ #iterate through each file
      currentFile <- read.csv(fileList[j], header = TRUE) #read the current file
      country <- as.character(countryList[i]) #get the corresponding country for each file
      countryColumn <- rep(x = country, nrow(currentFile)) #make a vector with the length of the number of rows in the file with the obtained country name
      fileStrSplit <- strsplit(fileList[j], split = "_") #get the day number.csv from the file name
      fileStrSplitChar <- fileStrSplit[[1]][2] #change this from an element of a list to a character
      dayStrSplit <- strsplit(fileStrSplitChar, split = "[.]") #split the number.csv into the number and csv; use an escape character for . as split
      day <- as.numeric(dayStrSplit[[1]][1]) #get the day from the split of the number.csv
      dayColumn <- rep(x = day, nrow(currentFile)) #make a vector with the day and the length of the number of rows
      currentFile$dayofYear <- dayColumn #create the column with the day
      currentFile$country <- countryColumn #create the column with the country
      if(i*j == 1){ #for the first row, the column names are needed
        write.table(currentFile, file = "output.csv", sep = ",", col.names = TRUE, row.names = FALSE) #append = TRUE is not needed, col.names is needed
      }else{ #for every row but the first row, col.names is not needed
        write.table(currentFile, file = "output.csv", append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE) #append = TRUE is needed, col.names is not needed
      }
    }
  }
  outputRead <- read.csv("output.csv") #read the intermediate .csv file
  if(NAChoice == "no NA"){   #if NAs are to be excluded
    for(k in 1:nrow(outputRead)){ #iterate through each row of the intermediate .csv file
      if(sum(is.na(outputRead[k,1:14])) == 0){ #Use !is.na() from lecture 22 notes; the sum(is.na()) adds 1 every time there is a NA value in a row (adds 1 for True and 0 for False)
        if(k != 1){ #for the first row, the column names are needed
          write.table(outputRead[k,1:14], file = "FinalOutput.csv", sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE) #col.names are not needed after the first row
        }else{ #when k = 1
          write.table(outputRead[k,1:14], file = "FinalOutput.csv", sep = ",", append = TRUE, col.names = TRUE, row.names = FALSE) #col.names = TRUE is needed for the first row
        }
      }
    }
  }else if(NAChoice == "NA with warning"){ #if NAs are to be included but a warning is provided
    print("NA values are included in this data.") #warning to the user
    FinalOutput.csv <- output.csv #save the intermediate output as the final output
  }else if(NAChoice == "keep NA"){ #if NAs are to be included without a warning
    FinalOutput.csv <- output.csv #save the intermediate output as the final output
  }else{ #if an unacceptable input is given to the NAChoice argument
    print("Please put an accepted argument for NAChoice: no NA, NA with warning, or keep NA") #error message
  }
  return(FinalOutput.csv) #return statement
}

#Write a function to summarize the compiled data set in terms of number of screens run,
 #percent of patients screened that were infected, the percent of patients that identify
 #as male and female, and the age distribution of patients (as a table or plot). Note that 
 #we provide a file with the data compiled (allData.csv), so that this task is not 
 #dependent on completion of the other tasks.
#Process:
#1. The instructions say that we can use the compiled allData.csv file so that this task
  #is not dependent upon the other tasks, so read the allData.csv file.
#2. Determine the number of screens run
#3. Determine the percentage of patients screened that were infected
#4. Determine the percent of patients that identify as male and the percent of patients
  #that identify as female.
#5. Make a graph to show the age distribution of patients (histogram using ggplot)

combinedData <- "C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/allData.csv" #from this point onwards, the instructions say that using the allData.csv file is allowed
summarize <- function(combinedData = "C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/allData.csv"){ #function
  data <- read.csv(combinedData) #read the combinedData file
  library(ggplot2) #load ggplot library
  library(cowplot) #load cowplot library
  infectedPatients <- 0 #initially set the number of infected patients counter to 0
  males <- 0 #initially set the number of males counter to 0
  NumberOfScreensRun <- nrow(data) - 1 #this is the number of tests conducted (the -1 is due to the first row being the column names)
  print(paste("The number of screens run between both countries was", {NumberOfScreensRun})) #print the number of screens run as output
  for(i in 1:nrow(data)){ #iterate through each row of the combined data file
    markersSum <- data$marker01[i] + data$marker02[i] + data$marker03[i] + data$marker04[i] + data$marker05[i] + data$marker06[i] + data$marker07[i] + data$marker08[i] + data$marker09[i] + data$marker10[i] #sum the values of the markers to check for positivity
    if(markersSum > 0){ #if the number of positive markers is greater than 0
      infectedPatients <- infectedPatients + 1 #the number of infectedPatients is increased by 1 if the test is positive
    }
    percentInfected <- 100*(infectedPatients/NumberOfScreensRun) #multiply the proportion of infected patients divided by the number of screens run by 100% to convert the number of patients infected to the percentage of patients who are infected
  }
  print(paste("The percentage of patients who are infected is ", percentInfected, "percent.")) #print the percentage of patients who are infected
  for(j in 1:nrow(data)){ #iterate through each row of the combined data
    if(data$gender[j] == "male") #check whether the patient is male
      males <- males + 1 #if the patient is male, add 1 to the males counter
  }
  percentMale <- 100*(males/NumberOfScreensRun) #multiply 100% by the proportion of patients who are male
  percentFemale <- 100 - percentMale #find the percentage of patients who are female by subtracting the percentage of patients who are male from 100 percent
  print(paste("The percentage of patients who identify as male is", percentMale, "percent.")) #print the percentage of patients who are male
  print(paste("The percentage of patients who identify as female is", percentFemale, "percent.")) #print the percentage of patients who are female
  agePlot <- ggplot(data = data, aes(x = age)) + #make a histogram for the age distribution; similar to an example in lecture 20
    geom_histogram(binwidth = 1, fill = "red", color = "black") + #make a histogram with a bar for every year, red bars, and bars with a black outline
    xlab("Age (years)") + #make the x label age
    ylab("Number of patients") + #make the y label the frequency of each age
    theme_classic() #change the theme of the histogram
  print(agePlot) #print the plot
  return(agePlot) #return statement
}
summarize("C:/Users/grace/Desktop/Biocomputing 2023/R/Final Project/Biocomputing-Final-Project/allData.csv") #run the summarize function with the allData.csv file