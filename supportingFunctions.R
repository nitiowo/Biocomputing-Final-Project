### Supporting Functions
### Emily Velasco, Lauren Schnabel, Judith Lanahan
### 12/14/23

# Function 1: fileConverter
# this function creates .csv files from all the .txt files in a directory
# usage: fileConverter(workingDir)
# arguments: workingDir is the absolute path to the directory containing the files to be converted

fileConverter<-function(workingDir){
  # set the working directory according to the user input
  setwd(workingDir)
  
  # list the .txt files to be converted
  files2convert<-list.files(workingDir, pattern = ".txt")
  
  # loop iterates through the list of files to be converted
  for(i in 1:length(files2convert)){
    # split the name of the file according to the "." and create a list of name elements
    # nameElements[1] is the name of the file without .txt
    nameElements<-unlist(strsplit(files2convert[i], split = "\\."))
    # read the data from the file into temporary data frame
    dfHolder<-read.table(files2convert[i], header = TRUE, sep = " ")
    # write the data from the .txt file into a new .csv file
    write.csv(dfHolder, paste(nameElements[1],".csv"), row.names=FALSE)
  }
}


# Function 2: compiledCSV 
# this function will combine all the .csv files in directories chosen by the user into one .csv
# usage: compiledCSV(workingDir)
# arguments: workingDir is the absolute path to the directory that holds the directories with .csv files to be combined
# assumes that the directory names inside the working directory are country names
# assumes that each .csv has the same format and that the user wants to compile all .csv files in a directory

compiledCSV<-function(workingDir){
  # set the working directory to the user inputted working directory
  setwd(workingDir)
  
  # get number of directories from user
  dirNum<-as.integer(readline("How many directories will you pull files from?  "))
  
  # make an empty list whose length is the number of user inputted directories
  dirList<-numeric(dirNum)
  
  # loop through number of directories and prompt user to input the name of the directory for that iteration
  # variable dirList[i] will replace the placeholder 0 with the relative path to the directory in that list
  for(directory in 1:dirNum){
    dirList[directory]<-readline(prompt = paste("Name of directory ",directory, ": "))
  }
  
  # this block builds an empty data frame to later hold the complete data
  # prompts the user for an example file to copy format
  # assumes that the user follows directions and enters an existing file of the form .csv
  fileFormat<-readline(prompt=paste("Enter the first file in ", dirList[1], "with the desired file format, should end in .csv: "))
  
  # get the column names from the first .csv file
  getCols<-read.csv(paste(dirList[1],"/",fileFormat, sep=''), header = TRUE)
  
  # make a vector of all the required column names, adding a country column and day of year column
  newCols<-c(colnames(getCols), "Country", "dayOfYear")
  
  # make an empty data frame in which to compile all .csv files
  allData<-data.frame(matrix(nrow=0, ncol=length(newCols)))
  
  # add the column names to the empty data frame
  colnames(allData)<-newCols
  
  # these nested loops compile the data from all the .csv files in each directory into data frame allData
  # first loop iterates through the number of country directories inputted by user 
  for(i in 1:length(dirList)){
    # get the country name for that iteration
    countryElem<-dirList[i]
    # list all .csv files for that country's directory
    files<-list.files(dirList[i], pattern=".csv")
    # get the column names for the country's first .csv
    getCols<-read.csv(paste(countryElem,"/",files[1], sep=''), header = TRUE)
    # name a vector of the all required column names
    newCols<-c(colnames(getCols), "Country", "dayOfYear")
    # make an empty data frames with the correct number of columns for that country
    currentData<-data.frame(matrix(nrow=0, ncol=length(newCols)))
    # add the column names vector to the empty data frame
    colnames(currentData)<-newCols
    
    # second loop iterates through each of the files in directory [i]
    for(j in 1:length(files)){
      # split the name of the file into a list of name elements by the underscore (file names should be of the form screen_dayOfYear.csv)
      nameElem<-unlist(strsplit(files[j], "_"))
      # take the second element in nameElem to extract day of year
      # split the name element by the period
      dayElem<-unlist(strsplit(nameElem[2], split = "\\."))
      # take the first element in dayElem as the day of year
      dayofYear<-dayElem[1]
      # read the .csv file that corresponds to country i and file j into a data frame
      df<-read.csv(paste(countryElem,"/",files[j], sep = ""), header = TRUE)
      
      # iterate through each row of the data frame to add Country and dayOfYear values
      for(z in 1:nrow(df)){
        # assign country i to row z of the data frame
        df$Country[z]<-countryElem
        # add the day of year the screen took place to row z of the data frame
        df$dayOfYear[z]<-dayofYear
      }
      
      # combine df with the data frame for country i
      currentData<-rbind(currentData, df)
    }
    
    # combine the data frames for all countries
    allData<-rbind(allData,currentData)
  }
  
  # the second half of this function determines whether the user wants to remove, be warned of, or ignore NAs in the data set
  # determine user preference
  findNA<-readline(prompt = "Would you like to remove rows containing N/As? y/n: ")
  
  #in the event that user wants to remove NA rows
  if(findNA == "y"){
    # deletes rows with NAs
    allData2<-na.omit(allData)
    
    # creates final .csv file
    write.csv(allData2, file="compiledData.csv", row.names=FALSE)
  
  # in the event that the user does not want to remove rows with NAs   
  }else if(findNA == "n"){
    
    # asks if the user would like to be warned
    warnNA<-readline(prompt = "Would you like to be warned of N/As? y/n: ")
    
    # if the user would like to be warned of NAs
    if(warnNA == "y"){
      # sums all occurrences of NAs in allData.  If NAtest>0, there are NAs present
      NAtest<-sum(is.na(allData))
      if(NAtest>0){
        print("Warning: NAs present in data")
      }else{
        print("No NAs in data")
      }
      #creates final .csv file
      write.csv(allData2, file="compiledData.csv", row.names=FALSE)
      
      # in the event that the user is satisfied with unclean data
    }else if(warnNA=="n"){
      write.csv(allData2, file="compiledData.csv", row.names=FALSE)
    }
    
    # in the event that the user types in something instead of y or n
    # default is unchanged CSV
  }else{
    print("Input not understood. Creating csv with N/As")
    write.csv(allData2, file="compiledData.csv", row.names=FALSE)
  }
  
}


# Function 3: dataSummary
# this function summarizes the data set and returns number of screens run, the percent of infected patients, 
# percent of patients that identify as male or female, and an age distribution as a plot
# usage: dataSummary(workingDir, file)
# arguments: workingDir is the directory containing the file to be summarized, file is the compiled .csv file




dataSummary<-function(workingDir, file){
  # set working directory 
  setwd(workingDir)
  
  # read in data from user specified file
  allData<-read.csv(file, header=TRUE)
  
  #step 1-find number of screens
  numberofScreens<-nrow(allData)
  
  ## step 2-find percent infected
  # make a variable for infected total
  infected = 0
  # make a data set with just marker columns
  marker_columns <- allData[, 3:12]
  # for each row in data set
  for(i in 1:nrow(marker_columns)){
    # find the number of markers found in a patient
    markerCount <- rowSums(marker_columns[i, ])
    # if the markerCount is greater than 0, at least one marker is present, indicating infection
    if(markerCount > 0){
      # increase the infected count by one
      infected = infected + 1
    }
  }
  
  # calculate the percent infected
  percentInfected <- (infected/numberofScreens) * 100
  
  ## step 3- find percent male or female
  
  # make empty variables
  male = 0
  female = 0
  
  # iterate through each row of allData
  for(i in 1:nrow(allData)){
    # find the columns that are 'male'
    if(allData$gender[i] == "male"){
      male = male + 1
      # find the columns that are female  
    }else if(allData$gender[i] == "female"){
      female = female + 1
    }
  }
  
  # find male percentage
  percentMale <- (male/numberofScreens) * 100
  
  # find female percentage
  percentFemale <- (female/numberofScreens) * 100
  
  ## step 4- age distribution of patients
  
  # make a new data frame with the ages of the patients
  ageData<- data.frame(allData$age)
  # add proper column name to data frame
  names(ageData) <- 'Patient.Age'
  
  # load in graphing tools
  library(ggplot2)
  # make a distribution graph
  ageGraph <- ggplot(data=ageData, aes(x=Patient.Age)) +
    geom_histogram(binwidth = 1, fill="purple", color="black") +
    theme_bw() +
    xlab("Patient Age") +
    ylab("Count")+
    ggtitle("Age Distribution of Patients")
  
  # present data to user 
  print(paste("The number of screens run is: ", numberofScreens))
  print(paste("Percent of patients that were infected: ", percentInfected,"%"))
  print(paste("Percent of Male Patients: ", percentMale,"%"))
  print(paste("Percent of Female Patients: ", percentFemale,"%"))
  
  # displays age graph
  ageGraph
}