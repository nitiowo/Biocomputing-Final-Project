### second try

setwd("C:/Users/evelasco/Desktop/Biocomputing-Final-Project-main")

# this function converts all the .txt files in a directory into .csv files
# usage: fileConverter(workingDir)
# arguments: workingDir is the absolute path to the directory containing the files to be converted

fileConverter<-function(workingDir){
  # set the working directory according to the user
  setwd(workingDir)
  # list the .txt files to be converted
  files2convert<-list.files(workingDir, pattern = ".txt")
  # iterate through the list of files to be converted
  for(i in 1:length(files2convert)){
    # split the name of the file according to the "." and create a list of vectors
    nameElements<-unlist(strsplit(files2convert[i], split = "\\."))
    # read the data from the file
    dfHolder<-read.table(files2convert[i], header = TRUE, sep = " ")
    # write the data from the .txt file into a .csv file
    write.csv(dfHolder, paste(nameElements[1],".csv"), row.names=FALSE)
  }
}


### second function: getting all data into one csv
# this funciton will combine all the .csv files into one .csv
# usage: bigCSV(workingDir)
# arguments: workingDir is the absolute path to the directory that holds the directories with .csv files


#### note from emily: do we wanna change the argument name? the previous function sets the workingDir to the country
#### but we need the second working directory to contain all the countries directories
bigCSV<-function(workingDir){
  # set the working direcorty to the user inputed working directory
  setwd(workingDir)
  # get number of directories from user
  dirNum<-as.integer(readline("How many directories?  "))
  # make a list whose length is the number of user inputed directories
  dirList<-numeric(dirNum)
  # loop through number of directories and prompt user to input the name of the directory for that iteration
  # variable dirList[i] will replace the placeholder 0 with the directory path in that list
  for(i in 1:dirNum){
    dirList[i]<-readline(prompt = paste("Path to directory ",i, ": "))
  }
  # make a for loop to get the files from the directory
  
  # sets the country name as the first entry in the dirList 
  country1<-dirList[1]
  # lists the .csv files in that country's directory
  files<-list.files(country1, pattern=".csv")
  # get the column names from the first .csv file
  getCols<-read.csv(paste(country1,"/",files[1], sep=''), header = TRUE)
  # make a vector of all the required column names
  newCols<-c(colnames(getCols), "Country", "dayOfYear")
  # make an empty data frame in which to compile all .csv files
  allData<-data.frame(matrix(nrow=0, ncol=length(newCols)))
  # add the colum names to the empty data frame
  colnames(allData)<-newCols
  
  # iterate through the number of country directories that the user inputted
  ##### note from Emily: I don't think we need this first for loop since we did it above????
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
    # iterate through each of the files in that country's directory
    for(j in 1:length(files)){
      # split the name of the file into a list of vectors by the underscore
      nameElem<-unlist(strsplit(files[j], "_"))
      # take the second vector in the list to find day of year
      # split the vector by a period
      dayElem<-unlist(strsplit(nameElem[2], split = "\\."))
      # take the first vector in dayElem as the day of year
      dayofYear<-dayElem[1]
      # read the .csv file that corresponds to that country and file iteration into a data frame
      df<-read.csv(paste(countryElem,"/",files[j], sep = ""), header = TRUE)
      # iterate through each row of the data frame
      for(z in 1:nrow(df)){
        # add the country to each row of the data frame
        df$Country[z]<-countryElem
        # add the day of year the screen took place to each row of the data frame
        df$dayOfYear[z]<-dayofYear
      }
      # bind df to the dataframe for that country
      currentData<-rbind(currentData, df)
    }
    #### note from emily: what is the difference between currentData and allData
    #### if i understand the first iteration of current data is for country[1] and the 2nd is country [2]?
    #### and allData combines the two+ countries?
    
    
    # combine all the combined dataframes for all countries
    allData<-rbind(allData,currentData)
  }
  
  # find out what user wants
  findNA<-readline(prompt = "Would you like to remove rows containing N/As? y/n: ")
  
  #in the event that user wants to get rid of NA rows
  if(findNA == "y"){
    allData2<-na.omit(allData)
    return(allData2)
    
  }else if(findNA == "n"){
    
    
    #if they would like to be warned...
    warnNA<-readline(prompt = "Would you like to be warned of N/As? y/n: ")
    if(warnNA == "y"){
      NAtest<-sum(is.na(allData))
      if(NAtest>0){
        print("Warning: NAs present in data")
      }
      # in the event that the user is satisfied with unclean data
    }else if(warnNA=="n"){
      print("I shall give you the unclean CSV")
    }
    
    # in the event that the user types in something weird instead of y or n
    #default is unclean CSV
  }else{
    print("Input not understood. Creating csv with N/As")
  }
  
}


newData<-df[df$dayOfYear==120,]
sum(newData[,3:12])
