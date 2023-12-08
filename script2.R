### second try


# this function converts all the .txt files in a directory into .csv files
# usage: fileConverter(workingDir)
# arguments: workingDir is the absolute path to the directory containing the files to be converted

fileConverter<-function(workingDir){
  setwd(workingDir)
  files2convert<-list.files(workingDir, pattern = ".txt")
  for(i in 1:length(files2convert)){
    nameElements<-unlist(strsplit(files2convert[i], split = "\\."))
    dfHolder<-read.table(files2convert[i], header = TRUE, sep = " ")
    write.csv(dfHolder, paste(nameElements[1],".csv"), row.names=FALSE)
  }
}


### second function: getting all data into one csv

bigCSV<-function(workingDir){
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
  
  # establishes 
  country1<-dirList[1]
  files<-list.files(country1, pattern=".csv")
  getCols<-read.csv(paste(country1,"/",files[1], sep=''), header = TRUE)
  newCols<-c(colnames(getCols), "Country", "dayOfYear")
  allData<-data.frame(matrix(nrow=0, ncol=length(newCols)))
  colnames(allData)<-newCols
  
  for(i in 1:length(dirList)){
    countryElem<-dirList[i]
    files<-list.files(dirList[i], pattern=".csv")
    getCols<-read.csv(paste(countryElem,"/",files[1], sep=''), header = TRUE)
    newCols<-c(colnames(getCols), "Country", "dayOfYear")
    currentData<-data.frame(matrix(nrow=0, ncol=length(newCols)))
    colnames(currentData)<-newCols
    for(j in 1:length(files)){
      nameElem<-unlist(strsplit(files[j], "_"))
      dayElem<-unlist(strsplit(nameElem[2], split = "\\."))
      dayofYear<-dayElem[1]
      df<-read.csv(paste(countryElem,"/",files[j], sep = ""), header = TRUE)
      for(z in 1:nrow(df)){
        df$Country[z]<-countryElem
        df$dayOfYear[z]<-dayofYear
      }
      currentData<-rbind(currentData, df)
    }
    allData<-rbind(allData,currentData)
  }
  
}

