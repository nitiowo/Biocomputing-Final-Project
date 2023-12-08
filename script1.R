### supportingFunctions.R contains the code for functions required during analysis
###Biocomputing Final Project
### Judith Lanahan, Emily Velasco, Lauren Schnabel
### 12/14/23

### set working directory

wd<-readline("Type your working directory: ")
setwd(wd)


# this makes lists of the files within the directories countryY and countryX
screenListY<-list.files(path = "countryY")

screenListX<-list.files(path = "countryX")


## to get into csv, first read in text data to a dataframe in R then export as a csv file.
# this reads in an example countryY text file into a table
# could be useful for converting into csv...

screen_120<-read.table("countryY/screen_120.txt", header = TRUE, sep = " ")

# this one converts into csv. 

write.csv(screen_120, "countryY/screen_120.csv", row.names= FALSE)

setwd("countryY")
for(i in 1:5){
  dfHolder<-read.table(screenListY[i], header = TRUE, sep = " ")
  write.csv(dfHolder, paste("countryY/",screenListY[i],".csv"), row.names = FALSE)
}

nameElements<-strsplit(screenListY[1],split="\\.")
nameElements
paste("countryY/",screenListY[1],".csv", sep = "")


setwd("C:/Users/Judith Lanahan/OneDrive/Desktop/Biocomputing/R/FinalProject/Biocomputing-Final-Project")



## second function practice stuff
# get number of directories from user
dirNum<-readline("How many directories?  ")
# make a list whose length is the number of user inputed directories
dirList<-numeric(dirNum)
# loop through number of directories and prompt user to input the name of the directory for that iteration
# variable dirList[i] will replace the placeholder 0 with the directory path in that list
for(i in 1:dirNum){
  dirList[i]<-readline(prompt = paste("Path to directory ",i, ": "))
}

dirList

# make a for loop to get the files from the directory

for(i in 1:length(dirList)){
  countryElem<-dirList[i]
  files<-list.files(dirList[i], pattern=".csv")
  dataList<-numeric(length(files))
  for(j in 1:length(files)){
    nameElem<-unlist(strsplit(files[j], "_"))
    dayElem<-unlist(strsplit(nameElem[2], split = "\\."))
    dayofYear<-dayElem[1]
    df<-read.csv(files[j], header = TRUE)
    for(z in 1:nrow(df)){
      df$Country[z]<-countryElem
      df$dayOfYear[z]<-dayElem
    }
    dataList[j]<-df
  }
}












