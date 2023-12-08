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


