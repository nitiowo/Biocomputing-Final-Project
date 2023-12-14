## New final project draft ##

setwd("/Users/faithschafer/Downloads/Biocomputing-Final-Project")


## code to begin and for question 1: (creating the script for SupportingFunctions.R)
getwd()
setwd("/Users/faithschafer/Downloads/Biocomputing-Final-Project/countryY")

delimiter2 <- function(directory="/Users/faithschafer/Downloads/Biocomputing-Final-Project/countryY") {
  #in this function we assume that the user is providing a full path to the folder
  setwd(directory)
  folder <- list.files(pattern= ".txt")
  
  for (i in 1:length(folder)) {
    files <- read.table(file=folder[i],header=T)
    write.csv(files, file=sub(".txt", ".csv", folder[i]), row.names=F)
  }
}
delimiter2("/Users/faithschafer/Downloads/Biocomputing-Final-Project/countryY")

setwd("/Users/faithschafer/Downloads/Biocomputing-Final-Project/countryY")

## Part 2: 


combinecsv <- function(path_to_countryY='C:/Users/faithschafer/Downloads/Biocomputing/Biocomputing-Final-Project/countryY', 
                       path_to_countryX= 'C:/Users/faithschafer/Downloads/Biocomputing/Biocomputing-Final-Project/countryX', 
                       csv_final= 'C:/Users/faithschafer/Downloads/Biocomputing/Biocomputing-Final-Project', NA_choice= 'include') {
  
  setwd(path_to_countryY)
  filelist <- list.files(pattern='.csv')
  #for loop binding rows of the country files together
  comb <- data.frame()
  for(i in 1:length(filelist)){
    var <- read.csv(filelist[i], header=TRUE)
    #create a column for the day of surveillance
    z <- strsplit(filelist[i],'_')
    y <- strsplit(z[[1]][2],'[.]')
    DayofYear <- as.numeric(y[[1]][1])
    var <- cbind(var,DayofYear)
    var$CountryName <- 'CountryY'
    comb <- rbind(comb,var)
  }
  #write into a single csv file for the whole country
  write.csv(comb, 'combinedY.csv', row.names=FALSE)
  
  setwd(path_to_countryX)
  filelist <- list.files(pattern='.csv')
  #for loop binding rows of the country files together
  comb <- data.frame()
  for(i in 1:length(filelist)){
    var <- read.csv(filelist[i], header=TRUE)
    #create a column for the day of surveillance
    z <- strsplit(filelist[i],'_')
    y <- strsplit(z[[1]][2],'[.]')
    DayofYear <- as.numeric(y[[1]][1])
    var <- cbind(var,DayofYear)
    var$CountryName <- 'CountryX'
    comb <- rbind(comb,var)
  }
  
  #write into a single csv file for the whole country
  write.csv(comb, 'combinedX.csv', row.names=FALSE)
  
  #combining both combined country files into one large file
  setwd(path_to_countryY)
  csv.y <- read.csv('combinedY.csv', header=TRUE)
  setwd(path_to_countryX)
  csv.x <- read.csv('combinedX.csv', header=TRUE)
  combine <- rbind(csv.y, csv.x)
  setwd(csv_final)
  write.csv(combine, 'combined_data.csv', row.names = FALSE)
  
  #check for NAs
  dat <- read.csv('combined_data.csv', header=TRUE)
  #if NA statements
  if (NA_choice=='remove') {
    dat <- na.omit(dat)
    write.csv(dat, 'combined_data.csv', row.names =FALSE)
  } else if (NA_choice=='warn') {
    print('NAs present, they will be included in calculations')
  } else if (NA_choice=='include'){
  }
}


## question 3

setwd("/Users/faithschafer/Downloads/Biocomputing-Final-Project")

#assume that the user’s working directory contains the csv file
summarize <- function(data="allData.csv") {
  dat <- read.csv(data, header=T)
  screens <- nrow(dat)
  cat("the number of screens run =", screens, "\n")
  #assume that there is a column called "gender" in data
  cat("the percentage of females = ", sum(dat$gender=="female")/nrow(dat), "\n")
  cat("the percentage of males = ", sum(dat$gender=="male")/nrow(dat), "\n")
  
  #assume that the markers are always located in columns 3-12
  infected <- 0
  for (i in 1:nrow(dat)) {
    if (sum(dat[i,3:12]) >=1) {
      infected <- infected + 1
    } else
      infected <- infected + 0
  }
  cat("the percentage infected =", (infected)/screens, "\n")
  #plot that shows the distribution of gender and age
  ## to get a visual of whether the disease affects people at the same demographic levels (males and females, across all ages)

  library(ggplot2)
  ggplot(dat,aes(gender,age))+geom_violin(color = "navy")
  return(table(dat$gender, dat$age))
}
summarize()
