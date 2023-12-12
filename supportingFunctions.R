## R Final Project--Functions

#create a function to read in csv files for country Y directory (.txt files)
delimiterFunction <- function(directory='C:/Users/cdmix/Desktop/Biocomputing/Biocomputing-Final-Project/countryY'){
  setwd(directory)
  files <- list.files()
  #create a for loop to convert the files of the files variable into csvs in the countryY directory
  for(i in 1:length(files)){
    #read the files into a separate variable
    txtFiles <- read.table(file=files[i],header=T)
    #ensure you are in the right working directory
    setwd('C:/Users/cdmix/Desktop/Biocomputing/Biocomputing-Final-Project/countryY')
    #write the files in the variable into csv files
    write.csv(txtFiles,file=sub('.txt','.csv',files[i]),row.names=F)
  }
}
delimiterFunction()

combinecsv <- function(countryY_path='C:/Users/cdmix/Desktop/Biocomputing/Biocomputing-Final-Project/countryY', 
                       countryX_path= 'C:/Users/cdmix/Desktop/Biocomputing/Biocomputing-Final-Project/countryX', 
                       csv_final_location= 'C:/Users/cdmix/Desktop/Biocomputing/Biocomputing-Final-Project', NA_choice= 'include') {
  
  setwd(countryY_path)
  filelist <- list.files(pattern='.csv')
  #for loop binding rows of the country files together
  comb <- data.frame()
  for(i in 1:length(filelist)){
    temp <- read.csv(filelist[i], header=TRUE)
    #create a column for the day of surveillance
    z <- strsplit(filelist[i],'_')
    y <- strsplit(z[[1]][2],'[.]')
    DayofYear <- as.numeric(y[[1]][1])
    temp <- cbind(temp,DayofYear)
    temp$CountryName <- 'CountryY'
    comb <- rbind(comb,temp)
  }
  #write into a single csv file for the whole country
  write.csv(comb, 'combinedY.csv', row.names=FALSE)
  
  setwd(countryX_path)
  filelist <- list.files(pattern='.csv')
  #for loop binding rows of the country files together
  comb <- data.frame()
  for(i in 1:length(filelist)){
    temp <- read.csv(filelist[i], header=TRUE)
    #create a column for the day of surveillance
    z <- strsplit(filelist[i],'_')
    y <- strsplit(z[[1]][2],'[.]')
    DayofYear <- as.numeric(y[[1]][1])
    temp <- cbind(temp,DayofYear)
    temp$CountryName <- 'CountryX'
    comb <- rbind(comb,temp)
  }
  #write into a single csv file for the whole country
  write.csv(comb, 'combinedX.csv', row.names=FALSE)
  
  #combining both combined country files into one large file
  setwd(countryY_path)
  ycsv <- read.csv('combinedY.csv', header=TRUE)
  setwd(countryX_path)
  xcsv <- read.csv('combinedX.csv', header=TRUE)
  combine <- rbind(ycsv, xcsv)
  setwd(csv_final_location)
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
combinecsv()

#working directory MUST contain the allData.csv file
setwd('C:/Users/cdmix/Desktop/Biocomputing/Biocomputing-Final-Project')
summ <- function(data='allData.csv') {
  dat <- read.csv(data, header=TRUE)
  screen <- nrow(dat)
  cat('number of screens ran = ', screen, '\n')
  #column named "gender" in data
  cat('percent female = ', sum(dat$gender=='female')/nrow(dat), '\n')
  cat('percent male = ', sum(dat$gender=='male')/nrow(dat), '\n')
  #markers will always be located in col 3-12
  infected <- 0
  for (i in 1:nrow(dat)){
    if (sum(dat[i,3:12]) >=1){
      infected <- infected + 1
    }else{
      infected <- infected
    }
  }
  cat('percent infected =', (infected)/screen, '\n')
  #plot for distribution of gender and age
  library(ggplot2)
  ggplot(dat,aes(gender,age))+geom_violin()
  #table that shows distribution of gender and age
  return(table(dat$gender, dat$age))
}
summ()
