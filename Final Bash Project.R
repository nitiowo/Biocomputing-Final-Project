##Final R Project##

##how to use strsplit: 
##strsplit(x, "_") --> splits things along the character "_" in a vector

getwd()
setwd("/Users/faithschafer/Downloads/Biocomputing-Final-Project/countryY")
delimiter2 <- function(directory="/Users/fschafer/Downloads/Biocomputing-Final-Project/countryY") {
  #in this function we assume that the user is providing a full path to the folder
  setwd(directory)
  folder <- list.files(pattern= ".txt")
  
  for (i in 1:length(folder)) {
    files <- read.table(file=folder[i],header=T)
    write.csv(files, file=sub(".txt", "_Y_.csv", folder[i]), row.names=F)
  }
}
delimiter2("/Users/faithschafer/Downloads/Biocomputing-Final-Project/countryY")


setwd("/Users/faithschafer/Downloads/Biocomputing-Final-Project/countryY")
filelist = list.files(pattern = ".txt")
for (i in 1:length(filelist)){
  input<-filelist[i]
  output<-paste0(input, ".csv")
  data = read.delim(input, header = TRUE)   
  setwd("/Users/faithschafer/Downloads/Biocomputing-Final-Project/countryY")
  write.table(data, file=output, sep=",", col.names=TRUE, row.names=FALSE)
  setwd("/Users/faithschafer/Downloads/Biocomputing-Final-Project/countryY")
}
output <- paste0(gsub("\\.txt$", "", input), ".csv")

getwd()
setwd("/Users/faithschafer/Downloads/Biocomputing-Final-Project")

filelist <- list.files(pattern=".csv")
#for loop binding rows of the country files together
for(i in length(filelist)){
  temp <- read.csv(filelist[i])
  #create a column for the day of surveillance
  z <- strsplit(filelist[i],"_")
  y <- strsplit(z[2],".")
  x <- y[1]
  temp <- cbind(temp,x)
  #combine each iteration of the for loop into a single file
  comb <- rbind(comb,temp)
}
#add the country name to the items in the dataset
countryname <- c('countryY')
ycomb <- cbind(ycomb,countryname)
}
combined <- 
  
  function(country1_path="/Users/fschafer/Downloads/Biocomputing-Final-Project/countryY", 
           country2_path= "/Users/fschafer/Downloads/Biocomputing-Final-Project/countryX", 
           csv_final_location= "/Users/fschafer/Downloads/Biocomputing-Final-Project", NA_choice= “include”) {
    
    setwd(country1_path)
    filelist <- list.files(pattern='.csv')
    #for loop binding rows of the country files together
    comb <- data.frame()
    for(i in 1:length(filelist)){
      temp <- read.csv(filelist[i], header=T)
      #create a column for the day of surveillance
      z <- strsplit(filelist[i],'_')
      y <- strsplit(z[[1]][2],'[.]')
      DayofYear <- as.numeric(y[[1]][1])
      temp <- cbind(temp,DayofYear)
      temp$CountryName <- "CountryY"
      
    }
    comb <- rbind(comb,temp)
    write.csv(comb, "combinedY.csv", row.names=F)
    
    setwd(country2_path)
    filelist <- list.files(pattern='.csv')
    #for loop binding rows of the country files together
    comb <- data.frame()
    for(i in 1:length(filelist)){
      temp <- read.csv(filelist[i], header=T)
      #create a column for the day of surveillance
      z <- strsplit(filelist[i],'_')
      y <- strsplit(z[[1]][2],'[.]')
      DayofYear <- as.numeric(y[[1]][1])
      temp <- cbind(temp,DayofYear)
      temp$CountryName <- "CountryX"
      #combine each iteration of the for loop into a single file
    }
    comb <- rbind(comb,temp)
    write.csv(comb, "combinedX.csv", row.names=F)
    
    #combining both
    setwd(country1_path)
    csvY <- read.csv("combinedY.csv", header=T)
    setwd(country2_path)
    csvX <- read.csv("combinedX.csv", header=T)
    combine <- rbind(csvY, csvX)
    setwd(csv_final_location)
    write.csv(combine, "combined_data.csv", row.names = FALSE)
    
    #checking for NAs
    dat <- read.csv("combined_data.csv", header=T)
    #if NA statements
    if (NA_choice=="remove") {
      dat <- na.omit(dat)
      write.csv(dat, "combined_data.csv", row.names =F)
    } else if (NA_choice=="warn") {
      print("there are NAs in the data, and they will be included in calculations")
    } else if (NA_choice=="include"){
    }
  }



##part 3:
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
      infected <- infected+0
  }
  cat("the percentage infected =", (screens-infected)/screens, "\n")
  
  #table that shows distribution of gender and age
table <-(table(dat$gender, dat$age))
return table

}
summarize()

library(ggplot2)


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
  #table that shows distribution of gender and age
  library(ggplot2)
  ggplot(dat,aes(gender,age))+geom_violin()
  return(table(dat$gender, dat$age))
}
summarize()

ggplot(dat,aes(gender,age))+geom_violin()

# Read the data from the combined file
data <- read.csv("alldata.csv")

# Find number of patients infected each day
data$InfectedCount <- rowSums(data[, 3:12] > 0)

# Separate the data by Country X or Y and by the day of year the patient is infected
sorted_data <- data[order(data$country, data$dayofYear), ]

# Calculate cumulative sum of patients infected for Country X and Country Y
sorted_data$CumulativeInfected <- unlist(lapply(split(sorted_data$InfectedCount, sorted_data$country), cumsum))


# Plotting a line graph comparing each Country's cumulative outbreak numbers
library(ggplot2)

ggplot(sorted_data, aes(x = dayofYear, y = CumulativeInfected, color = country, group = country)) +
  geom_line() +
  labs(x = "Day", y = "Cumulative Infected Patients",
       title = "Cumulative Disease Outbreak Comparison") +
  theme_minimal() +
  scale_color_manual(values = c("darkgreen", "red"))
  
