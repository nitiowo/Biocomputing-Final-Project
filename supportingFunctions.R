### SupportFunction.R 
## R Final Project 
# Allie Wu and Emily Rao
# Load Libraries
library(ggplot2)

## Function that converts all files in a directory into comma-separated value files.
convertFiles <- function(directory){
  txt_files <- list.files(path = directory, pattern = "\\.txt$", full.names = TRUE)
  for (txt_file in txt_files) {
    # Extract the data 
    data <- read.table(txt_file, header = TRUE, sep = "")
    # Replaces .txt to .csv 
    csv_file <- sub("\\.txt$", ".csv", txt_file)
    # Add to CSV file
    write.csv(data, csv_file, row.names = FALSE, sep = ",")
  }
}

# Usage: convertFiles(directory=”YOURDIRECTORY”)
# Example below to check: 
convertFiles("~/Biocomputing-Final-Project/countryY")

## Function to compile data from all .csv files
compileFiles <- function(directory){
  csv_files <- list.files(path=directory, pattern = "\\.csv$", full.names = TRUE)
  # Make empty dataframe
  compiled_csv <- data.frame()
  # Make for loop to loop through each .csv file 
  for (i in csv_files){
    data <- read.csv(i, header = TRUE)
    # Use strsplit to get the country name and day of year
    data$country <- strsplit(directory, "country")[[1]][2]
    data$dayofYear <- strsplit(strsplit(i, "screen_")[[1]][2], "\\.")[[1]][1]
    # combiles it into compiled_csv
    compiled_csv <- rbind(compiled_csv, data)
  }
  # Asks user what they want to do with NA values
  print("What do you want to do with the NA values? Choose: remove NA, include NA 
      and warning, include NA no warning")
  # Collects user input
  userInput <- readline(prompt= "remove NA, include NA 
      and warning, include NA no warning")
  # if else statement to output the correct file
  if (userInput == "remove NA"){
    compiled_csv <- na.omit(compiled_csv)
  } else if (userInput == "include NA and warning"){
      print("Warning: There are NAs in this file.")
  } else {
    }
  write.csv(compiled_csv, paste0(directory,"/compiled_data.csv"), row.names = FALSE)
}

# Usage: compileFiles(directory=”YOURDIRECTORY”)
# Example below to check: 
compileFiles("~/Biocomputing-Final-Project/countryX")

## Function to summarize compiled data 
summary <- function(directory){
  alldata <- read.csv("allData.csv", header=T)
  # Number of screens run
  num_screens <- nrow(alldata)
  cat("Number of Screens Run:", num_screens)
  # Percent male and female
  male <- ((sum(as.numeric(alldata$gender=="male"))) / (num_screens)) * 100
  cat("Percent Male:",male,"%")
  female <- ((sum(as.numeric(alldata$gender=="female"))) / (num_screens)) * 100
  cat("Percent Female:", female,"%")
  # Age distribution of patients 
  age_distr <- ggplot(alldata, aes(x=age)) +
    geom_histogram(alpha=1, position="identity", fill="skyblue", color="black") +
    ggtitle('Distribution of Age') +
    xlab('Age (years)') +
    ylab('Number of Patients')
  age_distr
  # Percent of infected patients
  infected_count <- 0
  for (i in 1:nrow(alldata)){
    if (any(alldata[i,3:12]) == 1){
      infected_count <- infected_count + 1
    } 
    else {
    }
  }
  infected_percent <- (infected_count/num_screens) * 100
  cat("Percent of Infected Patients:", infected_percent,"%")
}

# Usage: Put in the correct directory and run summary()
# Example:summary()