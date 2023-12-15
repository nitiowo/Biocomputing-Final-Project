#Introduction to Biocomputing R project - Supporting functions R
#Mark Jantz & Chris Rizkalli 
#3 diff functions

#clear preexisting variables 
rm(list=ls())

####################
#txt to csv FUNCTION
####################

#function to convert .txt files in a director to a .csv file 
#USAGE txt_to_csv(folder_path) just enter folder containing txt files
txt_to_csv <- function(folder_path) {
  #set working directory to file folder
  setwd(folder_path)
  #create a list of all txt files in the designated folder_path from user input
  csv_files <- list.files(folder_path, pattern = "\\.txt$", full.names = TRUE)
  
  #for loop to go through list of all txt files and convert to csv file  
  for (file in csv_files) {

    # Read the data from the txt file
    final_data <- read.table(file, header = TRUE, sep = " ")
    
    # Construct the file name for output by first extracting day from original file name
    day_str <- as.character(strsplit(file, "screen_|\\.txt")[[1]][2])
    #create a new file_name to write outputfile
    output_name <- paste("screen_", day_str, ".csv",sep="")
    
    # Write the result as an individual CSV file
    write.csv(final_data,output_name, row.names = FALSE)
    
    #remove original .txt file 
    file.remove(file)
}
}


######################
#compile csvs FUNCTION
######################

#function to convert text files in a director to a CSV file 
#Usage put source_path to folder containing csv files desired to compile
#Usage: put output_path as the filepath where the final alldata.csv file should go
compile_csvs <- function(source_path,output_path) {

# Create an empty dataframe to store compiled data
  final_data <- data.frame()
  
# Get a list of all .csv files in the source path folder specified
 csv_files <- list.files(source_path, pattern = "\\.csv$", full.names = TRUE)

  # Loop through each .csv file in the source path folder specified
  for (file in csv_files) {
    screen_data <- read.csv(file, header = TRUE)
    # Extract country and dayofYear information from the file name and put it in respective columns
    screen_data$country <- strsplit(source_path, "country")[[1]][2]
    screen_data$dayofYear <- strsplit(file, "screen_|\\.csv")[[1]][2]
    # Append data to a final dataframe
    final_data <- rbind(final_data,screen_data)
  }
  
  # Collects user input of what they want to do with NA values
  naval <- readline(prompt = "Please enter 1-3 to decide how to address NA's:
  1 = remove rows with NA’s in any columns
  2 = include NAs w/ warning
  3 = include NAs w/ no warning")
  # if else statement to respond to user input to deal with NA values
  if (naval == 1) {
    final_data <- na.omit(final_data)
    print("WARNING: Rows with NA values in data have been removed")
  } else if (naval == 2) {
    print("WARNING: Any NA values in DATA")
  } else if (naval == 3) {
    # Ignore NA values
  } else {
    print("Invalid input. Please re run function and enter a number between 1 and 3.")
  }
  
  country_name <- strsplit(source_path, "country")[[1]][2] #create a var with the country name
  output_name<- paste0("compiled_country",country_name,".csv") #create a file name to use when writing the compiled data
  
setwd(output_path)
  # Write the compiled data to a single .csv file
write.csv(final_data, output_name,row.names = FALSE)
  #prints output file location to console
cat("Task complete, data written to:",output_path,"\n")
}


######################
#Merge data   FUNCTION
######################

merge_both <- function() {

# Read the CSV files into data frames
data1 <- read.csv("compiled_countryX.csv")
data2 <- read.csv("compiled_countryY.csv")

# Combine the data frames vertically
combined_data <- rbind(data1, data2)

# Write the combined data to a new CSV file
write.csv(combined_data, "allData.csv", row.names = FALSE)
}

######################
#Data summary FUNCTION
######################

#USAGE: to get descrpitive stats for a compiled file, simply input the folder containing the compiled csv file
data_summary <- function(input_path) {
#Sets WD to path with the input
setwd(input_path)
#Read in all the data, load ggplot
allData<-read.csv("allData.csv")
library(tidyverse)

################
#Screen FUNCTION
################


#Prime number of Screenings
X_Screen_Count <-0
Y_Screen_Count <-0

#Count Screenings in each country row by row, and print that data
for (i in 1:nrow(allData)) {
  if (allData$country[i] == "X") {
    X_Screen_Count = X_Screen_Count + 1
  } else {
    Y_Screen_Count = Y_Screen_Count +1
  }
}

total_count <- X_Screen_Count + Y_Screen_Count
cat("Country X screened", X_Screen_Count, "people, while Country Y screened", Y_Screen_Count, ", for a total of", total_count, "tests!","\n")


################
#Infect FUNCTION
################


total_infections <-0

for (i in 1:nrow(allData)) {
  if (allData$marker01[i] == "1") {
    total_infections = total_infections +1
  } else if (allData$marker02[i] == "1") {
    total_infections = total_infections +1
  } else if (allData$marker03[i] == "1") {
    total_infections = total_infections +1
  } else if (allData$marker04[i] == "1") {
    total_infections = total_infections +1
  } else if (allData$marker05[i] == "1") {
    total_infections = total_infections +1
  } else if (allData$marker06[i] == "1") {
    total_infections = total_infections +1
  } else if (allData$marker07[i] == "1") {
    total_infections = total_infections +1
  } else if (allData$marker08[i] == "1") {
    total_infections = total_infections +1
  } else if (allData$marker09[i] == "1") {
    total_infections = total_infections +1
  } else if (allData$marker10[i] == "1") {
    total_infections = total_infections +1
  }
}
percent_infected<- round(((total_infections/total_count)*100),digits = 4)

cat("There was a total of", total_infections, "infections.","making the percent of Infected Patients:", percent_infected,"%", "\n")


################
#GENDER FUNCTION
################


#Prime counter for gender percentage
male <-0
female <-0

#Calculate gender percentage and print that data
for (i in 1:nrow(allData)) {
  if (allData$gender[i] == "male") {
    male = male + 1
  } else {
    female = female +1
  }
}
male_percent <- round((100* (male/total_count)),digits=4)
female_percent <-round((100* (female/total_count)),digits=4)

cat("The screenings were composed of", male_percent, "% males, and", female_percent, "% females.","\n")


################
#Age Px FUNCTION
################


#Age Px Screened Pyramid
ggplot(allData, aes(y = age, fill=gender)) +   # Fill column
  geom_bar(width = .6) +   # draw the bars
  labs(title="Age Pyramid")
}

