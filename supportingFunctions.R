# Biocomputing Final Project
# supportingFunctions.R

# This script is intended to contain functions that enhance the efficiency of
# the analysis.R script. It will contain 3 major components. First, it will 
# convert all files into .csv files with matching formats. Second, it will
# complile these .csv files into one master .csv file containing the original 12
# columns, as well as columns for the country and day of screening. Users will
# be able to determine if they want to remove rows containing NA's and be warned
# about this, or include NA rows. Lastly, the a function to convert the main .csv
# doc into relevant data analysis will be included. 






# Function 1: csvConverter2 & convert_all_files_to_csv
# These functions convert all non csv files into csv's
# -------------------------------------------------------------------------------
csvConverter2 <- function(file1) {
  x <- readLines(file1)
  
  # Function to create a new filename with .csv extension
  nameConverter <- function(fileVar) {
    fileName <- basename(fileVar)
    newFileName <- gsub("\\.txt$", ".csv", fileName, ignore.case = TRUE)
    fileDir <- dirname(fileVar)
    newFileDir <- file.path(fileDir, newFileName)
    return(newFileDir)
  }
  
  newFileDir <- nameConverter(file1)
  
  if (any(grepl(",", x)) == TRUE && any(grepl(" ", x)) == FALSE) {
    writeLines(x, newFileDir)
    print("Table is already in .csv format and has been copied to a new .csv file")
  } else if (any(grepl("\t", x)) == TRUE) {
    y <- gsub("\t", ",", x)
    writeLines(y, newFileDir)
    print("Table is separated by tabs and has been converted to a .csv")
  } else if (any(grepl(" ", x)) == TRUE) {
    y <- gsub(" ", ",", x)
    writeLines(y, newFileDir)
    print("Table is separated by spaces and has been converted to a .csv")
  } else {
    print("File does not contain a format that can be processed")
  }
}



convert_all_files_to_csv <- function(directory) {
  # List all files in the directory (modify the pattern if needed)
  files <- list.files(path = directory, pattern = ".*", full.names = TRUE)
  
  for (file in files) {
      csvConverter2(file)
  }
}

# example usage
# convert_all_files_to_csv("~/Desktop/BIOCOMP/Biocomputing-Final-Project/countryY/")
# convert_all_files_to_csv("~/Desktop/BIOCOMP/Biocomputing-Final-Project/countryX/")
# -------------------------------------------------------------------------------






# Function 2: combine_csv_files
# This function takes all csv files in the two countries' directories and converts
# them into one csv file called 'compiled_data.csv'
# -------------------------------------------------------------------------------
combine_csv_files <- function(directory1, directory2, directory3, remove_NAs = FALSE, warn_NAs = TRUE) {
  files1 <- list.files(path = directory1, pattern = "*.csv", full.names = TRUE)
  files2 <- list.files(path = directory2, pattern = "*.csv", full.names = TRUE)
  files <- c(files1, files2)  # Combine file lists from both directories
  
  combined_data <- data.frame()
  found_NAs <- FALSE
  
  for (file_name in files) {
    data <- read.csv(file = file_name)
    
    # Extract 'country' based on the directory
    country <- ifelse(grepl("countryX", file_name), "X", "Y")
    
    # Extract 'dayofYear' from the filename
    file_parts <- unlist(strsplit(basename(file_name), split = "_"))
    dayofYear <- gsub(".csv", "", file_parts[2])  # Assuming the format is 'screen_NNN.csv'
    
    # Add the new columns
    data$country <- country
    data$dayofYear <- as.numeric(dayofYear)
    
    # Check for NAs
    if (any(is.na(data))) {
      found_NAs <- TRUE
      if (remove_NAs) {
        data <- na.omit(data)
      }
    }
    
    combined_data <- rbind(combined_data, data)
  }
  
  # Warn about NAs if necessary
  if (found_NAs && warn_NAs && !remove_NAs) {
    message("Warning: Compiled data includes NAs.")
  }
  
  # Write the combined data to a new file in the first directory
  write.csv(combined_data, file.path(directory3, "compiled_data.csv"), row.names = FALSE)
  
  return(invisible(combined_data))
}

# Example usage
# combine_csv_files("~/Desktop/BIOCOMP/Biocomputing-Final-Project/countryX/", "~/Desktop/BIOCOMP/Biocomputing-Final-Project/countryY/", "~/Desktop/BIOCOMP/Biocomputing-Final-Project/compiled_data.csv", remove_NAs = TRUE, warn_NAs = FALSE)
# -------------------------------------------------------------------------------







# Function 3: summarize_dataset
# This function gives general data found in the data set in terms of percentage
# breakdowns and plots that describe the age distribution
# -------------------------------------------------------------------------------
library(ggplot2)

summarize_dataset <- function(file_path) {
  # Load the dataset
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Determine infection status based on markers
  data$infectedstatus <- apply(data[, grepl("marker", names(data))], 1, function(x) any(x == TRUE))
  
  # Total number of screens run
  total_screens <- nrow(data)
  
  # Percent of patients screened that were infected
  percent_infected <- sum(data$infectedstatus, na.rm = TRUE) / total_screens * 100
  
  # Gender distribution
  gender_distribution <- table(data$gender) / sum(table(data$gender), na.rm = TRUE) * 100
  percent_male <- gender_distribution['male']
  percent_female <- gender_distribution['female']
  
  # Age distribution plot
  age_distribution_plot <- ggplot(data, aes(x = age)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    ggtitle("Age Distribution of Patients") +
    xlab("Age") + ylab("Frequency")
  
  # Return a list containing the summary and the plot
  summary <- list(
    "Total Screens Run" = total_screens,
    "Percent Infected" = percent_infected,
    "Percent Male" = percent_male,
    "Percent Female" = percent_female,
    "Age Distribution Plot" = age_distribution_plot
  )
  
  return(summary)
}

# Example usage
# summary <- summarize_dataset("~/Desktop/BIOCOMP/Biocomputing-Final-Project/allData.csv")
# print(summary$`Total Screens Run`)
# print(summary$`Percent Infected`)
# print(summary$`Percent Male`)
# print(summary$`Percent Female`)
# print(summary$`Age Distribution Plot`)
# -------------------------------------------------------------------------------







# Function 4: questionAnswers
# This function provides plots that help to answer the major two questions
# -------------------------------------------------------------------------------
# loads necessary library
library(ggplot2)

questionAnswers <- function(file_path) {
  # load the dataset
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # determine infection status based on markers
  data$infectedstatus <- rowSums(data[, grepl("marker", names(data))]) > 0
  
  # preps data for the 'Infection Day Plot' with cumulative sum
  data <- data[order(data$country, data$dayofYear), ]
  data$cumulative_infections <- ave(data$infectedstatus, data$country, FUN = cumsum)
  
  # Infection Day Plot
  infection_day_plot <- ggplot(data, aes(x = dayofYear, y = cumulative_infections, group = country, color = country)) +
    geom_line() +
    ggtitle("Cumulative Infections by Day of Year") +
    xlab("Day of Year") + ylab("Cumulative Infections")
  
  # preps data for 'Marker Bar Graph'
  marker_columns <- grep("marker", names(data), value = TRUE)
  data[marker_columns] <- data[marker_columns] > 0
  markers_long <- do.call("rbind", lapply(marker_columns, function(marker) {
    data.frame(marker = marker, 
               infected = data[[marker]], 
               country = data$country)
  }))
  marker_totals_by_country <- aggregate(infected ~ marker + country, markers_long, sum)
  
  # Marker Bar Graph
  marker_bar_graph <- ggplot(marker_totals_by_country, aes(x = marker, y = infected, fill = country)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    ggtitle("Total Infections Per Marker by Country") +
    xlab("Marker") + ylab("Total Number of Infections")
  
  # returns the plots
  plots <- list(
    "Infection Day Plot" = infection_day_plot,
    "Marker Bar Graph" = marker_bar_graph
  )
  
  return(plots)
}

# Example usage
# plots <- questionAnswers("~/Desktop/BIOCOMP/Biocomputing-Final-Project/allData.csv")
# print(plots$`Infection Day Plot`)
# print(plots$`Marker Bar Graph`)
# -------------------------------------------------------------------------------