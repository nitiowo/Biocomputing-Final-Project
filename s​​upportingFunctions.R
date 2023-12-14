# FUNCTION ONE: converting files into csv

ConvertToCSV <- function(folder_path) {
  
  # Loop through each file in the directory specified in folder_path
  for (i in 1:length(dir(folder_path, pattern = "\\.txt$", full.names = TRUE))) {
    
    # Read the text file with space or tab delimiter
    data <- read.table(dir(folder_path, pattern = "\\.txt$", full.names = TRUE)[i], header = TRUE)
    
    # Write the data to a CSV file with comma delimiter
    write.table(data, sub("\\.txt$", ".csv", dir(folder_path, pattern = "\\.txt$", full.names = TRUE)[i]), sep = ",", row.names = FALSE, col.names = TRUE)
  }
}


# FUNCTION TWO: compiling data

CompileAndWriteCSV <- function(folder_path, output_file, NA_option = "keep") {
  #NAs will be included without warning by default unless otherwise specified by the user, with either NA_option = "warn" or NA_option = "omit"
  
  # List all CSV files in the specified folder and subdirectories
  file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  
  # Initialize an empty data frame to store compiled data
  compiled_data <- data.frame()
  
  # Loop through each CSV file and add data to a dataframe
  for (file_path in file_list) {
    # Read the CSV file
    data <- read.csv(file_path, header = TRUE)
    
    # Extract country and dayofYear from the file path
    file_info <- unlist(strsplit(gsub(".csv", "", basename(file_path)), "_"))
    country <- ifelse(grepl("countryX", file_path), "X", "Y")
    data$country <- as.character(country)
    data$dayofYear <- as.numeric(file_info[2])
    
    # Append data to the compiled_data data frame
    compiled_data <- rbind(compiled_data, data)
  }
  
  # Remove rows with NA's based on user preference
  if (NA_option == "omit") {
    compiled_data <- na.omit(compiled_data)
  } else if (NA_option == "warn") {
    if (any(is.na(compiled_data))) {
      print("There are NA values in the compiled data.")
    } else {
      print("There are no NA values in the compiled data.")
    }
    
    # Write the compiled data to a single CSV file
    write.csv(compiled_data, file = output_file, row.names = FALSE)
  }}


# FUNCTION THREE: summarizing data

SummarizeCompiledData <- function(compiled_data_file) {
  # Read the compiled data from the CSV file
  compiled_data <- read.csv(compiled_data_file, header = TRUE)
  
  # Number of screens run
  num_screens <- nrow(compiled_data)
  
  # Percentage of patients screened that were infected
  percent_infected <- mean(compiled_data$marker01 == 1)
  
  # Percentage of patients identifying as male and female
  percent_male <- mean(compiled_data$gender == "male", na.rm = TRUE)
  percent_female <- mean(compiled_data$gender == "female", na.rm = TRUE)
  
  # Age distribution of patients (as a table)
  age_distribution <- table(compiled_data$age)
  
  # Print the summary statistics
  cat("Number of screens run:", num_screens, "\n")
  cat("Percentage of patients screened that were infected:", percent_infected * 100, "%\n")
  cat("Percentage of patients identifying as male:", percent_male * 100, "%\n")
  cat("Percentage of patients identifying as female:", percent_female * 100, "%\n")
  cat("Age distribution of patients:\n")
  print(age_distribution)
  
  # Plot the age distribution (optional)
  barplot(age_distribution, main = "Age Distribution of Patients", xlab = "Age", ylab = "Count")
}
