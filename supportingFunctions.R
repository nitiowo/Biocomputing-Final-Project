# supportingFunctions.R
# Create a a function that converts all files in "countryY" with space- or tab-delimited data (.txt) into comma-separated value files.
convertToCSV <- function(countryY) {
  # Specify the full path to the "countryY" folder in the Downloads directory
  directory_path <- file.path(Sys.getenv("HOME"), "Downloads", countryY)
  # Get a list of all .txt files in the specified directory
  files <- list.files(directory_path, pattern = "\\.txt$", full.names = TRUE)
  # Iterate over each file and convert to .csv
  for (file in files) {
    # Read the data from the text file using tab as the delimiter
    data <- read.table(file, header = TRUE, sep = " ")
    # Replace periods with underscores in column names
    colnames(data) <- gsub("\\.", "_", colnames(data))
    # Create a new file name by replacing the .txt extension with .csv
    new_file <- sub("\\.txt$", ".csv", file)
    # Write the data to a new .csv file
    write.csv(data, file = new_file, row.names = FALSE)
    # Remove the original .txt file
    file.remove(file)
    # Give confirmation to the user that each file has been converted to .csv
    cat("Conversion completed for:", file, "\n")
  }
  # Give confirmation to the user that all files in the "countryY" folder has been converted to .csv
  cat("All files converted to CSV in:", directory_path, "\n")
}
# How to use:
# Specify the folder name within the Downloads directory
folder_name <- "countryY"
# Call the function to convert files in the specified folder
convertToCSV(folder_name)

# Function to compile data from all .csv files in a directory into a single .csv file
compileData <- function(directoryX, directoryY, outputCSV, removeNA = TRUE, warnNA = TRUE) {
  # Get a list of all .csv files in the specified directories
  filesX <- list.files(directoryX, pattern = "screen_\\d+\\.csv$", full.names = TRUE)
  filesY <- list.files(directoryY, pattern = "screen_\\d+\\.csv$", full.names = TRUE)
  allData <- data.frame()
  # Helper function to process files from a directory
  processFiles <- function(files, country) {
    combinedData <- NULL
    for (file in files) {
      data <- read.csv(file, sep = "\t")
      # Check if warning for NA is required
      if (warnNA && any(is.na(data))) {
        warning("There are NA values in the data from file:", file)
      }
      # Include NAs based on user preference
      if (removeNA) {
        data <- na.omit(data)
      }
      # Use strsplit to extract dayofYear information from the file name
      file_info <- unlist(strsplit(basename(file), "_|\\.csv"))
      data$country <- country
      data$dayofYear <- as.numeric(file_info[2])
      # Combine data
      if (is.null(combinedData)) {
        combinedData <- data
      } else {
        combinedData <- rbind(combinedData, data)
      }
    }
    return(combinedData)
  }
  # Process files from countryX
  allDataX <- processFiles(filesX, "X")
  # Process files from countryY
  allDataY <- processFiles(filesY, "Y")
  # Combine data from both countries
  allData <- rbind(allDataX, allDataY)
  # Write the compiled data to a new CSV file with each label in its own column
  write.csv(allData, file = outputCSV, row.names = FALSE, quote = FALSE)
}
# How to use
# Specify the folders within the Downloads directory
folderX <- "countryX"
folderY <- "countryY"
# Specify the output CSV file
output_file <- "compiledData.csv"
# Call the function to compile data from both folders
compileData(file.path(Sys.getenv("HOME"), "Downloads", folderX), file.path(Sys.getenv("HOME"), "Downloads", folderY), output_file, removeNA = FALSE, warnNA = TRUE)

# Create a function that summarizes the compiled data set
summarize_all_data <- function() {
  # Set the path to the 'allData.csv' file in the Downloads directory
  data_path <- file.path("~", "Downloads", "allData.csv")
  # Read the compiled data from the CSV file
  all_data <- read.csv(data_path, header = TRUE, stringsAsFactors = FALSE)
  # Number of screens run
  num_screens <- nrow(all_data)
  # Percentage of patients screened that were infected
  percent_infected <- mean(rowSums(all_data[, c("marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10")] == 1) > 0) * 100
  # Percentage of patients identifying as male and female
  percent_male <- mean(all_data$gender == "male") * 100
  percent_female <- mean(all_data$gender == "female") * 100
  # Age distribution of patients (table)
  age_distribution_table <- table(all_data$age)
  # Print summary information
  cat("Number of Screens Run:", num_screens, "\n")
  cat("Percentage of Patients Screened that were Infected:", percent_infected, "%\n")
  cat("Percentage of Patients Identifying as Male:", percent_male, "%\n")
  cat("Percentage of Patients Identifying as Female:", percent_female, "%\n")
  # Print age distribution table
  cat("Age Distribution Table:\n")
  print(age_distribution_table)
}
# How to use
summarize_all_data()
