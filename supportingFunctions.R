#function to convert files in a fdirectory to csv files
convert_files <- function(directory_path) {
  # List all files in the directory
  files <- list.files(path = directory_path, pattern = "\\.txt$", full.names = TRUE)
  
  # Iterate through each file
  for (file_path in files) {
    # Read the first line to determine the delimiter
    first_line <- readLines(file_path, n = 1)
    delimiter <- ifelse(grepl("\t", first_line), "\t", " ")
    
    # Read the file using read.table with the determined delimiter
    data <- read.table(file_path, sep = delimiter, header = TRUE, stringsAsFactors = FALSE)
    
    # Create a new CSV file with the same name
    csv_file_path <- sub("\\.txt$", ".csv", file_path)
    write.csv(data, file = csv_file_path, row.names = FALSE)
    
    cat(sprintf("Converted %s to %s\n", file_path, csv_file_path))
  }
}

#function to combine country csv files
combine_csv<- function(directories) {
  # Make an empty data frame to store combined data
  compiled_csv <- data.frame()
  
  # Combine all CSV files from the specified directories
  for (directory in directories) {
    csv_files <- list.files(path = directory, pattern = "\\.csv$", full.names = TRUE)
    
    # For loop to iterate through each .csv file
    for (file in csv_files) {
      # Reading in file data
      data <- read.csv(file, header = TRUE)
      
      # Use strsplit function to get country name and date
      data$country <- strsplit(directory, "country")[[1]][2]
      data$date <- strsplit(strsplit(file, "screen_")[[1]][2], "\\.")[[1]][1]
      
      # Compile data into one file
      compiled_csv <- rbind(compiled_csv, data)
    }
  }
  
  # Prompt user for options
  cat("Choose an option for handling NA values:\n
      1. Remove rows with NA values\n
      2. Include NAs in the compiled data but be warned\n
      3. Include NAs in the compiled data without a warning\n")
  
  # Read user's choice
  user_choice <- as.numeric(readline(prompt = "Enter your choice (1, 2, or 3): "))
  
  # Validate user's choice
  if (user_choice %in% c(1, 2, 3)) {
    remove_na <- user_choice == 1
    warn_na <- user_choice == 2
  } else {
    stop("Invalid choice. Please enter 1, 2, or 3.")
  }
  
  # If-else statement that executes user's choice
  if (remove_na) {
    compiled_csv <- na.omit(compiled_csv)
  } else if (warn_na) {
    print("There are NA values in this dataset.")
  } else {
    # Include other options if needed
  }
  
  # Write the compiled data to a CSV file
  write.csv(compiled_csv, "compiled_file.csv", row.names = FALSE)
  cat("Compilation complete. Data written to compiled_file.csv\n")
}

#combine_csv(c("countryX", "countryY"))

#function to analyze data
analyze_data <- function(data_file) {
  all_data <- read.csv(data_file, header=TRUE)
  
  # find the number of screenings run
  num_screens <- nrow(all_data)
  
  # initialize list of length(num_screens) to later store infection status of patient
  all_data$infected <- rep(0, num_screens)
  
  # iterate over each row and column of data to determine if marker is present for each screen
  for (i in 1:num_screens) {
    for (j in 3:12) {
      if(all_data[i, j] == 1) {
        # store 1 in list to indicate infection in screening
        all_data$infected[i] <- 1
        break
      }
    }
  }
  
  # find percentage of screens that indicate infection
  percent_infected <- (sum(all_data$infected) / num_screens) * 100
  
  # find percentage of patients by gender identity
  percent_male <- (sum(all_data$gender == "male") / num_screens) * 100
  percent_female <- (sum(all_data$gender == "female") / num_screens) * 100
  
  # create an age distribution plot
  age_dist_hist <- ggplot(all_data, aes(x = age)) +
    geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
    labs(title = "Age Distribution",
         x = "Age",
         y = "Frequency")
  
  # print information 
  cat("Summary: \n")
  cat("Number of screens run:", num_screens, "\n")
  cat("Percentage of patients screened that were infected:", percent_infected, "%\n")
  cat("Percentage of patients that identified as male:", percent_male, "%\n")
  cat("Percentage of patients that identified as female:", percent_female, "%\n")
  print(age_dist_hist)
}

analyze_data('compiled_file.csv')


