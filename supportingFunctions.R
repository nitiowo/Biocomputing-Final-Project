### supportingFunctions.R

# Notes: assumes that directory is pwd

## tab delim to csv
# Notes: Country X already in csv, just need Country Y


working_dir = 'C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project' # can be changed by the user
# working_dir = readline() # user inputs working directory
setwd(working_dir)

tab_to_csv <- function(dir = working_dir){
  country_Y <- file.path(working_dir, 'countryY')
  setwd(country_Y)
  
  country_y_dir <- list.files()  # List files in the 'countryY' directory
  
  for (file in country_y_dir){
    # Read the original .txt file
    temp <- read.table(file, header = TRUE)  # Assuming the first row contains headers
    
    # Extracting just the data (excluding row numbers and header)
    data <- temp[-c(1), ]
    
    # Create a copy of the original .txt file
    file_copy <- paste0("copy_", sub("\\.txt$", ".csv", file))  # Naming the copied file
    
    # Write the data to a .csv file without row numbers
    write.csv(data, file_copy, row.names = FALSE)
  }
}


tab_to_csv(working_dir)
setwd(working_dir)


## Extra Function: Intialize csv file to be used in compilation, just in case user doesn't have one yet, called "all_Data".
initialize_csv_file <- function(x){
  # Define headers
  headers <- c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10", "country","dayofYear")
  
  # Create an empty data frame with those columns
  empty_data <- data.frame(matrix(ncol = length(headers), nrow = 0))
  colnames(empty_data) <- headers
  
  # Write the empty data frame to a CSV file
  output_file <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/init_Data.csv"  # Specify the path and file name
  write.csv(empty_data, file = output_file, row.names = FALSE)
  
}
initialize_csv_file()

## make one giant csv
# Initialize csv you want to compile countryY and countryX to then pass it into the function as a parameter

append_to_csv <- function(input_directory, output_file, initial_csv, na_handling = "warn") {
  compiled_data <- read.csv(initial_csv, header = TRUE)
  
  directory_parts <- strsplit(basename(input_directory), "")[[1]]  # Split the directory name into individual characters
  
  # Extract 'Y' using strsplit()
  country <- directory_parts[length(directory_parts)]  # Extracts the last character
  
  files <- list.files(input_directory, pattern = "\\.csv$", full.names = TRUE)
  
  for (file in files) {
    data <- read.csv(file, header = TRUE)
    
    # Extract the number from the filename
    filename_number <- as.numeric(gsub("\\D", "", basename(file))) # easier to replace all occurrences with gsub
    
    # Add 'country' and 'dayofYear' columns to the data
    data$country <- country
    data$dayofYear <- filename_number
    
    compiled_data <- rbind(compiled_data, data)
  }
  
  
  if (na_handling == "remove") {
    compiled_data <- na.omit(compiled_data)
  } else if (na_handling == "warn") {
    if (anyNA(compiled_data)) {
      warning("There are NA values present in the compiled data.")
    }
  }
  
  write.csv(compiled_data, file = output_file, row.names = FALSE)
}

# Note: I will assume that the user knows what the paths to their files are so they can just change these variables when
# using the function in another file

initial_csv <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/init_Data.csv"
output_file <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/all_Data.csv"
append_file <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/all_Data.csv" # needed to append country Y after appending country X to init_data
input_directoryX <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/countryX/"
input_directoryY <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/countryY/"



append_to_csv(input_directoryX, output_file, initial_csv) # first run with just directory X
append_to_csv(input_directoryY, output_file, append_file) # second run with both directory X and Y


## summarize compiled data
