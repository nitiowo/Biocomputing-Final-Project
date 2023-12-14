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
  output_file <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/all_Data.csv"  # Specify the path and file name
  write.csv(empty_data, file = output_file, row.names = FALSE)
  
}
initialize_csv_file()

## make one giant csv
# Initialize csv you want to compile countryY and countryX to then pass it into the function as a parameter


append_to_csv <- function(input_directory, output_file, initial_csv, na_handling = "warn") {
  # Read the initial CSV file
  compiled_data <- read.csv(initial_csv, header = TRUE)
  
  # Extract the country from the initial CSV file's directory
  country <- basename(dirname(initial_csv))
  
  files <- list.files(input_directory, pattern = "\\.csv$", full.names = TRUE)
  
  for (file in files) {
    data <- read.csv(file, header = TRUE, skip = 1)  # Skip the first row
    
    # Extract dayofYear from the file name using strsplit()
    filename_parts <- strsplit(basename(file), "_")[[1]]  
    dayofYear <- filename_parts[length(filename_parts)]  
    
    # Add 'country' and 'dayofYear' columns to the data
    data$country <- country
    data$dayofYear <- dayofYear
    
    # Append data to the compiled_data data frame
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

initial_csv <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/all_Data.csv"
output_file <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/all_Data.csv"
input_directory <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/countryY/"


append_to_csv(input_directory, output_file, initial_csv)



## summarize compiled data