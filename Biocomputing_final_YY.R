library(ggplot2)
library(cowplot)
#set pathway
directory_X <- "/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/countryX"
directory_Y <- "/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/countryY"

# Country X's files are already in comma-delimited format;
# Country Y need to be changed from tab-delimited to comma-delimited
convert_Y_to_csv <- function(directory) {
  # Get a list of all files in the directory
  all_file <- list.files(directory)
  # Loop through all the files
  for(file in all_file) {
    # Use the original file name
    file_name <- strsplit(file,".txt")
    # add ".csv" to the end of file_name
    csv_file_name <- paste0(file_name, ".csv")
    # Read the tab-delimited file
    data <- read.table(file, header = TRUE, sep = " ", stringsAsFactors = FALSE)
    # Write into a csv file
    write.csv(data, csv_file_name, row.names = FALSE)
  }}
setwd("/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/countryY")
convert_Y_to_csv(directory_Y)



compile_data <- function(directory){
  # Get a list of all files in the directory
  all_file <- list.files(directory, pattern = "\\.csv$")
  # Make a new data frame to save all the data
  compiled_data <- data.frame()
  split_country = strsplit(directory,"country")
  country <- sapply(split_country, function(x) x[2])
  # add ".csv" to the end of file_name
  output <- paste0(country, ".csv")
  # Loop through all the data file
  for(file in all_file) {
    data <- read.csv(file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    split_csv <- strsplit(file,".csv")
    splited_csv <- paste(split_csv)
    split_year <- strsplit(splited_csv, "_")
    year <- split_year[[1]][2]
    # Add a new column 'dayofYear'
    data$dayofYear <- c(rep(year, nrow(data)))
    # Add a new column 'Country'
    compiled_data <- rbind(compiled_data, data)
  }
  # Write into a csv file
  write.csv(compiled_data, output, row.names = FALSE)
}
setwd("/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/countryX")
compile_data(directory_X)
setwd("/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/countryY")
compile_data(directory_Y)

compiled_country <- data.frame()
countryX <- read.csv("X.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
countryY <- read.csv("Y.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
compiled_data <- rbind(countryX, countryY)
