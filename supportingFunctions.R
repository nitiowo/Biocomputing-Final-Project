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
    # Add a new column 'country'
    data$country <- c(rep(country, nrow(data)))
    # Add a new column 'dayofYear'
    data$dayofYear <- c(rep(year, nrow(data)))
    
    compiled_data <- rbind(compiled_data, data)
  }
  # Write into a csv file
  write.csv(compiled_data, output, row.names = FALSE)
}

#data analysis
analysis <- function(x){
  #number of screen runs
  table <- read.csv(x, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  number <- dim(table)[1]
  text <- "The total number of screening is"
  print(paste(text, number))

  #percent of patients screened that were infected and put it to a dataframe
  total_infection <- 0
  for (i in 1:nrow(table)) {
    for (j in 3:12){
      if (!is.na(table[i,j]) && table[i,j] >= 1){
        total_infection <- total_infection+1
        break
      }
      }
  }
  text_2 <- "percentage of infected patients is "
  calc <- total_infection/number *100
  print(paste(text_2, calc))
  
}

