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
    data <- read.table(file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    # Write into a csv file
    write.csv(data, csv_file_name)
  }}
convert_Y_to_csv(directory_Y)
