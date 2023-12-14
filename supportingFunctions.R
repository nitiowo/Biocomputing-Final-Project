### supportingFunctions.R

# Notes: assumes that directory is pwd

## tab delim to csv
# Notes: Country X already in csv, just need Country Y


working_dir = 'C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project' # can be changed by the user
# working_dir = readline() # user inputs working directory
setwd(working_dir)

tab_to_csv <- function(dir = working_dir){ # Non destructively convert .txt to .csv by making copies.
  country_Y <- file.path(working_dir, 'countryY')
  setwd(country_Y)
  
  country_y_dir <- list.files()  # List files in the 'countryY' directory
  
  
  for (file in country_y_dir){
    # Read the original .txt file
    temp <- read.table(file)
    
    # Create a copy of the original .txt file
    file_copy <- paste0("copy_", file)
    file.copy(file, file_copy)
    
    # Convert the copied data to a .csv file
    file_csv <- sub("\\.txt$", ".csv", file)
    write.csv(temp, file_csv)
  }
}

tab_to_csv(working_dir)
setwd(working_dir)


## make one giant csv


## summarize compiled data