#### convert txt to csv ----

#### things i'm not sure about
  # can i have separate lines for space vs tab delimited

# ask the user for the directory and the delimiter
txt_to_csv <- function(directory, delim) {
  # if space delimited, run the code with space as the separator
  if (delim == "space") {
    # have it run the code for all txt files in the directory
    for (file in list.files(path = directory, pattern = "*.txt")) {
      # define a path variable with the path including the directory for each file
      path <- paste(directory, file, sep = "/")
      # read in each file as a dataset with space as the delimiter
      dataset <- read.table(path, header = TRUE, sep = " ")
      # define a variable to name the file with .csv at the end
      name <- paste(file, ".csv", sep = "")
      # define a new path variable that includes the directory and the name
      path_new <- paste(directory, name, sep = "/")
      # write a csv file from the dataset with the name of the new path
      write.csv(dataset, file = path_new, row.names = FALSE)
    }
  }
  # if not space delimited (so tab delimited), run the code with tab as the separator
  else{
    # have it run the code for all txt files in the directory
    for (file in list.files(path = directory, pattern = "*.txt")) {
      # define a path variable with the path including the directory for each file
      path <- paste(directory, file, sep = "/")
      # read in each file as a dataset with tab as the delimiter
      dataset <- read.table(path, header = TRUE, sep = "/t")
      # define a variable to name the file with .csv at the end
      name <- paste(file, ".csv", sep = "")
      # define a new path variable that includes the directory and the name
      path_new <- paste(directory, name, sep = "/")
      # write a csv file from the dataset with the name of the new path
      write.csv(dataset, file = path_new, row.names = FALSE)
    }
  }
}


#### compile data from all csv files into single file ----

# create function that asks user for the directory
compile <- function(directory) {
  # create an empty dataframe for the compiled data
  compileddata <- data.frame()
  # list all the csv files in the directory, do the following for each file
  for (file in list.files(path = directory, pattern = "*.csv")) {
    # create a path from the working directory with the csv directory and file name
    path <- paste(directory, file, sep = "/")
    # read in the file from its file path and save as a dataframe called dataset
    dataset <- read.csv(path)
    # isolate the day of the year from the file name
    dayofyear <- strsplit(file, "[^a-zA-Z0-9]")[[1]][2]
    # add a column to the data set with the day of the year
    dataset$dayofYear = dayofyear
    # isolate the country from the path
    country <- strsplit(path, "[^A-Z]")[[1]][8]
    # add a column to the data set with the country
    dataset$country = country
    # bind the dataset to the compileddata data frame
    compileddata <- rbind(dataset, compileddata)
  }
  # after all files are compiled, write a csv of the compiled dataset
  write.csv(compileddata, file = "compileddata.csv", row.names = FALSE)
}


#### summarize compiled data ----

# create a function that asks for the datafile
summary <- function(datafile) {
  # load library for ggplot
  # library(tidyverse)
  # read in data file
  datafile <- read.csv(datafile)
  ## number of screens run
  # create variable for the number of screens run
  screensrun <- nrow(datafile)
  ## percent positive
  # select only the columns that have at least one marker
  positive <- datafile[apply(datafile[, 3:12], 1, sum) > 0, ]
  # count the rows in the positive dataset
  posrows <- nrow(positive)
  # calculate the percent rows that are positive divided by the total screens
  percentpos <- posrows / screensrun
  ## percent female
  # select only rows with female gender, save as dataset
  female <- datafile[datafile$gender == "female", ]
  # count the number of rows that are female
  femalerows <- nrow(female)
  # calculate percent female by dividing female rows by total screens
  percentfemale <- femalerows / screensrun
  ## percent male
  # calculate percent male by calculating the number of nonfemale rows (male rows)
  # and divide by total screens
  percentmale <- (screensrun - femalerows) / screensrun
  ## age distribution of patients
  # create histogram with ages on the x axis and counts per age on y axis
  ggplot(datafile, aes(x = age)) +
    geom_histogram()
}
