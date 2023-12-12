#### convert txt to csv ----

#### things i'm not sure about
  # can i have separate lines for space vs tab delimited

txt_to_csv <- function(directory) {
  for (file in list.files(path = directory, pattern = "*.txt")) {
    path <- paste(directory, file, sep = "/")
    dataset <- read.table(path, header = TRUE, sep = " ")
    name <- paste(file, ".csv", sep = "")
    path_new <- paste(directory, name, sep = "/")
    write.csv(dataset, file = path_new, row.names = FALSE)
  }
  }


#### compile data from all csv files into single file ----

compile <- function(directory) {
  compileddata <- data.frame()
  for (file in list.files(path = directory, pattern = "*.csv")) {
    path <- paste(directory, file, sep = "/")
    dataset <- read.csv(path)
    dayofyear <- strsplit(file, "[^a-zA-Z0-9]")[[1]][2]
    dataset$dayofYear = dayofyear
    country <- strsplit(path, "[^A-Z]")[[1]][8]
    dataset$country = country
    compileddata <- rbind(dataset, compileddata)
    write.csv(compileddata, file = "compileddata.csv", row.names = FALSE)
  }
}


#### summarize compiled data ----
##%% issues: how to return multiple variables in the function

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
  posrows <- nrow(positive)
  percentpos <- posrows / screensrun
  ## percent female
  female <- datafile[datafile$gender == "female", ]
  femalerows <- nrow(female)
  percentfemale <- femalerows / screensrun
  ## percent male
  percentmale <- (screensrun - femalerows) / screensrun
  ## age distribution of patients
  # create histogram with ages on the x axis and counts per age on y axis
  ggplot(datafile, aes(x = age)) +
    geom_histogram()
}
