#### convert txt to csv ----

## note: example files are called file.txt and file.csv
#### things i'm not sure about
  # can i have separate lines for space vs tab delimited
  # do i need to save the files with different names/how to name files

txt_to_csv <- function(directory) {
  for (files in list.files(path = directory, pattern = "*.txt")) {
    file <- read.table(filename, header = TRUE, sep = " ")
    csvout <- write.csv(file, file = "file.csv", row.names = FALSE)
  }
  }

# doesn't work because the function wants an input of filename but the for loop wants
# an input of txt files. also, saving read.table as file isn't working

#### compile data from all csv files into single file ----

# add day of year column
filename <- "countryX/screen_120.csv"
screen_120.csv <- read.csv(filename)
dayofyear <- strsplit(filename, "[^a-zA-Z0-9]")[[1]][3]
screen_120.csv$dayofYear = dayofyear

# add country column
country_full <- strsplit(filename, "[^a-zA-Z0-9]")[[1]][1]
country <- strsplit(country, "[a-z]")[[1]][8]
screen_120.csv$country = country

# make a for loop to add columns to each csv file
filelist <- list.files()
length <- length(filelist)
for (i in 1:length) {
  print(i)
  # create the name of each file as the file name according to the iteration i
  name <- as.character(filelist[i])
  # use read.csv on the file for iteration i and name it the according name
  assign(name, read.csv(as.character(filelist[i])))
  ## add day of year column
  dayofYear <- strsplit(as.character(filelist[i]), "[^a-zA-Z0-9]")[[1]][2]
  ######### this doesn't work
  # use some sort of function that creates a column?
  # name$dayofYear = dayofyear
  ## add country column
  country <- "X"
}


# compile <- function()

#### summarize compiled data ----
##%% issues: how to return multiple variables in the function

summary <- function(datafile) {
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
  return(percentmale)
}

