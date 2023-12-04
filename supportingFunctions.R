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

#### summarize compiled data ----