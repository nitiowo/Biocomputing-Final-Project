#### convert txt to csv ----

## note: example files are called file.txt and file.csv
#### things i'm not sure about
  # can i have separate lines for space vs tab delimited
  # do i need to save the files with different names/how to name files

# read in text file that is separated by spaces
txtfile <- read.table("file.txt", sep = " ", header = TRUE)
write.csv(txtfile, file = "file.csv", row.names = FALSE)

# set working directory
setwd("countryX")
for (file in list.files()) {
  filename <- file
  assign(filename, read.csv(file))
  # write.table(file, sep = " ", col.names = TRUE, file = "file.txt")
}

#### compile data from all csv files into single file ----

#### summarize compiled data ----