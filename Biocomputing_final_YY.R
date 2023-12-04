library(ggplot2)
library(cowplot)
#set pathway
setwd("/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/countryX")



  csv_file <- list.files(pattern = "\\.csv$")
  all_csv <- list()
  # Loop through the files and read each one
  for (file in csv_file) {
    data <- read.csv(file)
    all_csv[[file]] <- data
  }
