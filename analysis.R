
source("/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/supportingFunctions.R")

library(ggplot2)
library(cowplot)
#set pathway for each country folder
directory_X <- "/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/countryX"
directory_Y <- "/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/countryY"

#go to directory of country Y, use the "convert_Y_to_csv" function to convert all data from countryY folder to csv file
setwd("/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/countryY")
convert_Y_to_csv(directory_Y)

#go to directory of country X, use the "compile_data" function to compile all csv file into one
setwd("/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/countryX")
compile_data(directory_X)
countryX <- read.csv("X.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#go to directory of country Y, use the "compile_data" function to compile all csv file into one
setwd("/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project/countryY")
compile_data(directory_Y)
countryY <- read.csv("Y.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#Write two country compiled data into one
all_data <- data.frame()
all_data <- rbind(countryX, countryY)
setwd("/Users/vivianyang/Documents/GitHub/Biocomputing-Final-Project")
write.csv(all_data, "all_data.csv", row.names = FALSE)

#
