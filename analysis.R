# analysis.R

#install necessary libraries in case one doesn't have these
install.packages("ggplot2")

# load functions in from supportingFunctions.R
source("supportingFunctions.R")

# load packages that will be used
library(ggplot2)

# convert all files in the directories to csv files that can be worked with
# function sourced from supportingFunctions.R
convert_all_files_to_csv("~/Desktop/BIOCOMP/Biocomputing-Final-Project/countryY/")
convert_all_files_to_csv("~/Desktop/BIOCOMP/Biocomputing-Final-Project/countryX/")


# merge all csv files into one main compiled data file
# function sourced from supportingFunctions.R
combine_csv_files("~/Desktop/BIOCOMP/Biocomputing-Final-Project/countryX/", "~/Desktop/BIOCOMP/Biocomputing-Final-Project/countryY/", "~/Desktop/BIOCOMP/Biocomputing-Final-Project/",remove_NAs = TRUE, warn_NAs = FALSE)



# Provides a summary of the data as it relates to the breakdown of percentage
# infected, percentage of male and female infected, and an age distribution plot
summary <- summarize_dataset("~/Desktop/BIOCOMP/Biocomputing-Final-Project/allData.csv")
print(summary$`Total Screens Run`)
print(summary$`Percent Infected`)
print(summary$`Percent Male`)
print(summary$`Percent Female`)
print(summary$`Age Distribution Plot`)



# provides plots that help answer the major two questions
plots <- questionAnswers("~/Desktop/BIOCOMP/Biocomputing-Final-Project/allData.csv")
print(plots$`Infection Day Plot`)
print(plots$`Marker Bar Graph`)

# looking at the 'Infection Day Plot', one can observe that Country X begins to 
# see a rise in infection long before Country Y does. This points to Country X
# being the first country to see infection

# looking at the 'Marker Bar Graph', one can see that the symptoms or markers 1-5
# are predominant in country X, while markers 6-10 are predominant in country Y.
# This suggests that a vaccine for this infection may not be universal, and may
# have to be adapted to treat the different varients that these countries are 
# seeing

