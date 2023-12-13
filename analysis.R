#Load the functions defined in supportingFunctions.R
source("supportingFunctions.R")

#Define the directory paths (this is how they are defined on my computer)
directory_path_X <- "C:/Users/maryc/OneDrive/Intro to Biocomputing/Country X"
directory_path_Y <- "C:/Users/maryc/OneDrive/Intro to Biocomputing/Country Y"

#Convert all the .txt files in Country Y folder to .csv files
convertToCSV(directory_path_Y)

#Compile all data into a single comma-separated value (.csv) file
compileData(directory_path_X)
compileData(directory_path_Y)

#Define the path to the newly created compiled .csv files
path_to_compiled_fileX <- "C:/Users/maryc/OneDrive/Intro to Biocomputing/Country X/compiled_data_Country X.csv"
path_to_compiled_fileY <- "C:/Users/maryc/OneDrive/Intro to Biocomputing/Country Y/compiled_data_Country Y.csv"

#Process the data included in the entire data set in order to answer the two questions above and provide graphical evidence for your answers
summarizeData(path_to_compiled_fileX)
summarizeData(path_to_compiled_fileY)

#Guiding Questions:

#Question 1:
#I believe that the disease outbreak likely began in Country X because the percentage of screened patients infected for Country Y is equal to 0% until day 139. 
#This indicates that Country Y did not have any positive cases of the disease until that day, while Country X had cases present on Day 120 (the first day of screenings). 

#Question 2:
#If Country Y develops a vaccine for the disease I don't believe that it will work for Country X because patients who are infected with the disease in Country X for the most part have corresponding presence of genetic markers 1-5. 
#However, patients infected in Country Y for the most part have the presence of genetic markers 6-10. This indicates that at some point the disease mutated. 
#If Country Y develops a vaccine based on the presence of these markers it will likely not be effective for citizens in Country X because the genetic markers present in the vaccine will not correspond with the genetic markers present in their strain of the disease.



