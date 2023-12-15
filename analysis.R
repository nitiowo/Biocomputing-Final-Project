# analysis.R
"
Members: Matthew Eleazar, Aaliyah Percell, Carlos Yovany Saravia

"

source("supportingFunctions.R")

working_dir = 'C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project' # can be changed by the user
# working_dir = readline() # user inputs working directory
setwd(working_dir)

# User can just change the paths to these directores specific to their machines
initial_csv <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/init_Data.csv"
output_file <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/all_Data.csv"
append_file <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/all_Data.csv" 
input_directoryX <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/countryX/"
input_directoryY <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/countryY/"

country_X_csv <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/country_X_data.csv"
country_Y_csv <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/country_Y_data.csv"

initialize_csv_file()



append_to_csv(input_directoryX, country_X_csv, initial_csv) # Make CSV File with only country X data
append_to_csv(input_directoryY, country_Y_csv, initial_csv) # Make CSV File with only country Y data


# Make visuals for country x and y

country_X_df <- read.csv("C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/country_X_data.csv")
country_Y_df <- read.csv("C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/country_Y_data.csv")

#Using Summary_Func we provided the .csv file for countries X and Y. This allows for the gathering of the information of
#Percent of Genders, Percent of Age groups, Percent of Markers. Through this analysis, it is clear
summary_func(country_X_df) 

summary_func(country_Y_df)

"
# 1. In which country (X or Y) did the disease outbreak likely begin?
The diseases most likely began in country X because of the larger number of cases in preteens, teens, and adults 
compared to country Y.

# 2. If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
If country Y develops a vaccine for the diease it will most likely will not work for Country X. 
This is because of the variation of markers. Country Y's population had predominantly markers 4 and 6 while 
Country X predominantly had markers 1 and 2. If a vaccine was to be made for country Y, it would not know which markers 
to effectively target in country X.

"




