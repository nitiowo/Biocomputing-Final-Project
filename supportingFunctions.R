### supportingFunctions.R

# Notes: assumes that directory is pwd

## tab delim to csv
# Notes: Country X already in csv, just need Country Y


working_dir = 'C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project' # can be changed by the user
# working_dir = readline() # user inputs working directory
setwd(working_dir)

tab_to_csv <- function(dir = working_dir){
  country_Y <- file.path(working_dir, 'countryY')
  setwd(country_Y)
  
  country_y_dir <- list.files()  # List files in the 'countryY' directory
  
  for (file in country_y_dir){
    # Read the original .txt file
    temp <- read.table(file, header = TRUE)  # Assuming the first row contains headers
    
    # Extracting just the data (excluding row numbers and header)
    data <- temp[-c(1), ]
    
    # Create a copy of the original .txt file
    file_copy <- paste0("copy_", sub("\\.txt$", ".csv", file))  # Naming the copied file
    
    # Write the data to a .csv file without row numbers
    write.csv(data, file_copy, row.names = FALSE)
  }
}


tab_to_csv(working_dir)
setwd(working_dir)


## Extra Function: Intialize csv file to be used in compilation, just in case user doesn't have one yet, called "all_Data".
initialize_csv_file <- function(x){
  # Define headers
  headers <- c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10", "country","dayofYear")
  
  # Create an empty data frame with those columns
  empty_data <- data.frame(matrix(ncol = length(headers), nrow = 0))
  colnames(empty_data) <- headers
  
  # Write the empty data frame to a CSV file
  output_file <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/init_Data.csv"  # Specify the path and file name
  write.csv(empty_data, file = output_file, row.names = FALSE)
  
}
initialize_csv_file()

## make one giant csv
# Initialize csv you want to compile countryY and countryX to then pass it into the function as a parameter

append_to_csv <- function(input_directory, output_file, initial_csv, na_handling = "warn") {
  compiled_data <- read.csv(initial_csv, header = TRUE)
  
  directory_parts <- strsplit(basename(input_directory), "")[[1]]  # Split the directory name into individual characters
  
  # Extract 'Y' using strsplit()
  country <- directory_parts[length(directory_parts)]  # Extracts the last character
  
  files <- list.files(input_directory, pattern = "\\.csv$", full.names = TRUE)
  
  for (file in files) {
    data <- read.csv(file, header = TRUE)
    
    # Extract the number from the filename
    filename_number <- as.numeric(gsub("\\D", "", basename(file))) # easier to replace all occurrences with gsub
    
    # Add 'country' and 'dayofYear' columns to the data
    data$country <- country
    data$dayofYear <- filename_number
    
    compiled_data <- rbind(compiled_data, data)
  }
  
  
  if (na_handling == "remove") {
    compiled_data <- na.omit(compiled_data)
  } else if (na_handling == "warn") {
    if (anyNA(compiled_data)) {
      warning("There are NA values present in the compiled data.")
    }
  }
  
  write.csv(compiled_data, file = output_file, row.names = FALSE)
}

# Note: I will assume that the user knows what the paths to their files are so they can just change these variables when
# using the function in another file

initial_csv <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/init_Data.csv"
output_file <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/all_Data.csv"
append_file <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/all_Data.csv" # needed to append country Y after appending country X to init_data
input_directoryX <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/countryX/"
input_directoryY <- "C:/Users/matte/Desktop/junior_fall/Biocomputing/Biocomputing-Final-Project/countryY/"



append_to_csv(input_directoryX, output_file, initial_csv) # first run with just directory X
append_to_csv(input_directoryY, output_file, append_file) # second run with both directory X and Y


## summarize compiled data
setwd(working_dir)

#Number of Screens Ran
#<-function(x){
all.data<-read.csv("allData.csv", sep= ",")
screenran<-nrow(all.data) #

#Percent of patients screened that were infected
infectedSum = 0 
marker1 = 0;
marker2 = 0;
marker3 = 0;
marker4 = 0;
marker5 = 0;
marker6 = 0;
marker7 = 0;
marker8 = 0;
marker9 = 0;
marker10 = 0;
totalSum = 0

for(i in 1:screenran){
  if(all.data$marker01[i] == 1){
    infectedSum = infectedSum + 1
    marker1 = marker1 +1
    totalSum = totalSum + 1
  }else if(all.data$marker02[i] ==1){
    infectedSum = infectedSum + 1
    marker2 = marker2 +1
    totalSum = totalSum + 1
  }else if(all.data$marker03[i] == "1"){
    infectedSum = infectedSum + 1
    marker3 = marker3 +1
    totalSum = totalSum + 1
  }else if(all.data$marker04[i] ==1){
    infectedSum = infectedSum + 1
    marker4 = marker4 +1
    totalSum = totalSum + 1
  }else if(all.data$marker05[i] ==1){
    infectedSum = infectedSum + 1
    marker5 = marker5 + 1
    totalSum = totalSum + 1
  }else if(all.data$marker06[i] ==1){
    infectedSum = infectedSum + 1
    marker6 = marker6 + 1
    totalSum = totalSum + 1
  }else if(all.data$marker07[i] ==1){
    infectedSum = infectedSum + 1
    marker7 = marker7 +1
    totalSum = totalSum + 1
  }else if(all.data$marker08[i] ==1){
    infectedSum = infectedSum + 1
    marker8 = marker8 +1
    totalSum = totalSum + 1
  }else if(all.data$marker09[i] ==1){
    infectedSum = infectedSum + 1
    marker9 = marker9 +1
    totalSum = totalSum + 1
  }else if(all.data$marker10[i] == 1){
    infectedSum = infectedSum + 1
    marker10 = marker10 + 1
    totalSum = totalSum + 1
  }else{
    totalSum = totalSum + 1
  }
}

PercentInfected <-infectedSum/totalSum
PercentInfected = PercentInfected*100

PercentM1<-marker1/infectedSum
PercentM1=PercentM1*100

PercentM2<-marker2/infectedSum
PercentM2=PercentM2*100

PercentM3<-marker3/infectedSum
PercentM3=PercentM3*100

PercentM4<-marker4/infectedSum
PercentM4=PercentM4*100

PercentM5<-marker5/infectedSum
PercentM5=PercentM5*100

PercentM6<-marker6/infectedSum
PercentM6=PercentM6*100

PercentM7<-marker7/infectedSum
PercentM7=PercentM7*100

PercentM8<-marker8/infectedSum
PercentM8=PercentM8*100

PercentM9<-marker9/infectedSum
PercentM9=PercentM9*100

PercentM10<-marker10/infectedSum
PercentM10=PercentM10*100

#Percent of Patients of that identify as Male or Female
# To get the % of screened patients that were male and female, use a for loop
totalfemale=0
totalmale=0
for (i in 1:screenran) {
  if (all.data$gender[i]=="female") {
    totalfemale=totalfemale+1
  } else if(all.data$gender[i]=="male") {
    totalmale=totalmale+1
  } 
}
fractionfemale<-totalfemale/screenran
percentfemale<-fractionfemale*100
fractionmale<-totalmale/screenran
percentmale<-fractionmale*100

#Age distribution of patients
child= 0 # Ages 0 - 8
preteen = 0 # Ages 9 - 12
teen = 0 # Ages 13 -19
adult = 0 #Ages 20 - 59
senior = 0 #Ages 60+

for(i in 1:screenran){
  if(all.data$age[i] >= 0 && all.data$age[i] < 9){
    child = child + 1
  }else if(all.data$age[i] >= 9 && all.data$age[i] <13){
    preteen =preteen + 1
  }else if(all.data$age[i] >= 13 && all.data$age[i] <20 ){
    teen = teen + 1
  }else if(all.data$age[i] >= 20 && all.data$age[i] < 60 ){
    adult = adult + 1
  }else if(all.data$age[i] >= 60){
    senior = senior + 1
  }
}

library(ggplot2)
library(cowplot)

agedf<-data.frame(
  agegroup = c('Child','Preteen','Teen','Adult','Senior'),
  population = c(child,preteen,teen,adult,senior))

# Define the order of age groups
age_order <- c('Child', 'Preteen', 'Teen', 'Adult', 'Senior')

# Convert agegroup to a factor with the specified order
agedf$agegroup <- factor(agedf$agegroup, levels = age_order)

# Create a bar plot for age distribution
ggplot(agedf, aes(x = agegroup, y = population, fill = agegroup)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Age Group", y = "Population", title = "Age Distribution of Patients") +
  scale_fill_manual(values = c('Child' = 'blue', 'Preteen' = 'green', 'Teen' = 'yellow', 'Adult' = 'orange', 'Senior' = 'red'))


library(ggplot2)


# Sample data creation (assuming marker1, marker2, ... are variables)
marker_df <- data.frame(
  marker = c(marker1, marker2, marker3, marker4, marker5, marker6, marker7, marker8, marker9, marker10),
  name = factor(c('marker1', 'marker2', 'marker3', 'marker4', 'marker5', 'marker6', 'marker7', 'marker8', 'marker9', 'marker10'),
                levels = c('marker1', 'marker2', 'marker3', 'marker4', 'marker5', 'marker6', 'marker7', 'marker8', 'marker9', 'marker10'))
)

# Create a ggplot line graph

ggplot(marker_df, aes(x = name, y = marker, group = 1)) +  # Add a group aesthetic to ensure connection
  geom_line() +
  geom_point() +  # Add points for each data point
  labs(x = "Marker Name", y = "Marker Value", title = "Line Graph of Marker Values for Country X and Y")



