# Final R Project
# Temitope Kessim, Joshua Sobo, Caroline Daniher

setwd("~/Desktop/R Biocomputing/Biocomputing-Final-Project")
library(ggplot2)
library(cowplot)

# CSV CONVERSION
# this is a function to convert .txt files into .csv files in a particular directory
# to convert a whole directory, simply write "csv_conversion('dir')", in which dir is the directory you want to transform the files in 
csv_conversion <- function(dir) {
  # create a file to get all the files together from a directory
  files <- list.files(dir,"\\.txt",full.names=TRUE)
  # if/else statment to avoid converting files already in csv format
  if (length(files)==0) {
    print("The files in this directory are already in .csv format.")
  } else{
    for (i in 1:length(files)){
      # for loop through each .txt file to identify it and read it
      new_csv_name <- sub("\\.txt", ".csv", files[i])
      data <- read.table(files[i], header = TRUE, sep = "")
      # then, write a .csv file given the .txt file and remove the old .txt file
      write.csv(data, new_csv_name, row.names = FALSE)
      file.remove(files[i])
    }
    print("All files in this directory have been converted to .csv.")
  }
}

# COMPILE FUNCTION
# for this function, you must compile the directories you want to combine in a single csv files.
# you assume that all the directories have csv files in them
# simply write "csv_compile(c('dir1','dir2',...)), in which dir1 and dir2 are directories that you want to compile. you can add any number of directories 
csv_compile<-function(dir_list) {
   #create a data frame to store all the csv file data
    compile_all<-data.frame(matrix(ncol=14,nrow=0))
    cat("You have three options:\n",
        "1. Remove rows with NA in any of the columns.\n",
        "2. Include NAs in the compiled data but be warned of their presence.\n",
        "3. Include NAs in the compiled data without a warning.\n",
        "Please choose 1, 2, or 3.\n")
    answer<-readline("Answer: ")
    # loop through each directory provided in the function prompt to get all the data
    for (dir in dir_list) {
      # create a file to get all the files together from each directory (in csv format)
      files_csv <- list.files(dir,"\\.csv$",full.names=TRUE)
      # loop through each file in files_csv to read each csv file in each directory
      for(i in 1:length(files_csv)){
        # get path of current file
        current_file<-read.csv(files_csv[i])
        #extract the country name 
        country<-sub("country","",dir)
        file_name<-basename(files_csv[i])
        # extract the day from each csv file name using strsplit
        day<-(strsplit(file_name,"[_.]")[[1]][2])
        # column bind the data in the current file and the country and the day
        current_file<-cbind(current_file,country,day)
        # row bind all the new data in the current file and the compile_all
        compile_all<-rbind(compile_all, current_file)
      }
    }
    # remove the NA if the user chooses answer 1 and provide a warning but don't remove NA if the user choose answer 2 
    if(sum(is.na(compile_all))>0) {
      if(answer == "1") {
        compile_all<-na.omit(compile_all)
      } else if(answer=="2") {
        print("Warning! There are NAs present in the compiled data")
      }
    }
    # return the compiled csv file, called compile_all.
    output_file<-paste0("compile_all",".csv")
    return(write.csv(compile_all, file = output_file, row.names = FALSE))
  }

# SUMMARIZE FUNCTION
# for use, simply write "countingCases(data)", where "data" is the compiled file of everything
data <- read.csv('allData.csv', header = TRUE, sep = ',')
countingCases <- function(filename){
  xcount<- 0
  ycount <- 0
  malecount<-0
  femalecount<- 0 
  #counting infected patients
  for (line in 1:nrow(filename)){
    if (filename$country[line] == 'X'){
      for (x in 3:12){
        if (filename[line,x] == 1){
          xcount <- xcount + 1
          break;
        }
      }
    }
    else if (filename$country[line] == 'Y'){
      for (x in 3:12){
        if (filename[line,x] == 1){
          ycount <- ycount + 1
          break;}
      }
    }
  }
  print(paste('total amount of infected patients in X:', xcount))
  print(paste('total amount of infected patients in Y:', ycount))
  #total amount of screens
  totalscreens<- 0
  for (line in 1:nrow(filename)){
    totalscreens = totalscreens +1  }
  
  totalinfected <- ((xcount + ycount)/totalscreens) *100
  
  print(paste('The total amount of screens are:', totalscreens))
  print(paste('The percent of patients that were infected:', totalinfected))
  
  # finding male and female patient percentages
  
  totalmalepatients<-nrow(data[data$gender == 'male',]) #counting total male patients by rows with gender == male
  totalfemalepatients<- nrow(data[data$gender == 'female',]) #same as above but for female
  
  for (line in 1:nrow(data)){
    for (x in 3:12){ #iterates only through the markers and passes following conditions
      if (data$gender[line] == 'male' & data[line,x] == 1){ #iterating through to find which males are infected
        malecount <- malecount + 1 #counts infected males
        break;}
      else if (data$gender[line] == 'female' & data[line,x] == 1){ #iterates through females that are infected
        femalecount <- femalecount + 1 #counts infected females
        break;}
    }
  }
  
  totalmale <- totalmalepatients/totalscreens*100 #percentage of males infected out of all screens
  maleratio <- malecount/totalscreens *100 #percentage of males out of all screens
  totalfemale<- totalfemalepatients/totalscreens*100 #percentage of females infected out of all screens
  femaleratio<- femalecount/totalscreens *100 #perecentage of females out of all screens
  print('Distribution of Infected Patients')
  print(paste('The percent of patients that were infected:', totalinfected))
  print(paste('The percent of infected patients that identify as male:', maleratio))
  print(paste('The percent of patients that identify as male:', totalmale))
  print(paste('The percent of infected patients that identify as female:', femaleratio))
  print(paste('The percent of patients that identify as female:', totalfemale))
  
  #Age distribution table
  ggplot (data = data, aes(x = age)) +
    geom_histogram(binwidth = 1, fill ="lightblue", color = "black") +
    theme_minimal()
}

