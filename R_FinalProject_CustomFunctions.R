#Custom Functions 

# 1. convert a .txt file to .csv
convert_txt_to_csv_file = function(txt_filename, csv_filename, sep = " ") {
  data = read.table(txt_filename, sep = sep, header = TRUE)
  write.csv(data, csv_filename, row.names = FALSE)
}

# 2. convert all .txt files under a dir into .csv format
convert_txt_to_csv_dir = function(dirname) {
    dirname = "countryY"
  files = list.files(dirname, full.names = TRUE)
  for (file in files) {
    if (grepl("\\.txt$", file)) {
      new_file = sub("\\.txt$", ".csv", file)
      convert_txt_to_csv_file(file, new_file)
    }
  }
}

# 3.Compiles Multiple files, adds two columns, and allows user to specify about NAs 
CompileFiles<- function(Path, CountryName, NameFile, VectorFileNames, na="Warn"){
  #this makes an empty data frame to store compiled data
  allDataDF = data.frame(matrix(vector(), 0, 14,
                                dimnames=list(c(), c("Gender","Age","Marker01", "Marker02", "Marker03", "Marker04", "Marker05", "Marker06", "Marker07", "Marker08", "Marker09", "Marker10", "Country", "DayofYear"))),
                         stringsAsFactors=F)
  #for loop to go through each file and places the info into the empty data frame
  for(i in VectorFileNames){
    o<-read.csv(i,header =TRUE) #Reads in all the csv's 
    
    #this splits the name of the file into 120.csv
    W<-strsplit(i,"_")[[1]][2]
    #this splits the name of the file into 120 and stores it into a vector
    day<-as.numeric(strsplit(W,".csv")[[1]][1])
    
    #makes a column called Country and lets the user name the country by using it as an argument
    o$Country<-CountryName
    
    #makes a column called DayofYear and puts in the day info
    o$DayofYear<-day
    
    #puts the csv info into the empty matrix
    allDataDF<-rbind(allDataDF, o)
  }
  #if else statment that allows the user to choose how to handle NA's
  if (na=="remove") {
    isthereNA<-rowSums(is.na(allDataDF))
    allDataDF[isthereNA==0,]
  }else if(na=="Ignore"){
    isthereNA<-rowSums(is.na(allDataDF))
  }else if(na=="Warn"){
    isthereNA<-rowSums(is.na(allDataDF))
    print("There are NA's Present")
  }
  write.csv(allDataDF, NameFile, row.names = FALSE)
}


#example of usage
#CompileFiles(path, CountryName = "Y", NameFile = "YCountryData.csv",VectorFileNames = csvfilesY, na="remove")


# 4. custom function to summarize the all data
data_analyze = function(filename) {
  data = read.csv(filename) #read in information
  #initialize a data frame to store all summarized information
  summarized_all_df = data.frame(
    DayofYear = numeric(), # day
    Country = character(), # country
    screens = numeric(),# number of screens
    infected = numeric(), # number of infected patients
    infected_male = numeric(), # number of infected male patients
    infected_female = numeric(), # number of infected female patients
    infected_male_portion = numeric(), # percentage of infected male
    infected_age_group1 = numeric(), # number of patients younger than 20
    infected_age_group2 = numeric(), # number of patients between 20 to 40
    infected_age_group3 = numeric(), # number of patients between 40 to 60
    infected_age_group4 = numeric(), # number of patients between 60 to 80
    infected_age_group5 = numeric(), # number of patients older than 80
    stringsAsFactors = FALSE
  )
  
  days = unique(data$DayofYear) # get how many different days are there in the all data file
  countries = unique(data$Country) # get how many countries are recorded in all data file 
  
  for (day_id in days) {
    df_day = data[data$DayofYear == day_id, ] #get subset data frame by different day
    for (country_id in countries) {
      df = df_day[df_day$Country == country_id, ] #get a individual data frame for one country on single day 
      df_infected = df[rowSums(df[, 3:12], na.rm = TRUE) > 0, ] # analyze the individual data frame to see how many patients are infected
      # number of screens for a day
      num_screens = nrow(df)
      # number of infected for a day
      num_infected = nrow(df_infected)
      # number of infected by gender
      num_male_infected = nrow(df_infected[df_infected$gender == "male", ])
      num_female_infected = nrow(df_infected[df_infected$gender == "female", ])
      infected_male_portion = 0 # initialize infected_male_portion
      if (num_infected != 0) # calculate the infected male percentage when there are infected patients
        infected_male_portion = num_male_infected / num_infected
      # number of infected by age group
      num_infected_age_group1 = nrow(df_infected[df_infected$age >= 0 & df_infected$age < 20, ])
      num_infected_age_group2 = nrow(df_infected[df_infected$age >= 20 & df_infected$age < 40 , ])
      num_infected_age_group3 = nrow(df_infected[df_infected$age >= 40 & df_infected$age < 60, ])
      num_infected_age_group4 = nrow(df_infected[df_infected$age >= 60 & df_infected$age < 80, ])
      num_infected_age_group5 = nrow(df_infected[df_infected$age >= 80, ])
      # create a new row
      new_row = list(DayofYear = day_id, Country = country_id,
                     screens = num_screens, infected = num_infected,
                     infected_male = num_male_infected, infected_female = num_female_infected,
                     infected_male_portion = infected_male_portion,
                     infected_age_group1 = num_infected_age_group1,
                     infected_age_group2 = num_infected_age_group2,
                     infected_age_group3 = num_infected_age_group3,
                     infected_age_group4 = num_infected_age_group4,
                     infected_age_group5 = num_infected_age_group5)
      summarized_all_df[nrow(summarized_all_df) + 1, ] = new_row
    }
  }
  return(summarized_all_df) # get the summarized data frame
}

























































