convert_Y_to_csv <- function(directory) {
  # Get a list of all files in the directory
  all_file <- list.files(directory)
  # Loop through all the files
  for(file in all_file) {
    # Use the original file name
    file_name <- strsplit(file,".txt")
    # add ".csv" to the end of file_name
    csv_file_name <- paste0(file_name, ".csv")
    # Read the tab-delimited file
    data <- read.table(file, header = TRUE, sep = " ", stringsAsFactors = FALSE)
    # Write into a csv file
    write.csv(data, csv_file_name, row.names = FALSE)
  }}

compile_data <- function(directory){
  # Get a list of all files in the directory
  all_file <- list.files(directory, pattern = "\\.csv$")
  # Make a new data frame to save all the data
  compiled_data <- data.frame()
  split_country = strsplit(directory,"country")
  country <- sapply(split_country, function(x) x[2])
  # add ".csv" to the end of file_name
  output <- paste0(country, ".csv")
  # Loop through all the data file
  for(file in all_file) {
    data <- read.csv(file, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    split_csv <- strsplit(file,".csv")
    splited_csv <- paste(split_csv)
    split_year <- strsplit(splited_csv, "_")
    year <- split_year[[1]][2]
    # Add a new column 'country'
    data$country <- c(rep(country, nrow(data)))
    # Add a new column 'dayofYear'
    data$dayofYear <- c(rep(year, nrow(data)))
    
    compiled_data <- rbind(compiled_data, data)
  }
  # Write into a csv file
  write.csv(compiled_data, output, row.names = FALSE)
}

#data analysis
analysis <- function(x){
  table <- read.csv(x, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  #number of screen runs
  number <- dim(table)[1]
  text <- "The total number of screening is"
  print(paste(text, number))

  #percent of patients screened that were infected and put it to a dataframe
  total_infection <- data.frame()
  for (i in 1:nrow(table)) {
    for (j in 3:12){
      if (!is.na(table[i,j]) && table[i,j] >= 1){
        total_infection <- rbind(total_infection, table[i,])
        break
      }
      }
  }
  text_2 <- "percentage of infected patients is "
  all_infect <- dim(total_infection)[1]
  calc <- all_infect/number *100
  print(paste(text_2, calc))
  
  #percentage of male and female from all patients
  #(I was a little confused about this, I calculated the percentage according to the whole number of patients instead of infected onces, because the question did not ask about it
  male <- table[table$gender == "male", ]
  female <- table[table$gender == "female", ]
  num_male <- dim(male)[1]/number*100
  text_male <- "The percentage of male patients is "
  print(paste(text_male, num_male))
  num_female <- dim(female)[1]/number*100
  text_female <- "The percentage of female patients is "
  print(paste(text_female, num_female))
  
  #percentage of male and female from infected patients
  inmale <- total_infection[total_infection$gender == "male", ]
  infemale <- total_infection[total_infection$gender == "female", ]
  num_inmale <- dim(inmale)[1]/all_infect*100
  text_inmale <- "The percentage of infected male among infected patients is "
  print(paste(text_inmale, num_inmale))
  num_infemale <- dim(infemale)[1]/all_infect*100
  text_infemale <- "The percentage of infected female among infected patients is "
  print(paste(text_infemale, num_infemale))
  
  
  #age distribution
  ggplot(data = table, aes(x = age)) +
    geom_histogram(binwidth = 5, fill = "red", color = "black") +
    theme_classic()
  
  #percentage of country X and Y from infected patients
  inX <- total_infection[total_infection$country == "X", ]
  inY <- total_infection[total_infection$country == "Y", ]
  num_inX <- dim(inX)[1]/all_infect*100
  text_inX <- "The percentage of infected people from country X "
  print(paste(text_inX, num_inX))
  num_inY <- dim(inY)[1]/all_infect*100
  text_inY <- "The percentage of infected people from country Y "
  print(paste(text_inY, num_inY))
  
  #Markers analysis plot
    #all infected from X
    Marker01 <- c("Marker01", dim(inX[inX$marker01 == 1, ])[1])
    Marker02 <- c("Marker02", dim(inX[inX$marker02 == 1, ])[1])
    Marker03 <- c("Marker03",dim(inX[inX$marker03 == 1, ])[1])
    Marker04 <- c("Marker04",dim(inX[inX$marker04 == 1, ])[1])
    Marker05 <- c("Marker05",dim(inX[inX$marker05 == 1, ])[1])
    Marker06 <- c("Marker06",dim(inX[inX$marker06 == 1, ])[1])
    Marker07 <- c("Marker07",dim(inX[inX$marker07 == 1, ])[1])
    Marker08 <- c("Marker08",dim(inX[inX$marker08 == 1, ])[1])
    Marker09 <- c("Marker09",dim(inX[inX$marker09 == 1, ])[1])
    Marker010 <- c("Marker10",dim(inX[inX$marker10 == 1, ])[1])
    Marker_X <- data.frame()
    Marker_X <- rbind(Marker01, Marker02, Marker03, Marker04, Marker05, Marker06, Marker07, Marker08, Marker09,Marker010)
    print(Marker_X)
    
    #all infected from Y
    Marker1 <- c("Marker01", dim(inY[inY$marker01 == 1, ])[1])
    Marker2 <- c("Marker02", dim(inY[inY$marker02 == 1, ])[1])
    Marker3 <- c("Marker03",dim(inY[inY$marker03 == 1, ])[1])
    Marker4 <- c("Marker04",dim(inY[inY$marker04 == 1, ])[1])
    Marker5 <- c("Marker05",dim(inY[inY$marker05 == 1, ])[1])
    Marker6 <- c("Marker06",dim(inY[inY$marker06 == 1, ])[1])
    Marker7 <- c("Marker07",dim(inY[inY$marker07 == 1, ])[1])
    Marker8 <- c("Marker08",dim(inY[inY$marker08 == 1, ])[1])
    Marker9 <- c("Marker09",dim(inY[inY$marker09 == 1, ])[1])
    Marker10 <- c("Marker10",dim(inY[inY$marker10 == 1, ])[1])
    Marker_Y <- data.frame()
    Marker_Y <- rbind(Marker1, Marker2, Marker3, Marker4, Marker5, Marker6, Marker7, Marker8, Marker9, Marker10)
    print(Marker_Y)

}



