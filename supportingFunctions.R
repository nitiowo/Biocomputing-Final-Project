

#Step 1: Write a function to convert all the .txt files in Country Y Folder to .csv files

convertToCSV <- function(directory) {
  files <- list.files(directory, pattern = "\\.(txt|TXT)$", full.names = TRUE)
  
  for (file in files) {
    data <- read.table(file, sep = "", header = TRUE)
    filename <- tools::file_path_sans_ext(basename(file))
    
    write.csv(data, file = paste0(filename, ".csv"), row.names = FALSE)
  }
}


#Step 2: Compile data from all .csv files into a directory into a single .csv file
compileData <- function(directory) {
  na_handling <- readline(prompt = "Choose NA handling (remove/warn/include): ")
  
  files <- list.files(directory, pattern = "screen_\\d+\\.csv$", full.names = TRUE)
  
  extractInfo <- function(directory, file) {
    parts_dir <- unlist(strsplit(directory, "/"))
    country <- parts_dir[length(parts_dir)]  # Get the country from the directory name
    
    filename <- basename(file)
    dayOfYear <- as.integer(gsub("screen_|\\.csv", "", filename))  # Get the day of the year from the file name
    return(list(country = country, dayOfYear = dayOfYear))
  }
  
  all_data <- lapply(files, function(file) {
    data <- read.csv(file)
    info <- extractInfo(directory, file)
    data$Country <- info$country
    data$DayOfYear <- info$dayOfYear
    return(data)
  })
  
  #code for na handling
  if (na_handling == "remove") {
    all_data <- lapply(all_data, function(data) data[complete.cases(data), ])
  } else if (na_handling == "warn") {
    na_count <- sapply(all_data, function(data) sum(is.na(data)))
    if (any(na_count > 0)) {
      warning("Warning: There are NA values in the compiled data.")
    }
  }
  #make output file and save it to the directory
  output_file <- paste0("compiled_data_", all_data[[1]]$Country[1], ".csv")
  combined_data <- do.call(rbind, all_data)
  write.csv(combined_data, file = file.path(directory, output_file), row.names = FALSE)
}



#Step 3: Summarize Data

summarizeData <- function(file_path) {
  
  all_data <- read.csv(file_path)
  
  #If the user decided to keep the NA values in this will exclude them from analysis so the code can run properly
  all_data <- na.omit(all_data)
  
  #screens run
  num_screens <- nrow(all_data)
  
  #identify patients as infected if they have at least 1 genetic marker present
  all_data$infected <- ifelse(rowSums(all_data[, 3:12]) >= 1, "Yes", "No")
  
  #calculate percentage of screened patients infected based on patients identified 
  percent_infected <- mean(all_data$infected == "Yes") * 100
  
  #create a barplot to summarize the presence/frequency of genetic markers present in patients to help answer the vaccine question
  sums <- colSums(file_path[,3:12])
  barplot(sums, names.arg = colnames(file_path)[3:12],
          main = "Frequency of Genetic Markers")
  
  #calculate percentage of patients identifying as male and female
  percent_male <- mean(all_data$gender == "male") * 100
  percent_female <- mean(all_data$gender == "female") * 100
  
  #create age distribution plot -> histogram
  hist(all_data$age, main="Age Distribution of Patients", xlab="Age", ylab="Frequency")
  
  #disply the summary results
  cat("Number of screens run:", num_screens, "\n")
  cat("Percentage of screened patients infected:", percent_infected, "%\n")
  cat("Percentage of patients identifying as male:", percent_male, "%\n")
  cat("Percentage of patients identifying as female:", percent_female, "%\n")

}



