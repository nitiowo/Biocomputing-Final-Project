# Biergans and Bianchi Final R Project

# Supporting Functions Script

# Initializes an empty data frame with 14 columns (12 for data, 1 for dayofYear, and 1 for Country)
column_names <- c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10", "dayofYear", "Country")
combined_data <- data.frame(matrix(ncol = 14, nrow = 0))
colnames(combined_data) <- column_names

# Process files from countryX
files_countryX <- list.files(path = "countryX", pattern = "*.csv", full.names = TRUE)
for (file in files_countryX) {
  # Read the CSV file
  data <- read.csv(file, header = TRUE, sep = ",")
  
  # Extract day of year from the file name and assign country
  dayofYear <- as.numeric(sub("screen_(\\d+)\\.csv", "\\1", basename(file)))
  data$dayofYear <- dayofYear
  data$Country <- "X"
  
  # Combine with the main dataframe
  combined_data <- rbind(combined_data, data)
}

# Process files from countryY
files_countryY <- list.files(path = "countryY", pattern = "*.txt", full.names = TRUE)
for (file in files_countryY) {
  # Read the TXT file, skipping the first (header) line that contains the titles.
  data <- read.table(file, header = FALSE, sep = " ", skip = 1, col.names = column_names[-(13:14)])
  
  # Extract day of year from the file name and assign country
  dayofYear <- as.numeric(sub("screen_(\\d+)\\.txt", "\\1", basename(file)))
  data$dayofYear <- dayofYear
  data$Country <- "Y"
  
  # Combine with the main dataframe
  combined_data <- rbind(combined_data, data)
}

# Write the combined data to a new CSV file
write.csv(combined_data, "combined_data.csv", row.names = FALSE)

# Summarizes the combined data
summarizeData <- function(combined_data.csv) {
  data <- read.csv(combined_data.csv)
  
  # Total number of screens
  total_screens <- nrow(data)
  
  # Percentage of infected patients
  data$infected <- rowSums(data[, grepl("marker", names(data))]) > 0
  percent_infected <- sum(data$infected) / total_screens * 100
  
  # Gender percentages
  percent_male <- sum(data$gender == "male") / total_screens * 100
  percent_female <- sum(data$gender == "female") / total_screens * 100
  
  # Age distribution
  age_distribution <- table(data$age)
  
  # Print results
  cat("Total Screens:", total_screens, "\n")
  cat("Percent Infected:", percent_infected, "%\n")
  cat("Percent Male:", percent_male, "%\n")
  cat("Percent Female:", percent_female, "%\n")
  cat("Age Distribution:\n")
  print(age_distribution)
}
