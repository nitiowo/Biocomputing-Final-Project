# Supporting Functions
# Thomas Joyce and Patrick Kuebler

# Import libraries
library(ggplot2)


# Function that converts all files in a directory with 
# space- or tab-delimited data (.txt) into .csv files
# To use this function, input the absolute path to the directory

txt_to_csv <- function(directory_path){
  txt_files <- list.files(path = directory_path,"\\.txt$",full.names=TRUE) # Extract .txt files
  for (file in txt_files){
    data <- read.table(file, header = TRUE,sep="") # Read .txt files 
    file_name <- tools::file_path_sans_ext(file) # Remove extension
    write.csv(data, paste0(file_name,".csv"), row.names = FALSE) # Write .csv files
    file.remove(file) # Remove .txt files
  }
}


# Function to compile data from all .csv files in a 
# directory into a single .csv file
# To use this function, input the absolute path to the directory

single_csv <- function(directory_path){
  csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE) # Extract all .csv files
  compiled_data <- data.frame() # Initialize empty data frame
  for (file in csv_files) {
    country <- strsplit(strsplit(file,"y")[[1]][2],"/")[[1]][1] # Extract country
    dayofYear <- strsplit(strsplit(strsplit(file,"y")[[1]][2],"_")[[1]][2],"\\.")[[1]][1] # Extract dayofYear
    data <- read.csv(file, header = TRUE) # Read the .csv file
    data$country <- country # Add column for country
    data$dayofYear <- dayofYear # Add column for day of year
    compiled_data <- rbind(compiled_data, data) # Add the file to complied_data
  }
  cat("Please choose what you want to do about NA values. You have the following options: 
  A) Remove rows with NA's in any columns.
  B) Include NA's in the compiled data but be warned of their presence.
  C) Include NA's in the compiled data without a warning.")
  NA_choice <- readline(prompt= "Input Option A, B, or C: ")
  if (NA_choice == "A"){
    compiled_data <- na.omit(compiled_data)
  } else if (NA_choice == "B"){
    if (anyNA(compiled_data) == TRUE){
      print("Warning: The compiled data has NA's")
    } } else {
      
  }
  write.csv(compiled_data,paste0(directory_path,"/compiled_data.csv"), row.names = FALSE) # Write complied_data to a single .csv file
}


# Function to summarize the compiled dataset
# To use this function, input the data frame name for the compiled dataset

data_summary <- function(compiled_data){
  # Determine number of screens run
  screens_run <- nrow(compiled_data)
  # Determine percent of patients screened that were infected
  total_infected <- 0
  for (i in 1:nrow(compiled_data)){
    markers <- compiled_data[i,3:12]
    if (any(markers %in% 1) == TRUE){
      total_infected <- total_infected + 1
    } else {}
  }
  pct_infected <- 100*(total_infected/screens_run)
  # Determine percent of patients that identify as male and female
  males <- 0
  females <- 0
  for (i in 1:nrow(compiled_data)){
    if (compiled_data[i,1] == "male"){
      males <- males + 1
    } else {
      females <- females + 1
    }
  }
  pct_males <- (males/screens_run)*100
  pct_females <- (females/screens_run)*100
  # Age distribution of patients
  age_histogram <- ggplot(compiled_data, aes(x = age))+geom_histogram(binwidth=10,center=5,color="black",fill="blue")+
    ggtitle("Age Distribution of Patients")+xlab("Age (years)")+ylab("Number of Patients")
  # Create a data frame for the summary statistics
  summary_statistics <- data.frame(
    `Screens Run` = screens_run,
    `Percent Infected` = pct_infected,
    `Percent Male` = pct_males,
    `Percent Female` = pct_females
  )
  # Returned the desired output in a list format
  return(list(summary_statistics,age_histogram))
}


# Function that returns a line plot for the cumulative number of infections over time by country
# To use this function, input the data frame name for the compiled dataset

cumulative_infections <- function(compiled_data){
  countryX_infections <- 0
  countryY_infections <- 0
  countryX <- c()
  countryY <- c()
  times = c(unique(compiled_data$dayofYear))
  for (i in times){
    dailyX_infections <- 0
    dailyY_infections <- 0
    for (j in 1:nrow(compiled_data)){
      if (compiled_data[j,14] == i){
        markers <- compiled_data[j,3:12]
        if (any(markers %in% 1) == TRUE & compiled_data[j,13] == "X"){
          dailyX_infections <- dailyX_infections + 1
        } else if (any(markers %in% 1) == TRUE & compiled_data[j,13] == "Y"){
          dailyY_infections <- dailyY_infections + 1
        } else {}
      } else {}
    }
    countryX_infections <- countryX_infections + dailyX_infections
    countryY_infections <- countryY_infections + dailyY_infections
    countryX <- c(countryX,countryX_infections)
    countryY <- c(countryY,countryY_infections)
  }
  cumulative_infection_data <- data.frame(times,countryX,countryY)
  cumulative_infection_plot <- ggplot(cumulative_infection_data, aes(x = times)) +
    geom_line(aes(y = countryX, color="Country X"), linetype = "solid", size = 1) +
    geom_line(aes(y = countryY, color="Country Y"), linetype = "solid", size = 1) +
    scale_color_manual(breaks=c("Country X","Country Y"),values=c("blue","green"))+
    scale_x_continuous(breaks=c(120,125,130,135,140,145,150,155,160,165,170,175))+
    labs(title = "Cumulative Number of Infections Over Time by Country", x = "Day of Year", y = "Cumulative Number of Infections",color="Country")
  return(cumulative_infection_plot)
}


# Function that outputs a bar plot for marker frequencies for all infected individuals in a country in a particular time span
# To use this function, input the data frame name, country name ("X" or "Y"), first day of period, and last day of period.
# Note: first_day=120 and last_day=175 by default, but the user can change this if desired.

marker_frequencies <- function(compiled_data,country,first_day=120,last_day=175){
  infected_individuals <- data.frame()
  for (i in 1:nrow(compiled_data)){
    markers <- compiled_data[i,3:12]
    if (any(markers %in% 1) == TRUE & compiled_data[i,13]==country & compiled_data[i,14] >= first_day & compiled_data[i,14] <= last_day){
      infected_individuals <- rbind(infected_individuals,compiled_data[i,])
    } else {}
  }
  marker_counts <- rep(0,10)
  for (j in 1:nrow(infected_individuals)){
    for (k in 1:10){
      if (infected_individuals[j,k+2] == 1){
        marker_counts[k] <- marker_counts[k] + 1
      } else {}
    }
  }
  freq <- marker_counts
  marks <- data.frame(marker = c("01","02","03","04","05","06","07","08","09","10"),frequency <- freq)
  barplot <- ggplot(marks,aes(x=marker,y=frequency))+geom_bar(stat="identity",color="Black",fill="Orange")+
    ggtitle(paste("Marker Frequencies for Country",country,"between Day",first_day,"and Day",last_day))+xlab("Marker")+ylab("Number of Infected Individuals with Marker")
  return(barplot)
}



