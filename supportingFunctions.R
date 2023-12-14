# function to convert .txt to .csv files
# usage: argument is the directory path to where the txt files are nested
# output: .csv files saved in same location(s) in which the .txt files were pulled from
txt2csv <- function(directory) {
  # loading files from directory
  files <- dir(no.. = TRUE, recursive = TRUE, include.dirs = TRUE)
  # taking only the .txt files
  txtfiles <- dir(pattern = "[.txt]$", recursive = TRUE)
  # for loop to go through each txt file and save as a csv
  for(i in 1:length(txtfiles)) {
    # rewriting file name to end in .csv rather than .txt
    newname <- paste0(gsub("\\.txt", "", txtfiles[i]), ".csv")
    # reading in each of the txt files from the inputted directory
    readingfile <- (read.delim(txtfiles[i], sep = " ", header = TRUE))
    # saving the .txt files as .csv files
    write.csv(readingfile, file = newname, sep = ",", row.names = FALSE)    # row.names to remove indices
  }
}


################################################################################


# usage: argument is the directory path to where the csv files are nested in
# output: will combine and save csv files into one file named "all.csv" or "all_noNA.csv"
#         also provides global variable called "allcsv" that is the combined csv dataset
# includes user input to choose between 3 options for handling NA's in data
mergecsv <- function(directory) {
  # storing all the csv files into a placeholder
  csvfiles <- list.files(directpath, recursive = TRUE, pattern = "(screen).*csv$")
  
  # initializing variables to use in function
  country <- c()
  DayOfYear <- c()
  
  # nested function to create the country and DayOfYear variables to add to the compiled csv file
  countryandday <- function(input) {
    # initialize the variables
    separate <- c()
    countryonly <- c()
    dayonly <- c()
    # for loop to iterate through each csv file
    for (i in 1:length(input)) {
      # separating the country from the day of the year
      separate[i] <- strsplit(input[[i]], split = "/")
      # getting only the country and separating each character
      countryonly <- unlist(strsplit(unlist(separate[[i]])[1], split = ""))
      
      # getting the day of the year
      dayonly <- strsplit(unlist(separate[[i]])[2], split = "\\.")
      
      # getting the number of rows of each file
      num = nrow(read.csv(input[i], header = TRUE))
      
      # number of times to repeat the country/day for each file to correspond to every patient
      countryrepetition <- rep(countryonly[[8]], num)       # country by taking the last character of the string
      dayrepetition <- rep(gsub("\\D", "", dayonly), num)   # day of the year by only taking the digit values
      
      # placing the country (X or Y) into the country variable
      country <<- append(country, countryrepetition)
      # placing the day of year into the DayOfYear variable
      DayOfYear <<- append(DayOfYear, dayrepetition)
      
    }
  }
  
  # running the countryandday function to get the country and DayOfYear variables
  countryandday(csvfiles)
  
  # initialize variables to iterate while combinind csv files into a single data file
  toadd <- c()
  all <- c()
  # for loop to iterate through each file and compile it
  for (i in seq_along(csvfiles)) {
    # for the first file, making it start the compiled file
    if (i == 1) {
      compiled <- read.csv(csvfiles[i], header = TRUE)
    }
    # for every file after, adding it to the running file compilation
    else {
      toadd <- read.csv(csvfiles[i], header = TRUE)
      compiled <- rbind(compiled, toadd)
    }
  }
  
  # adding the country and day of year variables to the compiled data, then assigning as a variable
  all <- cbind(compiled, country, DayOfYear)
  
  answer <- menu(c("Remove any rows with NA", "Include NA, but provide warning", "Include NA; no warning"), 
                 title="How would you like to compile the .csv files?")
  if (answer == 1) {
    all_noNA <- na.omit(all)
    write.csv(all_noNA, file = "all_noNA.csv", row.names = FALSE)
    return(allcsv <<- all_noNA)
  }
  else if (answer == 2) {
    write.csv(all, file = "all.csv", row.names = FALSE)
    warning("The dataset contains NA values.")
    return(allcsv <<- all)
  }
  else {
    write.csv(all, file = "all.csv", row.names = FALSE)
    return(allcsv <<- all)
  }
}


################################################################################


# function to summarise the data (percent of infected, percent of female/male, total number of screens)
# usage: argument is the compiled csv data
# output: summary table global variable and saves as a csv
datasummary <- function(allcsv) {
  # separating the countries into their own subset
  countryX <- allcsv[allcsv$country == "X",]
  countryY <- allcsv[allcsv$country == "Y",]
  
  # calculating the percentage of country infected by finding the number of infected 
  # and then dividing by total number of screens for the country
  rowsXinfect <- apply(countryX[, 3:12], 1, function(x) any(x > 0))
  Xpercent <- (nrow(countryX[rowsXinfect,]) / nrow(countryX)) * 100
  
  rowsYinfect <- apply(countryY[, 3:12], 1, function(x) any(x > 0))
  Ypercent <- (nrow(countryY[rowsYinfect,]) / nrow(countryY)) * 100
  
  # percentage of gender is calculated by finding number of screens for each gender
  # then dividing by the total number of screens for the country
  femX <- nrow(countryX[countryX$gender == "female",]) / nrow(countryX) * 100
  femY <- nrow(countryY[countryY$gender == "female",]) / nrow(countryY) * 100
  maleX <- 100-femX
  maleY <- 100-femY
  
  # saving all of the above into a table
  summarytable <- data.frame(country = c("X", "Y"), 
                             screens = c(nrow(countryX), nrow(countryY)),
                             percent_infect = c(Xpercent, Ypercent), 
                             percent_female = c(femX, femY),
                             percent_male = c(maleX, maleY))
  
  # saving table as csv and global variable
  write.csv(summarytable, "summary table.csv", row.names = FALSE)
  summarytable <<- summarytable
}


################################################################################


# function for plotting the data and comparing data between countries
# population pyramid, incidence graph, heat map of marker occurrence, and a scatter plot of marker occurence
# usage: argument is the compiled csv data and as a single pdf file
# requires tidyverse and gridExtra packages
countryplots <- function(alldata) {
  
  library(tidyverse)
  library(gridExtra)
  
  
  # Population pyramid
  poppyr <- alldata %>%
    ggplot(., aes(x = age, fill = country)) +
    # filtering by gender
    geom_bar(data = subset(alldata, gender == "female")) +
    geom_bar(data = subset(alldata, gender == "male"), aes(y = after_stat(count)*(-1))) +
    scale_y_continuous(breaks = seq(-1500,1500,500), labels = abs(seq(-1500,1500,500))) +
    scale_x_continuous(breaks = seq(0,110,10), labels = scales::comma(abs(seq(0,110,10)))) +
    coord_flip() + xlab("Age (Years)") + ylab("Population") +
    theme_light() + guides(fill = guide_legend(title = "Country")) +
    scale_fill_brewer(palette = "Dark2") + labs(title = "Population Pyramid of Country X & Country Y")
  
  # Incidence graph
  incidence <- allcsv %>%
    # selecting screens that indicate infection
    filter(rowSums(.[, 3:12]) >= 1) %>%
    ggplot(., aes(x = as.numeric(DayOfYear))) + 
    geom_histogram(na.rm = TRUE, aes(group = country, fill = country, color = country),
                                                   position = "identity", alpha = 0.65) +
    scale_fill_brewer(palette = "Dark2") + scale_color_brewer(palette = "Dark2") + theme_bw() +
    labs(title = "Incidence of Disease") + xlab("Day of the Year") + ylab("Case Counts") +
    guides(fill = guide_legend(title = "Country"), color = guide_legend(title = "Country"))
  
  
  # Heat map
  heatmap <- alldata %>%
    select(contains("marker"), country) %>%
    pivot_longer(starts_with("marker"), names_to = "markers") %>%
    group_by(country, markers) %>%
    summarise(counts = sum(value)) %>%
    ggplot(., aes(x = markers, y = country, fill = counts)) + 
    geom_tile(color = "white", linewidth = 1.25, linetype = 1) + coord_fixed() +
    scale_fill_viridis_c(direction = -1, option = "viridis") + theme_light() +
    labs(title = "Heatmap of the Occurence of the Markers in Each Country", 
         fill = "Counts") + xlab("Marker") + ylab("Country") +
    scale_x_discrete(labels = c(seq(1:10))) + theme(legend.position = "bottom")
  
  
  # Scatter plot
  scatplot <- alldata %>%
    select(contains("marker"), country) %>%
    pivot_longer(starts_with("marker"), names_to = "markers") %>%
    group_by(country, markers) %>%
    summarise(counts = sum(value)) %>%
    ggplot(., aes(x = markers, y = counts)) + geom_point(aes(color = country, shape = country), size = 8) +
    scale_color_brewer(palette = "Dark2") + theme_bw() +
    labs(title = "Plot of Marker Occurence for Each Country", 
         color = "Country", shape = "Country") + xlab("Marker") + ylab("Counts") +
    scale_x_discrete(labels = c(seq(1:10)))
  
  
  # saving as pdf
  ggsave(plot = marrangeGrob(list(poppyr, incidence, heatmap, scatplot), nrow = 2, ncol = 2), 
         filename = "country plots.pdf", dpi = 700, width = 11, height = 10)
  print("Plots saved as 'country plots.pdf'")
}