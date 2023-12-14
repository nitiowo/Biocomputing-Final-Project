#1 SupportingFunctions.R


#a Function to convert Delimited files to CSV
cconvertToCSV <- function(directory) {
  files <- list.files(directory, pattern="\\.txt$", full.names=TRUE)

  for (file in files) {
    # Read the first line to guess the delimiter
    firstLine <- readLines(file, n=1)
    delimiter <- ifelse(grepl("\t", firstLine), "\t", " ")

    # Read the file with the appropriate delimiter
    data <- read.table(file, sep=delimiter, header=TRUE, fill=TRUE, check.names=TRUE)

    # Create CSV filename
    csvName <- sub("\\.txt$", ".csv", file)

    # Write to CSV
    write.csv(data, csvName, row.names=FALSE)
  }
}

#b Function to compile CSV files into a single file
compileCSV <- function(directory, removeNA=FALSE, warnNA=TRUE, markerPrefix="marker") {
  files <- list.files(directory, pattern="\\.csv$", full.names=TRUE)
  compiledData <- NULL

  for (file in files) {
    data <- read.csv(file)
    fileNameParts <- unlist(strsplit(basename(file), "[_.]"))
    data$country <- fileNameParts[1]
    data$dayofyear <- as.numeric(fileNameParts[2])

    # Identify marker columns and determine if a patient is infected
    markerCols <- grep(markerPrefix, names(data), value = TRUE)
    data$infected <- rowSums(data[, markerCols], na.rm = TRUE) > 1

    if (removeNA) {
      data <- na.omit(data)
    } else if (warnNA && any(is.na(data))) {
      warning("NA values found in file: ", file)
    }

    compiledData <- rbind(compiledData, data)
  }

  write.csv(compiledData, file.path(directory, "compiled_data.csv"), row.names=FALSE)
  return(compiledData)
}

#c Function to summarize Data
summarizeData <- function(data) {
  totalScreens <- nrow(data)
  infectedRate <- mean(data$infected, na.rm=TRUE) * 100
  genderDistribution <- prop.table(table(data$gender)) * 100
  ageDistribution <- hist(data$age, plot=FALSE)

  cat("Total Screens Run:", totalScreens, "\n")
  cat("Percentage of Patients Infected:", infectedRate, "%\n")
  cat("Gender Distribution (Percentage):\n")
  print(genderDistribution)
  plot(ageDistribution, main="Age Distribution of Patients", xlab="Age", ylab="Frequency")
}


#d Function to graph number of infected in each country
plotNumberOfInfectedForAllCountries <- function(filePath, timeVariable = "dayofyear") {
  # Load the compiled data
  data <- read.csv(filePath, stringsAsFactors = FALSE)

  # Check if necessary columns are present
  if (!(timeVariable %in% names(data) && "infected" %in% names(data) && "country" %in% names(data))) {
    stop("Required columns are missing in the data")
  }

  # Sum the number of infected individuals per day for each country
  numberOfInfectedOverTime <- aggregate(infected ~ country + get(timeVariable), data = data, sum)

  # Plotting number of infected over time for all countries
  library(ggplot2)
  ggplot(numberOfInfectedOverTime, aes_string(x = timeVariable, y = "infected", color = "country", group = "country")) +
    geom_line() +
    labs(title = "Number of Infected Individuals Over Time for All Countries",
         x = "Day of Year",
         y = "Number of Infected Individuals") +
    theme_minimal()
}

#e Compare marker frequencies by Country
compareMarkerFrequencies <- function(filePath, markerPrefix = "marker") {
  # Load the compiled data
  data <- read.csv(filePath, stringsAsFactors = FALSE)

  # Identify marker columns
  markerCols <- grep(markerPrefix, names(data), value = TRUE)

  # Check if country column is present
  if (!"country" %in% names(data)) {
    stop("Country column not found in the data")
  }

  # Calculate frequencies of markers for each country
  frequencies <- lapply(markerCols, function(marker) {
    aggregate(data[[marker]] ~ data$country, data, mean)
  })

  # Rename columns for clarity
  frequencies <- lapply(frequencies, function(df) {
    colnames(df) <- c("Country", "Frequency")
    df
  })

  # Create a combined data frame for plotting
  combinedFrequencies <- do.call(rbind, frequencies)
  combinedFrequencies$Marker <- rep(markerCols, each = nrow(data[data$country == unique(data$country)[1], ]))

  # Plotting frequencies of markers by country
  compareMarkerFrequencies <- function() {
  # Set the file path to "filepath"
  filePath <- "path/to/compiled.csv"  # Update this path to the actual location of the file

  # Load the data
  data <- read.csv(filePath, stringsAsFactors = FALSE)

  # Identify marker columns
  markerCols <- grep("marker", names(data), value = TRUE)

  # Check if country column is present
  if (!"country" %in% names(data)) {
    stop("Country column not found in the data")
  }

  # Reshape data for plotting
  longData <- reshape2::melt(data, id.vars = "country", measure.vars = markerCols)

  # Calculate frequencies of markers for each country
  markerFrequencies <- aggregate(value ~ country + variable, data = longData, mean)

  # Check if ggplot2 package is installed; install it if not
  if (!require("ggplot2", character.only = TRUE)) {
    install.packages("ggplot2")
    library(ggplot2)
  }

  # Plotting frequencies of markers by country
  ggplot(markerFrequencies, aes(x = variable, y = value, fill = country)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = "Marker Frequencies by Country",
         x = "Marker",
         y = "Frequency") +
    theme_minimal()
}

  # Stephan Lukashev
