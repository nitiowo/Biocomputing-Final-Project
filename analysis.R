#Analysis.R
# Load supporting functions
source("supportingFunctions.R")

# Specify folder names within the Downloads directory
folderX <- "countryX"
folderY <- "countryY"

# Specify the output CSV file
output_file <- "compiledData.csv"

# Call the function to convert files in "countryY" folder
convertToCSV(folderY)

# Call the function to compile data from both folders
compileData(file.path(Sys.getenv("HOME"), "Downloads", folderX), 
            file.path(Sys.getenv("HOME"), "Downloads", folderY), 
            output_file, 
            removeNA = FALSE, 
            warnNA = TRUE)

# Call the function to summarize the compiled data set
summarize_all_data()

# Question 1
# Make graphs that compare trends in marker counts between country X and country Y for all 10 markers
 # Read the data from the CSV file
   allData <- read.csv("allData.csv", header = TRUE)
 
   # Aggregate data
   agg_data <- aggregate(cbind(marker01, marker02, marker03, marker04, marker05,
                                +                             marker06, marker07, marker08, marker09, marker10) ~ country + dayofYear, data = allData, sum)
 
   # Plotting trends
   ggplot(agg_data, aes(x = dayofYear, y = marker01, group = country, color = country)) +
  +     geom_line() +
  +     labs(title = "Genetic Marker Trends Over Time",
             +          x = "Day of Year",
             +          y = "Marker 01 Count") +
  +     theme_minimal()
 # Read the data from the CSV file
   allData <- read.csv("allData.csv", header = TRUE)
 
   # Aggregate data
   agg_data <- aggregate(cbind(marker01, marker02, marker03, marker04, marker05,
                                +                             marker06, marker07, marker08, marker09, marker10) ~ country + dayofYear, data = allData, sum)
 
   # Plotting trends
   ggplot(agg_data, aes(x = dayofYear, y = marker02, group = country, color = country)) +
  +     geom_line() +
  +     labs(title = "Genetic Marker Trends Over Time",
             +          x = "Day of Year",
             +          y = "Marker 02 Count") +
  +     theme_minimal()
 # Read the data from the CSV file
   allData <- read.csv("allData.csv", header = TRUE)
 
   # Aggregate data
   agg_data <- aggregate(cbind(marker01, marker02, marker03, marker04, marker05,
                                +                             marker06, marker07, marker08, marker09, marker10) ~ country + dayofYear, data = allData, sum)
 
   # Plotting trends
   ggplot(agg_data, aes(x = dayofYear, y = marker03, group = country, color = country)) +
  +     geom_line() +
  +     labs(title = "Genetic Marker Trends Over Time",
             +          x = "Day of Year",
             +          y = "Marker 03 Count") +
  +     theme_minimal()
 # Read the data from the CSV file
   allData <- read.csv("allData.csv", header = TRUE)
 
   # Aggregate data
   agg_data <- aggregate(cbind(marker01, marker02, marker03, marker04, marker05,
                                +                             marker06, marker07, marker08, marker09, marker10) ~ country + dayofYear, data = allData, sum)
 
   # Plotting trends
   ggplot(agg_data, aes(x = dayofYear, y = marker04, group = country, color = country)) +
  +     geom_line() +
  +     labs(title = "Genetic Marker Trends Over Time",
             +          x = "Day of Year",
             +          y = "Marker 04 Count") +
  +     theme_minimal()
 # Read the data from the CSV file
   allData <- read.csv("allData.csv", header = TRUE)
 
   # Aggregate data
   agg_data <- aggregate(cbind(marker01, marker02, marker03, marker04, marker05,
                                +                             marker06, marker07, marker08, marker09, marker10) ~ country + dayofYear, data = allData, sum)
 
   # Plotting trends
   ggplot(agg_data, aes(x = dayofYear, y = marker05, group = country, color = country)) +
  +     geom_line() +
  +     labs(title = "Genetic Marker Trends Over Time",
             +          x = "Day of Year",
             +          y = "Marker 05 Count") +
  +     theme_minimal()
 # Read the data from the CSV file
   allData <- read.csv("allData.csv", header = TRUE)
 
   # Aggregate data
   agg_data <- aggregate(cbind(marker01, marker02, marker03, marker04, marker05,
                                +                             marker06, marker07, marker08, marker09, marker10) ~ country + dayofYear, data = allData, sum)
 
   # Plotting trends
   ggplot(agg_data, aes(x = dayofYear, y = marker06, group = country, color = country)) +
  +     geom_line() +
  +     labs(title = "Genetic Marker Trends Over Time",
             +          x = "Day of Year",
             +          y = "Marker 06 Count") +
  +     theme_minimal()
 # Read the data from the CSV file
   allData <- read.csv("allData.csv", header = TRUE)

   # Aggregate data
   agg_data <- aggregate(cbind(marker01, marker02, marker03, marker04, marker05,
                                +                             marker06, marker07, marker08, marker09, marker10) ~ country + dayofYear, data = allData, sum)
 
   # Plotting trends
   ggplot(agg_data, aes(x = dayofYear, y = marker07, group = country, color = country)) +
  +     geom_line() +
  +     labs(title = "Genetic Marker Trends Over Time",
             +          x = "Day of Year",
             +          y = "Marker 07 Count") +
  +     theme_minimal()
 # Read the data from the CSV file
   allData <- read.csv("allData.csv", header = TRUE)
 
   # Aggregate data
   agg_data <- aggregate(cbind(marker01, marker02, marker03, marker04, marker05,
                                +                             marker06, marker07, marker08, marker09, marker10) ~ country + dayofYear, data = allData, sum)
 
   # Plotting trends
   ggplot(agg_data, aes(x = dayofYear, y = marker08, group = country, color = country)) +
  +     geom_line() +
  +     labs(title = "Genetic Marker Trends Over Time",
             +          x = "Day of Year",
             +          y = "Marker 08 Count") +
  +     theme_minimal()
 # Read the data from the CSV file
   allData <- read.csv("allData.csv", header = TRUE)
 
   # Aggregate data
   agg_data <- aggregate(cbind(marker01, marker02, marker03, marker04, marker05,
                                +                             marker06, marker07, marker08, marker09, marker10) ~ country + dayofYear, data = allData, sum)
 
   # Plotting trends
   ggplot(agg_data, aes(x = dayofYear, y = marker09, group = country, color = country)) +
  +     geom_line() +
  +     labs(title = "Genetic Marker Trends Over Time",
             +          x = "Day of Year",
             +          y = "Marker 09 Count") +
  +     theme_minimal()
 # Read the data from the CSV file
   allData <- read.csv("allData.csv", header = TRUE)
 
   # Aggregate data
   agg_data <- aggregate(cbind(marker01, marker02, marker03, marker04, marker05,
                                +                             marker06, marker07, marker08, marker09, marker10) ~ country + dayofYear, data = allData, sum)
 
   # Plotting trends
   ggplot(agg_data, aes(x = dayofYear, y = marker10, group = country, color = country)) +
  +     geom_line() +
  +     labs(title = "Genetic Marker Trends Over Time",
             +          x = "Day of Year",
             +          y = "Marker 10 Count") +
  +     theme_minimal()

# From these ten graphs, it can be seen that the outbreak began in country X with a prevalence of markers 1-5.

# Question 2      
# Using the same ten graphs, it can be seen that the outbreak in country Y is associated with the presence of markers 6-10, while in country X, the outbreak is associated with the presence of markers 1-5. Therefore, the way the disease progresses in country Y differs from the way it progresses in country X, so a vaccine for the disease in country Y is not likely to work for people in country X.    