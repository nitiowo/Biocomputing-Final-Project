#Question 1 - Which country did the disease begin (X/Y)?
# First: Get a summary of total people infected (at least one positive marker)
# We can mark “infected by totaling the markers per patient and see if it ≥0
# Then we create a cumulative_infections function that will output a line graph of infections over time

compiled_data <- read.csv("allData.csv")

calculate_cumulative_infections <- function(compiled_data) {
  result <- data.frame()

  for (country in unique(compiled_data$country)) {
    country_data <- compiled_data[compiled_data$country == country,]
    
    # Create 'infected' column based on markers
    country_data$infected <- rowSums(country_data[, 3:12]) > 0

    for (day in sort(unique(country_data$dayofYear))) {
      day_data <- country_data[country_data$dayofYear <= day,]
      cumulative_infections <- sum(day_data$infected)

      # Verbose Debugging
      print(paste("Country:", country, "Day:", day, "Cumulative infections:", cumulative_infections))

      # Collecting results
      result <- rbind(result, data.frame(country = country, dayofYear = day, cumulative_infections = cumulative_infections))
    }
  }

  return(result)
}
# Apply the function
cumulative_data <- calculate_cumulative_infections(compiled_data)

# Plotting the infections over time using ggplot2
cumulative_infection_plot <- ggplot(cumulative_data, aes(x = dayofYear, y = cumulative_infections, color = country)) +
  geom_line(size=1) +  # Increase line size for visibility
  scale_color_manual(values = c("X" = "red", "Y" = "blue")) +  # Map colors directly to countries
  labs(title = "Cumulative Number of Infections Over Time by Country",
       x = "Day of Year", y = "Cumulative Number of Infections", color = "Country")

print(cumulative_infection_plot)

# Answer 1 -
# Because country X shows infected individuals in the early days of testing, 
# and during that early stage there are 0 people infected in country Y,
# we can conclude that the outbreak began in country X
# not crossing into country Y until about 20 days after (~day 140) it began in X


#Question 2 - If country Y develops a vaccine, is it likely to work for country X?
#First, we'll need to determine the marker frequencies for both countries approaching day 175
#If the markers are similar enough in both countries, we can assume 
# that a vaccined produced in country Y will be affective in country X
# We will need to create barplots for both countries to compare the frequency of markers

library(ggplot2)

# Load the dataset
compiled_data <- read.csv("path/to/allData.csv")

# Subset the data for each country
data_X <- subset(compiled_data, country == "X")
data_Y <- subset(compiled_data, country == "Y")

# Calculate frequencies of markers for each country
freq_X <- colSums(data_X[, 3:12]) / nrow(data_X)
freq_Y <- colSums(data_Y[, 3:12]) / nrow(data_Y)

# Convert to data frames for plotting
freq_X_df <- data.frame(marker = names(freq_X), frequency = freq_X, country = "X")
freq_Y_df <- data.frame(marker = names(freq_Y), frequency = freq_Y, country = "Y")

# Combine data for plotting
combined_freq <- rbind(freq_X_df, freq_Y_df)

# Plot
ggplot(combined_freq, aes(x = marker, y = frequency, fill = country)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Marker Frequency Comparison between Country X and Y",
       x = "Marker", y = "Frequency") +
  scale_fill_manual(values = c("X" = "blue", "Y" = "red"))

# Answer to Question 2
# The data is clear that the distribution markers in country X 
# are too different than in country Y to suggest that a vaccine could work for both. 
# the graph indicates that country X is dealing with a version of the disease that has mainly
# markers 1-5, while country Y has a mutated version with markers 6-10 characteristics.
# By day 175 there would need to be two vaccines in development for both countries
# to cover the variance in both forms of the disease and help these countries.


