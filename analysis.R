# Biergans and Bianchi Final R Project

# Analysis Script

# Identify screens that contain the presence of a marker
marker_cols <- names(combined_data)[grepl("marker", names(combined_data))]

# Add a column to indicate if a row has at least one marker
combined_data$has_marker <- apply(combined_data[, marker_cols], 1, function(x) any(x == 1))

# Add a 'infection_count' column for aggregation purposes
combined_data$case_count <- 1

# Plot 1: Accumulation of Infections
# Filter data to include only rows with at least one marker
filtered_data <- combined_data[combined_data$has_marker, ]

# Aggregate the data to calculate cumulative screenings
accumulated_screenings <- aggregate(case_count ~ dayofYear + Country, data = filtered_data, FUN = sum)
accumulated_screenings$cumulative_screenings <- ave(accumulated_screenings$case_count, accumulated_screenings$Country, FUN = cumsum)

# Split data by country
data_X <- accumulated_screenings[accumulated_screenings$Country == "X", ]
data_Y <- accumulated_screenings[accumulated_screenings$Country == "Y", ]

# Line graph
plot(data_X$dayofYear, data_X$cumulative_screenings, type = "l", col = "blue", xlab = "Day of Year", ylab = "Cumulative Screenings", main = "Cumulative Infections Over Time")
lines(data_Y$dayofYear, data_Y$cumulative_screenings, col = "red")
legend("topright", legend = c("Country X", "Country Y"), col = c("blue", "red"), lty = 1)

# Question 1: We think that country X was the orgin of the disease. We think this based on the information that we gathered from
# the line graph titled "Cumulative Infections Over Time". The graph shows that the infections in country X started on day 120 and country Y's
# started on day 139. Because of this difference of initial infection, we think that country X, the country with the earlier initial infection,
# is the likely origin of the disease.

# Plots 2 and 3

# Define the marker columns for markers 1 to 10
marker_cols <- paste0("marker", sprintf("%02d", 1:10))

# Plot 2: Frequency of Markers in Country X
marker_data_X <- combined_data[combined_data$Country == "X", marker_cols]
marker_sums_X <- colSums(marker_data_X)
barplot(marker_sums_X, 
        main = "Marker Frequency in Country X", 
        xlab = "Marker", 
        ylab = "Frequency", 
        col = "blue", 
        names.arg = 1:length(marker_sums_X))

# Plot 3: Frequency of Markers in Country Y
marker_data_Y <- combined_data[combined_data$Country == "Y", marker_cols]
marker_sums_Y <- colSums(marker_data_Y)
barplot(marker_sums_Y, 
        main = "Marker Frequency in Country Y", 
        xlab = "Marker", 
        ylab = "Frequency", 
        col = "red", 
        names.arg = 1:length(marker_sums_Y))

# Question 2: Our analysis from looking at the frequency of the markers in each country indicate that a vaccine
# developed in country Y does not have a very high chance of being effective for people infected in country X. We do not think the 
# the vaccine would not have a high chance of being effective because there is not much overlap in the presence of markers. In order for 
# the vaccine to be effective it would have to target the markers that have prevalence in both countries. These markers would have to 4 or
# 5 in order for the vaccine to be effective in both countries.
