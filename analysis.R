#Analysis.R
# Q1. In which country (X or Y) did the disease outbreak likely begin?
# Disease outbreak likely began in country X.
# According to our graph showing total number of infections in each country by date, 
# infections began in country X on the 120th day of the year, 
# but infections did not begin in country Y until the 139th day of the year. 

#Q2. No, a vaccine development for Country Y would not work for country X.
#According to marker frequency counts for each country, Country X has greater 
#presence of marker 1-5, while y has more of a presence in markers 6-10.


#Set working 
setwd("/Users/izabellalopez/Junior_yr/Biocomputing/Biocomputing-Final-Project")

source("SupportingFunctions.R")

convert_files("countryX")
convert_files("countryY")

combine_csv(c("countryX", "countryY"))

analyze_data('compiled_file.csv')

all.data <- read.csv("compiled_file.csv", header=TRUE)

#The following code is for question 1
# initialize list of length(num_screens) to later store infection status of patient
all.data$infected <- rep(0, num_screens)

# iterate over each row and column of data to determine if marker is present for each screen
num_screens <- nrow(all.data)
for (i in 1:num_screens) {
  for (j in 3:12) {
    if(all.data[i, j] == 1) {
      # store 1 in list to indicate infection in screening
      all.data$infected[i] <- 1
      break}}}

# create a histogram showing frequency of infection per day of year
ggplot(all.data, aes(x = date, y = infected, fill = country)) +
  geom_col() +
  labs(title = "Total Number of Infections in each Country per Day of Year",
       x = "Day of Year",
       y = "Total Number of Infections") +
  theme_minimal()


#The following code below makes the graphs for question 2
data <- read.csv("compiled_file.csv", header=TRUE)
# marker plot for country X
selected_value_x <- "X"
country_column <- "country"

specified_columns<-c("marker01", "marker02", "marker03", "marker04" ,
                     "marker05", "marker06", "marker07", "marker08",
                     "marker09", "marker10")

# Filter the original dataset for the specified value in the country column
filtered_data_x <- data[data[[country_column]] == selected_value_x, specified_columns, drop = FALSE]

column_sums_x <- colSums(filtered_data_x, na.rm = TRUE)

# Create a bar plot
barplot(column_sums_x, 
        main = "Marker frequencies for Country X",
        ylab = "Sum",
        col = "lightblue",
        names.arg = names(column_sums),
        las = 2)


#marker plot for country Y
selected_value_y <- "Y"
country_column <- "country"

specified_columns<-c("marker01", "marker02", "marker03", "marker04" ,
                     "marker05", "marker06", "marker07", "marker08",
                     "marker09", "marker10")

# Filter the original dataset for the specified value in the country column
filtered_data_y <- data[data[[country_column]] == selected_value_y, specified_columns, drop = FALSE]

column_sums_y <- colSums(filtered_data_y, na.rm = TRUE)

# Create a bar plot
barplot(column_sums_y, 
        main = "Marker frequencies for Country Y",
        ylab = "Sum",
        col = "lightgreen", 
        names.arg = names(column_sums),
        las = 2)


