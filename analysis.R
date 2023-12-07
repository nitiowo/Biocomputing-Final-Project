# Analysis
# Thomas Joyce and Patrick Kuebler

# Question 1:
# We believe that the disease outbreak likely began in Country X. We were able to create a
# function that shows the cumulative number of infections across time in a line graph
# for each country. This showed us that Country X had cases starting in the first few days,
# while Country Y did not have cases until about day 140. Therefore, it seems like Country X
# was ahead of Country Y in terms of the disease progression, and the outbreak likely began there.
# The cumulative infection data graph is found in our GitHub repository under the name:
# "Cumulative Number of Infections Over Time by Country".

# Question 2:
# If Country Y developed a vaccine, it would likely NOT work for the residents of Country X.
# We came to this conclusion because the microsatellite markers seem to indicate that the strains
# in each country are significantly different. We were able to visualize these differences
# using a bar graph that shows the frequency of each of the 10 microsatellite markers for each
# country. For Country X, markers 1-5 had high counts throughout the outbreak. For Country Y,
# markers 1-5 were mostly absent, and markers were 6-10 more prevalent, especially in the later
# days of the screening. In fact, during the first ~20 days of the infection in Country Y (140-160)
# all markers were present in significant amounts, with 4-7 being the most prevalent. But
# in the later days (160-175), markers 6-10 were much more prevalent than markers 1-5. This shows that
# the pathogen in Country Y was likely diverging significantly away from the pathogen form in
# Country X, so a vaccine for one country would likely not work for the other at this point.
# Our bar graphs for marker frequencies can be found in our GitHub repository. 

# Analysis Code

# Load the functions in supportingFunctions.R
source("C:/Users/thoma/OneDrive/Documents/NOTRE DAME FALL 2023/Intro to Biocomputing/R Project/Biocomputing-Final-Project/supportingFunctions.R")

# Convert all .txt files in the countryY folder to .csv files
txt_to_csv("C:/Users/thoma/OneDrive/Documents/NOTRE DAME FALL 2023/Intro to Biocomputing/R Project/Biocomputing-Final-Project/countryY")

# Compile data from all the .csv files in countryX to a single .csv file
single_csv("C:/Users/thoma/OneDrive/Documents/NOTRE DAME FALL 2023/Intro to Biocomputing/R Project/Biocomputing-Final-Project/countryX")

# Compile data from all the .csv files in countryY to a single .csv file
single_csv("C:/Users/thoma/OneDrive/Documents/NOTRE DAME FALL 2023/Intro to Biocomputing/R Project/Biocomputing-Final-Project/countryY")

# Read in the single .csv file for countryX
countryX_data <- read.csv("C:/Users/thoma/OneDrive/Documents/NOTRE DAME FALL 2023/Intro to Biocomputing/R Project/Biocomputing-Final-Project/countryX/compiled_data.csv")

# Read in the single .csv file for countryY
countryY_data <- read.csv("C:/Users/thoma/OneDrive/Documents/NOTRE DAME FALL 2023/Intro to Biocomputing/R Project/Biocomputing-Final-Project/countryY/compiled_data.csv")

# Merge the data for countryX and countryY into a single data frame called allData
allData <- rbind(countryX_data,countryY_data)

# Summarize the compiled dataset for both countries combined
data_summary(allData)
# Total number of screens run = 39744
# 56.56% of patients screened were infected
# 50.15% of patients identify as male
# 49.85% of patients identify as female
# The age distribution of patients is right-skewed, with most patients <30 years old.

# Outbreak Origin Analysis

# Plot the cumulative number of infections in both countries over time
cumulative_infections(allData) 
# Based on this plot, we can clearly see that the outbreak began in Country X.
# Country Y did not have any infections until around day 140, while Country
# X had infections beginning on day 120.

# Microsatellite Analysis

# Create bar plots for the marker frequencies for each country from days 120-175
marker_frequencies(allData,"X",first_day=120,last_day=175)
marker_frequencies(allData,"Y",first_day=120,last_day=175)
# Based on these two bar plots, it appears that Country X has high frequencies of
# markers 1-5 and low frequencies of markers 6-10, while Country Y has high frequencies
# of markers 6-10 and low frequencies of markers 1-5. To account for the fact that the
# disease-causing bacteria is evolving along its transmission path, we need to look at 
# closer at specific time segments.

# Create a bar plot for the marker frequencies for each country from days 140-160
marker_frequencies(allData,"X",first_day=140,last_day=160)
marker_frequencies(allData,"Y",first_day=140,last_day=160)
# For this time span, Country X mainly has markers 1-5, while Country Y
# has high frequencies for markers 4-7.

# Create bar plots for the marker frequencies for each country from days 160-175
marker_frequencies(allData,"X",first_day=160,last_day=175)
marker_frequencies(allData,"Y",first_day=160,last_day=175)
# In the last 15 days, Country X predominately has markers 1-5, while 
# Country Y mainly has markers 6-10. Hence, if Country Y develops a vaccine 
# for the disease, it is NOT likely to work for Country X.

# To confirm that this is the case, let's just look at the marker
# frequencies for the past 7 days.
marker_frequencies(allData,"X",first_day=168,last_day=175)
marker_frequencies(allData,"Y",first_day=168,last_day=175)
# The same trend holds true. 





