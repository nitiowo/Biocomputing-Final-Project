## load functions from supportingFunctions.R
source("supportingFunctions.R")

# run txt_to_csv script with countryY as the directory and space as the delimiter
txt_to_csv("countryY", "space")

# run compile script with countryX as the directory
compile("countryX")

# run summary script with allData.csv as the datafile
summary("allData.csv")

#### question 1: in which country did the disease outbreak begin? ----

# load tidyverse for ggplot
library(tidyverse)
# read in dataset
alldata <- read.csv("allData.csv")
# subset data
positive <- alldata[apply(alldata[, 3:12], 1, sum) > 0, ]
# plot the data of only positive individuals
ggplot(data = positive, aes(x = country, y = dayofYear, color = country)) +
  geom_jitter()

#### question 1 answer ----
# As seen from the graph, the outbreak likely began in country X. 
# Country X has positive individuals starting
# at day 120, the first day of the screening. Country Y has positive individuals starting
# at day 140ish, almost 20 days after country X. This means that the outbreak
# probably started in country X and spread to country Y about 20 days later.

#### question 2: if country Y develops a vaccine, will it work for country X? ----

# what to do:
# see how similar the diseases are based on the microsatellite data

# count the total number of microsatellites per marker per country, save as dataframe
x_markers <- apply(alldata[alldata$country == "X", 3:12], 2, FUN = sum)
y_markers <- apply(alldata[alldata$country == "Y", 3:12], 2, FUN = sum)

# compile x_markers and y_markers into one dataset
# note: I realize this code is not versatile or applicable, but I could not figure
# out a different way to turn the x_markers and y_markers data into a dataframe that
# would be usable in ggplot. The dataframe I was able to make was formatted weirdly
# and wouldn't work, so I didn't include it and did the following instead.
markers <- data.frame(marker = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2, 3, 4, 5, 6,
                                 7, 8, 9, 10),
                      country = c("X", "X", "X", "X", "X", "X", "X", "X", "X", "X",
                                  "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y"),
                      sum = c(6545, 6658, 6549, 6765, 6605, 55, 78, 78, 66, 63,
                              198, 205, 210, 809, 791, 1879, 1905, 1425, 1467, 1439))
# plot the data with the marker on the x axis, sum on y axis, and color by country
ggplot(data = markers, aes(x = marker, y = sum, color = country)) +
  geom_jitter()

#### question 2 answer ----
# If country Y develops a vaccine, it likely will not work for country X. When you
# add up the occurence of each of the 10 markers per country, you see from the graph 
# that country X has a very high number of markers 1-5 and very low number of markers 6-10.
# Country Y has fewer markers 1-5 and a bit higher number of markers 6-10. Because
# the protein for immunological response is coded for in the different markers, it
# is likely that the immune response is different between the two countries.
# As a result, a vaccine from country Y that would accurately target that country's
# strain and specific microsatellite markers would not work for country X because
# of the very different distribution of markers.
