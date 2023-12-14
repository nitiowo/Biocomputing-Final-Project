##Analysis.R

source("SupportingFunctions.R")


## graphical analysis evidence for the answers to 1 and 2: (analysis.R)

##To answer question 1:  In which (X or Y) did the disease outbreak likely begin? 

# Read the data from the combined file
data <- read.csv("alldata.csv")

# Find number of patients infected each day
data$InfectedCount <- rowSums(data[, 3:12] == 1) > 0

for(i in 1:nrow(data)){
  if(sum(data[i,3:12]) > 0){
    data$InfectedCount[i] <- 1
  }else{
    data$InfectedCount[i] <- 0
  }
}

# Separate the data by Country X or Y and by the day of year the patient is infected
sorted_data <- data[order(data$country, data$dayofYear), ]

# Calculate cumulative sum of patients infected for Country X and Country Y
sorted_data$CumulativeInfected <- unlist(lapply(split(sorted_data$InfectedCount, sorted_data$country), cumsum))

# Create cumulative sum of patients infected for both countries
lengX <- as.numeric(length(which(sorted_data$country == 'X')))
lengY <- lengX+1
end <- as.numeric(nrow(sorted_data))
#cumulative sum of X
cumsumX <- cumsum(sorted_data[1:lengX,15])
cumsumY <- cumsum(sorted_data[lengY:end,15])
sorted_data$CumulativeCount <- c(cumsumX,cumsumY)


# Plotting a line graph comparing each Country's cumulative outbreak numbers
library(ggplot2)

ggplot(sorted_data, aes(x = dayofYear, y = CumulativeInfected, color = country, group = country)) +
  geom_line(size=1) +
  labs(x = "Day", y = "Cumulative Infected Patients",
       title = "Cumulative Disease Outbreak Comparison") +
  theme_minimal() +
  scale_color_manual(values = c("darkgreen", "red"))

# to fully show the answer to question 1: 
#this graph shows when the outbreak occurred for each country. 
#The green line indicates that Country X had a more exponential growth at the beginning than Country Y (the red line). 
#Thus, the outbreak most likely began in country X because there are no recorded cases of the disease in country Y until day 139.

# to answer question 2: If Country Y develops a vaccine for the disease, is it likely to work for citizens of Country X?
## checking to see if the markers expressed are the same between the two countries
dat2 <- read.csv("allData.csv")
X <- dat2[dat2$country=="X", 3:12]
Y <- dat2[dat2$country=="Y", 3:12]
colSums(X)
colSums(Y)

## checking to see markers (graphically)

#creating a new data frame to show the country, marker, and each number of counts
marker <- c(rep("marker01"), rep("marker02"), rep("marker03"), rep("marker04"), rep("marker05"), rep("marker06"), rep("marker07"), rep("marker08"), rep("marker09"), rep("marker10"))
country <- c(rep("countryX", 10), rep("CountryY", 10))
count <- c(rep(colSums(X)), rep(colSums(Y)))
frame <- data.frame(country, count, marker)
frame

ggplot(frame, aes(fill=country, y=count, x=marker)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Marker Number", y = "Cumulative Markers Present",
       title = "Total Genetic Markers Identified") +
  theme_minimal()

#This code produces a table output for the count of markers for each country. 
#When you look at the tables, it is clear that different markers are more prevalent in each country. 
#This is confirmed by the graph: different markers correspond with the populations of different countries.
#Markers 1-5 have high counts in Country X, and Markers 6-10 have high counts in Country Y. 
#This evidence makes it doubtful that a vaccine developed for one country could work for the other country. 
#This is because the genetic markers that the disease targets are different for the different populations. 

