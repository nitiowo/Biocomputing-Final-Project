source('supportingFunctions.R')

#Question 1: Which country did the outbreak start in?
# Read data in
data <- read.csv("alldata.csv")

# Find number of infected patients
for(i in 1:nrow(data)){
  if(sum(data[i,3:12]) > 0){
    data$InfectedCount[i] <- 1
  }else{
    data$InfectedCount[i] <- 0
  }
}

# Sort the dataset by country and day
sorted_data <- data[order(data$country, data$dayofYear), ]

# Create cumulative sum of patients infected for both countries
##create parameters for the dataset to be cumulatively summed by
lengX <- as.numeric(length(which(sorted_data$country == 'X')))
lengY <- lengX+1
end <- as.numeric(nrow(sorted_data))
##cumulative sum of X and Y
cumsumX <- cumsum(sorted_data[1:lengX,15])
cumsumY <- cumsum(sorted_data[lengY:end,15])

#add the cumulative sums back to the sorted_data df
sorted_data$CumulativeCount <- c(cumsumX,cumsumY)

# Plot a line graph comparing each country's outbreaks
library(ggplot2)
ggplot(sorted_data, aes(x = dayofYear, 
                        y = CumulativeCount, 
                        color = country, 
                        group = country)) +
  geom_line() +
  xlab('Screening Day')+
  ylab('Cumulative Infected')+
  ggtitle('Cumulative Disease Outbreak Comparison Between Countries X and Y')+
  scale_color_manual(values = c("blue", "red"))
#Answer 1: 
##The disease outbreak seems to have began in Country X.
##As can be seen in the graph displayed by the code above, the incidence of cases begins and continues to rise in country X (seen in blue) from day 120 on. 
##Country Y (seen in red) doesn't begin to see cases until around the 140th day, meaning that their first cases were most likely acquired from country X, assuming that this is an emerging disease that was first seen globally in country X.
##Thus, it can be reasonably concluded that the disease outbreak began in country X.

#Question 2: Would a vaccine developed in country Y be able to help those in country X?
#read in data
data_q_2 <- read.csv('allData.csv')
#store just the markers for each country in a variable
x <- data_q_2[data_q_2$country=='X',3:12]
y <- data_q_2[data_q_2$country=='Y',3:12]
#get a summary of these markers, store in a new variable, combine into a df
sumx <- as.numeric(colSums(x))
sumy <- as.numeric(colSums(y))
markers <- c('Marker1','Marker2','Marker3','Marker4','Marker5','Marker6','Marker7','Marker8','Marker9','Marker10')
prex <- cbind(sumx,'X',markers)
prey <- cbind(sumy,'Y',markers)
pre <- rbind(prex,prey)
df <- data.frame(pre)
colnames(df) <- c('Occurrence','Country','Markers')
df$Occurrence <- as.numeric(df$Occurrence)
#create a bar graph to view the df
library(ggplot2)
ggplot(df, aes(Country,Occurrence)) +
  geom_bar(stat='identity') + 
  xlab('Markers Present')+
  ylab('Occurrence of Markers')+
  ggtitle('Frequency of Disease Markers by Country')+
  facet_grid(.~factor(Markers,levels=markers),scales='free')
#Answer 2: 
##The vaccine developed in country Y would most likely not be very effective in country X.
##As can be seen in the graph displayed by the code above, the two countries have vastly different prevalences of markers.
##Since the markers are the different proteins displayed by the disease, this means that the proteins of the disease itself differs between the nations.
##Because of this difference in the targeting protein, the vaccine for one country's strain of the disease would not offer the best protection for the strain from the other country.
