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