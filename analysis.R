# Load in the functions
source("supportingFunctions.R")

# Call the function to convert files to CSV
ConvertToCSV("C:/Users/nbark/Documents/Rproject/countryY")

# Call the function to compile the dataset and add a row for country and day of the year
CompileAndWriteCSV("C:/Users/nbark/Documents/Rproject", "compiled_data.csv", NA_option = "keep" )

# Summarize statistics from the compiled data 
SummarizeCompiledData("compiled_data.csv")

# Read in compiled data for analysis
data <- read.csv("compiled_data.csv")

# Load required packages
library(ggplot2)
library(dplyr)

# Make a new column that interprets the marker data and spits out an indication of the infection status of each individual
for (i in 1:length(data$gender)) {
  if (sum(data[i, 3:12]) > 0) {
    data$infection_status[i] = 1
  } else {
    data$infection_status[i] = 0
  }
}


# Get summary statistics for each country by day
epidemicdata <- data %>%
  group_by(country, dayofYear) %>%
  summarise(incidence = sum(infection_status))

###Summary Statistics: 
#Number of screens run: 39744 
#Percentage of patients screened that were infected: 16.96608 %
#Percentage of patients identifying as male: 50.15348 %
#Percentage of patients identifying as female: 49.84652 %
#Age distribution of patients: 
#  1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21   22   23   24   25   26   27   28   29   30   31   32   33   34   35   36   37   38   39 
#943 2880 3472 3528 3077 2759 2464 2005 1879 1604 1443 1286 1143  991  902  750  659  611  572  531  411  418  408  366  290  317  275  239  234  208  180  188  156  144  133  125  132  123   98 
#40   41   42   43   44   45   46   47   48   49   50   51   52   53   54   55   56   57   58   59   60   61   62   63   64   65   66   67   68   69   70   71   72   73   74   75   76   77   78 
#105  111   76   75   62   72   44   57   73   60   56   49   51   39   47   43   45   29   36   43   29   30   25   27   22   36   24   14   29   17   20   16   19   11   20   14   11   14   17 
#79   80   81   82   83   84   85   86   87   88   89   90   91   92   93   94   95   96   97   98   99  100  101  102  103  104  105  106  107  108  109 
#11    7   12   13   14   12    9    5   14   15    6    7   12   12    5    4    9    4    7    8    6    6    4    9    2    2    2    4    3    5    3 

# Plot data for question 1
ggplot(epidemicdata, aes(x=dayofYear, y=incidence)) +
  geom_line(aes(color = country, linetype = country)) +
  labs(x ="Day", y = "Disease Incidence") +
  scale_color_manual(values = c("darkred", "steelblue")) +
  theme_bw()

# Get summary statistics for each marker by country
MarkerSum <- data %>%
  group_by(country) %>%
  summarize(
    Count = c(sum(marker01), sum(marker02), sum(marker03), sum(marker04), sum(marker05),
              sum(marker06), sum(marker07), sum(marker08), sum(marker09)),
  )

# Add marker number to summary statistics
Marker <- data.frame(Marker = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9))

MarkerData <- cbind(MarkerSum, Marker)

# Plot data for question 2
ggplot(data=MarkerData, aes(x=Marker, y=Count, fill=country)) +
  geom_bar(stat="identity")+
  scale_x_continuous(breaks = 1:9)+
  theme_bw()

# Question 1: The disease likely originated in country X, because disease incidence appeared first in country 
#X, and then only after 20 days did people in country y start being infected. Based on the available data, 
#it appears that the disease is going to stay in each country for the time being, as both countries are at an
#all time high incidence at the ends of the available data period.

# Question 2: A vaccine that works for country Y is not likely to work for country X. This is because
#the prevalence of disease markers between the two countries is highly varied, with country X having
#a large portion of their population with markers 1-5, and country Y where there is a large portion
#of the population with markers 6-9. Because differences in the markers present indicates differences
#in the proteins within the disease, as well as the immune response to the presence of the disease,
#an infection with different markers will likely require seperate vacinations.

