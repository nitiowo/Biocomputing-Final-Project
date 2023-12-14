# Czerwiec, Mae
# Biocomputing Final Project
# "Analysis"

setwd("~/Desktop/Biocomputing/Biocomputing-Final-Project")
source("supportingFunctions.R")
install.packages("ggplot2")
library(ggplot2)

# Step 1 - Convert All Files to .csv
ytxt=list.files("countryY", pattern=".txt")
for(y in 1:length(ytxt)){
  setwd("countryY")
  name<-unlist(strsplit(ytxt[y], ""))
  newname=paste("screen_", name[8], name[9], name[10], ".csv")
  newname=gsub(" ", "", newname)
  filetocsv(ytxt[y], head=TRUE, csvname=newname)
  setwd("..")
}

#Step 2 - Create Datafiles for Country X and Country Y
filecompile("countryX", finalfile="countryXfinal.csv")
filecompile("countryY", finalfile="countryYfinal.csv")

#Step 3 - Determine the country where the outbreak began
# Use a line graph for running trends
Xdata=read.csv("countryXfinal.csv", header=TRUE)
Ydata=read.csv("countryYfinal.csv", header=TRUE)

Xrunning=summarizeData(Xdata, 1)
Yrunning=summarizeData(Ydata, 1)
for(i in seq(1, nrow(Xdata), by=1000)){
  t=summarizeData(Xdata, i)
  print(t)
  Xrunning=rbind(Xrunning, t)
}
for(i in seq(1, nrow(Ydata), by=1000)){
  t=summarizeData(Ydata, i)
  print(t)
  Yrunning=rbind(Yrunning, t)
}
ggplot() +
  geom_line(data=Xrunning, aes(x=numscreens, y=pctinfected), color="darkorchid4") +
  geom_line(data=Yrunning, aes(x=numscreens, y=pctinfected), color="darkorange1") +
  theme_classic()

#These plots demonstrate the percent of tested patients who have the disease over time.
#Since the line for CountryX shoots up immediately and is much higher than the one for Country Y
#It makes sense that the disease began in CountryX, since most of its tests are already positive.
#Country Y begins to see more positive tests later, probably after the disease came over from Country X.
#This analysis could be narrowed down further by focusing on the earliest 1,000 screens for each country,
#which should be easy to do with my orthogonal function summarizeData.

#Step 4 - Would a vaccine developed in Country Y work in Country X?

Xmarkers=data.frame(markers=c(1:10), count=c(sum(Xdata$marker01), sum(Xdata$marker02), sum(Xdata$marker03), sum(Xdata$marker04), sum(Xdata$marker05), sum(Xdata$marker06), sum(Xdata$marker07), sum(Xdata$marker08), sum(Xdata$marker09), sum(Xdata$marker10)))
Ymarkers=data.frame(markers=c(1:10), count=c(sum(Ydata$marker01), sum(Ydata$marker02), sum(Ydata$marker03), sum(Ydata$marker04), sum(Ydata$marker05), sum(Ydata$marker06), sum(Ydata$marker07), sum(Ydata$marker08), sum(Ydata$marker09), sum(Ydata$marker10)))
Xmarkers$country="X"
Ymarkers$country="Y"
markers=rbind(Xmarkers, Ymarkers)
ggplot(markers, aes(markers, count, fill=country)) +
  geom_bar(stat="identity", position="dodge") +
  theme_classic()

#This plot shows the number of each marker found in each country.
#In country X, markers 1-5 prevail on positive tests. In country Y, markers 5-10 are more common.
#Therefore, the disease has mutated traveling between countries X and Y. Therefore,
#a vaccine developed in country Y, targeting markers 5-10, may not work for patients infected in country X.


