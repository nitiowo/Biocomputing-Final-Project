#Final Biocomputing Project Submission for Daniel Gatewood
rm(list=ls())

allData<-read.csv("/users/daniel/desktop/Biocomputing-Final-Project/allData.csv")
source("/Users/daniel/Desktop/Biocomputing-Final-Project/SupportingFinalProj.R")
csvconvert("/Users/daniel/Desktop/Biocomputing-Final-Project/countryY",TRUE," ")
datacompile("/Users/daniel/Desktop/Biocomputing-Final-Project",c("countryX","countryY"),NAs=1)
summarize("/Users/daniel/Desktop/Biocomputing-Final-Project/CompiledData.csv")

Data<-read.csv("/Users/daniel/Desktop/Biocomputing-Final-Project/CompiledData.csv",header=TRUE)
#Creates a column of the number of markers detected per patient
Data$NumberMarkers<-Data$marker01+Data$marker02+Data$marker03+Data$marker04+Data$marker05+Data$marker06+
  Data$marker07+Data$marker08+Data$marker09+Data$marker10

#This creates new data frames where each row is a day in a specific country and it lists how many infections were detected on each day
firstday<-min(Data[,"dayofYear"])
lastday<-max(Data[,"dayofYear"])
InfectionsX<-data.frame()
InfectionsY<-data.frame()
for (i in firstday:lastday){
  number<-nrow(Data[(Data$country=="countryX")&(Data$dayofYear==i)&(Data$NumberMarkers>0),])
  tempdf<-data.frame("day"=i,"country"="countryX","infections"=number)
  InfectionsX<-rbind(InfectionsX,tempdf)
}
for (i in firstday:lastday){
  number<-nrow(Data[(Data$country=="countryY")&(Data$dayofYear==i)&(Data$NumberMarkers>0),])
  tempdf<-data.frame("day"=i,"country"="countryY","infections"=number)
  InfectionsY<-rbind(InfectionsY,tempdf)
}

#The next 2 for loops update the Infections data frames so that they list the total number of cases detected per day since the beginning of the pandemic
for( i in 2:nrow(InfectionsX)){
  InfectionsX[i,3]<-InfectionsX[i-1,3]+InfectionsX[i,3]
}
for( i in 2:nrow(InfectionsY)){
  InfectionsY[i,3]<-InfectionsY[i-1,3]+InfectionsY[i,3]
}
#Plots for both countries the number of total infections in those countries since the beginning of the pandemic by day
ggplot()+
  geom_step(mapping=aes(y=infections,x=day),colour="red",data=InfectionsX)+
  geom_step(mapping=aes(y=infections,x=day),colour="darkgreen",data=InfectionsY)+
  theme_classic()
###Country X has many more infections prior to the beginning of the infections in Country Y
#Country X has infections on day 120 but Country Y doesn't beging having infections until day 140

#I will now make a similar graph which tracks how many instances of infection are detected per day by each of the 10 markers in each country
Marker01Y<-data.frame()
Marker02Y<-data.frame()
Marker03Y<-data.frame()
Marker04Y<-data.frame()
Marker05Y<-data.frame()
Marker06Y<-data.frame()
Marker07Y<-data.frame()
Marker08Y<-data.frame()
Marker09Y<-data.frame()
Marker10Y<-data.frame()
Marker01X<-data.frame()
Marker02X<-data.frame()
Marker03X<-data.frame()
Marker04X<-data.frame()
Marker05X<-data.frame()
Marker06X<-data.frame()
Marker07X<-data.frame()
Marker08X<-data.frame()
Marker09X<-data.frame()
Marker10X<-data.frame()

for (i in firstday:lastday){
  number<-nrow(Data[(Data$country=="countryY")&(Data$dayofYear==i)&(Data$marker01==1),])
  tempdf<-data.frame("day"=i,"country"="countryY","marker"=number)
  Marker01Y<-rbind(Marker01Y,tempdf)
  
  number<-nrow(Data[(Data$country=="countryY")&(Data$dayofYear==i)&(Data$marker02==1),])
  tempdf<-data.frame("day"=i,"country"="countryY","marker"=number)
  Marker02Y<-rbind(Marker02Y,tempdf)

  number<-nrow(Data[(Data$country=="countryY")&(Data$dayofYear==i)&(Data$marker03==1),])
  tempdf<-data.frame("day"=i,"country"="countryY","marker"=number)
  Marker03Y<-rbind(Marker03Y,tempdf)

  number<-nrow(Data[(Data$country=="countryY")&(Data$dayofYear==i)&(Data$marker04==1),])
  tempdf<-data.frame("day"=i,"country"="countryY","marker"=number)
  Marker04Y<-rbind(Marker04Y,tempdf)

  number<-nrow(Data[(Data$country=="countryY")&(Data$dayofYear==i)&(Data$marker05==1),])
  tempdf<-data.frame("day"=i,"country"="countryY","marker"=number)
  Marker05Y<-rbind(Marker05Y,tempdf)

  number<-nrow(Data[(Data$country=="countryY")&(Data$dayofYear==i)&(Data$marker06==1),])
  tempdf<-data.frame("day"=i,"country"="countryY","marker"=number)
  Marker06Y<-rbind(Marker06Y,tempdf)

  number<-nrow(Data[(Data$country=="countryY")&(Data$dayofYear==i)&(Data$marker07==1),])
  tempdf<-data.frame("day"=i,"country"="countryY","marker"=number)
  Marker07Y<-rbind(Marker07Y,tempdf)

  number<-nrow(Data[(Data$country=="countryY")&(Data$dayofYear==i)&(Data$marker08==1),])
  tempdf<-data.frame("day"=i,"country"="countryY","marker"=number)
  Marker08Y<-rbind(Marker08Y,tempdf)

  number<-nrow(Data[(Data$country=="countryY")&(Data$dayofYear==i)&(Data$marker09==1),])
  tempdf<-data.frame("day"=i,"country"="countryY","marker"=number)
  Marker09Y<-rbind(Marker09Y,tempdf)

  number<-nrow(Data[(Data$country=="countryY")&(Data$dayofYear==i)&(Data$marker10==1),])
  tempdf<-data.frame("day"=i,"country"="countryY","marker"=number)
  Marker10Y<-rbind(Marker10Y,tempdf)
  
  number<-nrow(Data[(Data$country=="countryX")&(Data$dayofYear==i)&(Data$marker01==1),])
  tempdf<-data.frame("day"=i,"country"="countryX","marker"=number)
  Marker01X<-rbind(Marker01X,tempdf)
  
  number<-nrow(Data[(Data$country=="countryX")&(Data$dayofYear==i)&(Data$marker02==1),])
  tempdf<-data.frame("day"=i,"country"="countryX","marker"=number)
  Marker02X<-rbind(Marker02X,tempdf)
  
  number<-nrow(Data[(Data$country=="countryX")&(Data$dayofYear==i)&(Data$marker03==1),])
  tempdf<-data.frame("day"=i,"country"="countryX","marker"=number)
  Marker03X<-rbind(Marker03X,tempdf)
  
  number<-nrow(Data[(Data$country=="countryX")&(Data$dayofYear==i)&(Data$marker04==1),])
  tempdf<-data.frame("day"=i,"country"="countryX","marker"=number)
  Marker04X<-rbind(Marker04X,tempdf)
  
  number<-nrow(Data[(Data$country=="countryX")&(Data$dayofYear==i)&(Data$marker05==1),])
  tempdf<-data.frame("day"=i,"country"="countryX","marker"=number)
  Marker05X<-rbind(Marker05X,tempdf)
  
  number<-nrow(Data[(Data$country=="countryX")&(Data$dayofYear==i)&(Data$marker06==1),])
  tempdf<-data.frame("day"=i,"country"="countryX","marker"=number)
  Marker06X<-rbind(Marker06X,tempdf)
  
  number<-nrow(Data[(Data$country=="countryX")&(Data$dayofYear==i)&(Data$marker07==1),])
  tempdf<-data.frame("day"=i,"country"="countryX","marker"=number)
  Marker07X<-rbind(Marker07X,tempdf)
  
  number<-nrow(Data[(Data$country=="countryX")&(Data$dayofYear==i)&(Data$marker08==1),])
  tempdf<-data.frame("day"=i,"country"="countryX","marker"=number)
  Marker08X<-rbind(Marker08X,tempdf)
  
  number<-nrow(Data[(Data$country=="countryX")&(Data$dayofYear==i)&(Data$marker09==1),])
  tempdf<-data.frame("day"=i,"country"="countryX","marker"=number)
  Marker09X<-rbind(Marker09X,tempdf)
  
  number<-nrow(Data[(Data$country=="countryX")&(Data$dayofYear==i)&(Data$marker10==1),])
  tempdf<-data.frame("day"=i,"country"="countryX","marker"=number)
  Marker10X<-rbind(Marker10X,tempdf)
}
  
ggplot()+
  geom_step(mapping=aes(y=marker,x=day),colour="red",data=Marker01Y)+
  geom_step(mapping=aes(y=marker,x=day),colour="orange",data=Marker02Y)+
  geom_step(mapping=aes(y=marker,x=day),colour="yellow",data=Marker03Y)+
  geom_step(mapping=aes(y=marker,x=day),colour="green",data=Marker04Y)+
  geom_step(mapping=aes(y=marker,x=day),colour="darkgreen",data=Marker05Y)+
  geom_step(mapping=aes(y=marker,x=day),colour="aquamarine",data=Marker06Y)+
  geom_step(mapping=aes(y=marker,x=day),colour="blue",data=Marker07Y)+
  geom_step(mapping=aes(y=marker,x=day),colour="violet",data=Marker08Y)+
  geom_step(mapping=aes(y=marker,x=day),colour="purple",data=Marker09Y)+
  geom_step(mapping=aes(y=marker,x=day),colour="pink",data=Marker10Y)+
  theme_classic()
###This reveals that indeed the virus is evolving because the number of new infections by marker detected changes over time.
#On day 175 countryY has many more infections by markers 6-10
ggplot()+
  geom_step(mapping=aes(y=marker,x=day),colour="red",data=Marker01X)+
  geom_step(mapping=aes(y=marker,x=day),colour="orange",data=Marker02X)+
  geom_step(mapping=aes(y=marker,x=day),colour="yellow",data=Marker03X)+
  geom_step(mapping=aes(y=marker,x=day),colour="green",data=Marker04X)+
  geom_step(mapping=aes(y=marker,x=day),colour="darkgreen",data=Marker05X)+
  geom_step(mapping=aes(y=marker,x=day),colour="aquamarine",data=Marker06X)+
  geom_step(mapping=aes(y=marker,x=day),colour="blue",data=Marker07X)+
  geom_step(mapping=aes(y=marker,x=day),colour="violet",data=Marker08X)+
  geom_step(mapping=aes(y=marker,x=day),colour="purple",data=Marker09X)+
  geom_step(mapping=aes(y=marker,x=day),colour="pink",data=Marker10X)+
  theme_classic()
#The virus seems to be evolving less rapidly in countryX because the markers 
#that are responsible for new infections is not changing.

#On day 175 countryX has many more infections by markers 1-5
#Therefore the microbes causing the illness in countryX are likely significantly 
#genetically different from that in countryY because they are causing different
#markers to appear

#Therefore it is likely that a vaccine or treatment developed in countryY is 
#likely to not be effective in countryX





