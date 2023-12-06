# Supporting Functions for the analysis of the country disease outbreak R Project
# Justin Detweiler, Qiushi Peng
# Introduction to Biocomputing


#The function will convert all .txt files in a directory to .csv files
TxtToCsv=function(directory){
# Make a object with all of the files that are .txt and not currently 
  # comma delimited
countrycsv<-list.files(directory, pattern = ".txt")
#Set working directory to where you would like to place the new .csv files
setwd(directory)
#For loop to change each .txt file to a .csv file
  for (i in 1:length(countrycsv)){
    FILE<-read.table(file=countrycsv[i],header=TRUE,sep="")
    write.table(FILE,file=paste0(sub(".txt","",countrycsv[i]),".csv"),
    row.names=F,quote=F,sep=",")
  }
}


# Function  that combines the data from CountryX and CountryY into a large 
# .csv file and allows the user to decide the purpose of the NA rows
Combined_CSV=function(directory1, directory2, directory3){
  setwd(directory1)
  All_DataX=data.frame(gender=character(), age=numeric(), marker01=numeric(), 
  marker02=numeric(), marker03=numeric(), marker04=numeric(), marker05=numeric(), 
  marker06=numeric(), marker07=numeric(), marker08=numeric(), marker09=numeric(), 
  marker10=numeric(), country=character(), dayofYear=numeric())
  #Adding X Files to the DF
  listfilesX=list.files(pattern=".csv")
  #For Loop to read through each X file
  for (i in 1:length(listfilesX)){
    X=read.csv(listfilesX[i])
    #Add the country to each X row
    X$country="X"
    #Add the day of year to each row
    X$dayofYear=as.numeric(substr(listfilesX[i], 8, 10))
    All_DataX = rbind(All_DataX, X)
  }
  #Creating the Large .csv with all Y data from all screens
  setwd(directory2)
  #Creating a DF for all of the data
  All_DataY=data.frame(gender=character(), age=numeric(), marker01=numeric(), 
  marker02=numeric(), marker03=numeric(), marker04=numeric(), marker05=numeric(),
  marker06=numeric(), marker07=numeric(), marker08=numeric(), marker09=numeric(), 
  marker10=numeric(), country=character(), dayofYear=numeric())
  #Adding X Files to the DF
  listfilesY=list.files(pattern=".csv")
  #For loop to read through each Y file
  for (i in 1:length(listfilesY)){
    Y=read.csv(listfilesY[i])
    #Add the country to each Y row
    Y$country="Y"
    #Add the day of year to each row
    Y$dayofYear=as.numeric(substr(listfilesY[i], 8, 10))
    All_DataY = rbind(All_DataY, Y)
  }
  All_Data_Both=rbind(All_DataX, All_DataY)
  setwd(directory3)
  #Allow the user to decide what to do with NA rows
  print("What would you like to do with variables with NA in dataset?")
  print("Type 1 to remove NA rows")
  print("Type 2 to keep NA rows but display a warning")
  print("Type 3 to keep the rows with NA without warning")
  Number_Chosen=readline(prompt= "Type Number: ")
  #For loop to bind data if user chooses to do nothing with the NA
  if (Number_Chosen==3){
    All_Data_Both= rbind(All_DataX, All_DataY)
    write.csv(All_Data_Both, file= "All_Data_Both.csv", row.names = FALSE)
  }
  #For loop to print a warning if the user chooses to
  if (Number_Chosen==2){
    All_Data_Both= rbind(All_DataX, All_DataY)
    write.csv(All_Data_Both, file= "All_Data_Both.csv", row.names = FALSE)
    print("Warning: Rows with no data are present")
  }
  #For loop to remove the NA rows if the user chooses to
  if (Number_Chosen==1){
    All_Data_Both= rbind(All_DataX, All_DataY)
    All_Data_Both=na.omit(All_Data_Both)
    write.csv(All_Data_Both, file= "All_Data_Both.csv", row.names = FALSE)
  }
}

#Data Summary Function: This function will give the Total patients, 
# total male patients, total female patients, the percentage infected, 
# and a graph of the age distribution of patients
Data_Summary=function(file){
  #Remove all People over the age of 120 as that is likely impossible 
  file=file[!(file$age>120),]
  #To count the total number of screens run
  Total_Screens=nrow(file)
  #To count the number of screens for each country
  TotalX_Screens=nrow(subset(file, file$country=="X"))
  TotalY_Screens=nrow(subset(file, file$country=="Y"))
  #Number of Males Screened
  Male_Patients=nrow(subset(file, file$gender=="male"))
  #Number of Female Patients
  Female_Patients=nrow(subset(file, file$gender=="female"))
  #Count the Percentage screened that were infected
  total_infected=0
  for (i in 1:nrow(file)){
    Marker_Sum=sum(file[i,3:12])
    if(Marker_Sum>=1){
      total_infected=total_infected+1
    }
  }
  Percentage_Infected=total_infected/Total_Screens
  Age_Distribution_Plot=ggplot(allData, aes(x=age))+
    geom_histogram(binwidth=1)+
    theme_classic()+ggtitle("Age Distribution of Patients")+
    xlab("Age")+ylab("Count")
  
  print(paste("The Total Screens taken was", Total_Screens))
  print(paste("The total females screened was", Female_Patients))
  print(paste("The total males screened was", Male_Patients))
  print(paste("The total screens infected was", total_infected))
  print(paste("The percentage of screens infected was", Percentage_Infected))
  print(Age_Distribution_Plot)
}



#Function that answers question 2
#Create Data Frame with Data to answer question 2
#Create a function that looks at the given file with all of the country data
Vaccine_Question=function(file){
  #Create a data frame that will hold the data for each country by marker, with
  # a row for each marker in each country
  Marker_Counts=data.frame(matrix(data=NA, nrow=20, ncol=3))
  #Vector to be inserted as the column names in the data frame
  vec1=c("Marker", "Count", "Country")
  #Insert the vector as the column names
  colnames(Marker_Counts)=vec1
  #For loop to sum the patients in country X that were positive for each marker
  for (i in 3:12){
    Marker_Counts[i-2,2]=sum(file[which(file$country=="X"),i])
  }
  #For loop to sum the patients in country Y that were positive for each marker
  for (i in 3:12){
    Marker_Counts[i+8,2]=sum(file[which(file$country=="Y"),i])
  }
  #Insert a marker column with the labels for each
  Marker_Counts[,1]=rep(colnames(file[,3:12,2]))
  #Make the third row of the data frame have country X for the first 10 markers
  Marker_Counts[1:10,3]="X"
  #Make the third row of the data frame have country Y for the second set of 10 markers
  Marker_Counts[11:20,3]="Y"
  #Use GGplot to make a graph of the data frame
  ggplot(data=Marker_Counts, aes(x=Marker, y=Count, fill=Country))+
    geom_bar(stat="identity")+
    ggtitle("Markers Found in Each Country")+
    xlab("Marker Numbers")+ ylab("Number of Patients")+theme_classic()
}



