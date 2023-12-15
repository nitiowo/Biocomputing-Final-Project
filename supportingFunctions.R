#Kate Jackowski and James Magas
#2023-Dec-14
#Biocomputing Final Project
#Script: supportingFunctions.R


#1ST REQUIREMENT
#function that converts all files in a directory with space or tab delimited data into comma-separated value files
###because country X and country Y use different delimiters in their files
####default for read.table() is sep = " " which is one or more spaces, tabs, newlines, or carriage returns
####need to use paste and variables to create new file name for write.table() to put new file into
####custom function needs to loop through all files in a directory using for loop
####start in directory containing files to convert
  
#####custom function that converts a given file to a .csv file
convert_to_csv<-function(filename){
  ogfile<-read.table(file=filename,header=TRUE,stringsAsFactors=FALSE)
  filenamevector<-paste(filename)
  csvname<-strsplit(filenamevector,".txt")
  csvfilename<-paste(csvname, "csv", sep=".")
  write.table(ogfile,file=csvfilename,sep=",",row.names = FALSE,col.names=TRUE)
  return(csvfilename)
}


######use custom function above to create custom function that loops through all files in a directory to convert all of them to .csv files
convertdirectorytocsv<-function(directory){
  setwd(directory)
  files<-list.files(directory)
  filestoconvert<-data.frame(filename=files)
  for(i in 1:nrow(filestoconvert)){
    convert_to_csv(filestoconvert$filename[i])
  }
}


#-----------------------------------------------------------------------------------------------------------------
#2ND REQUIREMENT

#function to compile data from all .csv files in a directory into a single .csv file
### final file should have original 12 columns and country and dayofYear columns
###user can choose to remove any row with an NA in it by typing in "remove" as the option in this function
###user can choose to display warning if there are any NAs in data by typing in "warn" as the option in this function

many2onecsv<-function(directory, NAdecision = "do_nothing"){
  #set working directory as directory you are now using
  setwd(directory)
  ####create list of all csv files in directory
  csvfiles<-list.files(directory,pattern = "*.csv")
  ####create dataframe with that list of all files in directory
  csvlist<-data.frame(filename=csvfiles)
  
  ####go through all files and put information into new csv file called all.csv
  for(i in 1:nrow(csvlist)){
    #bind data from new list to old one
    screeningdata<-read.csv(csvlist$filename[i],header=TRUE,stringsAsFactors = FALSE)
    #replace day of year with day of year from name of file
    csvname<-strsplit(csvlist$filename[i],".csv")
    namesplit<-strsplit(csvname[[1]],"screen_")
    day<-namesplit[[1]][2]
    #create variable to store country name from directory name
    directoryname<-directory
    directorynamepieces<-strsplit(directoryname,"/")
    #find out how many pieces there are to user's directory
    lastnum<-length(directorynamepieces[[1]])
    letter<-strsplit(directorynamepieces[[1]][lastnum],"country")
    #add necessary columns to data
    data_with_all_columns<-cbind(screeningdata,Country=letter[[1]][2],dayofYear=day)
    write.table(data_with_all_columns,file="C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project/all.csv",append=TRUE,sep=",",row.names = FALSE,col.names=!file.exists("C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project/all.csv"))
    
    #column name issue solved by excluding column names from being written if file already exists!
  }
  #I will now set the working directory so that I can use the newly created all.csv file
  setwd('..')

  #if the option to check for NA is designated by user I can loop through and perform needed actions
  #if default is selected i will skip this part

  
    if (NAdecision == "remove"){
      #since user designated to perform action for occurances of NA I will loop through all of the columns and rows
      #and read the newly created all.csv file table
      #then convert newly created table to dataframe
      #and process data using for loop
      #then I will convert processed data frame back to table / csv file
      update <- read.csv("all.csv")
      #This will remove all rows with NA present
      na.omit(update)
      #once done write it to a table / csv file
      all.csv <- write.table(update)
      #and now i am back to where i started before manipulating all.csv
    }
    if (NAdecision == "warn"){
      #since user designated to perform action for occurances of NA I will loop through all of the columns and rows
      #and read the newly created all.csv file table
      #then convert newly created table to dataframe
      #and process data using for loop
      #then I will convert processed data frame back to table / csv file
      update <- read.csv("all.csv")
    #include NA but be warned of their presence
    for(i in 1:nrow(update)) {
      for(j in 1:ncol(update)) {
        if (is.na(update[i,j]) == TRUE){
          cat("*Warning*: NA value present in data at [", i, ",", j, "]\n")
        }
      }
    }
      #once done write it to a table / csv file
      all.csv <- write.table(update)
      #and now i am back to where i started before manipulating all.csv
  }
}


#-----------------------------------

#3RD REQUIREMENT
#write a function to summarize the compiled data set in terms of number
#screens run, percent of patients screened that were infected
#percent of patients that identify as male and female
#age distribution of patients (as a table or plot)


#Using all.csv file to accomplish these tasks


#need number of screens run
#percent of patients screened that were infected
#percent of patients that identify as male and female
#age distribution of patients as table or plot


#User will select the summarized data filename (all.csv), they will need to have their working directory set to the directory with this file
sumview<-function(filename){
  summary<-read.csv(filename)
  head(summary)
  tail(summary)

  #Number of screens per run
  total_screens = nrow(summary)
  #create a variable for tracking number of positive cases
  totalpos = 0
  #code will loop through each row, but only columns that contain markers
  #if one of the rows markers contains a 1 it will increment variable and move to the next row
  #loop through all rows, if any contain a 1 count it and move to the next row
  for(i in 1:nrow(summary)) {
    for(j in (which(colnames(summary)=="marker01"):which(colnames(summary)=="marker10"))) {
      if (summary[i,j] == 1){
        totalpos <- (totalpos + 1)
        break
      }
    }
  }
  #Percentage of infected patients
  p_infected = 100*(totalpos/total_screens)
  
  #Percent of patients that identify as male
  male <- sum(summary$gender == "male")
  female <- sum(summary$gender == "female")
  #male percent
  malepercent = 100*(male/total_screens)
  #female percent
  femalepercent = 100*(female/total_screens)
  
  #Creating a summarized data frame to reference if prefer that to having the data on the plot
  summarydf <- data.frame(total_screens,p_infected,malepercent,femalepercent)
  colnames(summarydf) <- c("Total Screenings", "Percent Infected", "Percent Male", "Percent Female")
  

  
#Using ggplot to create a histogram for age distribution and displaying the needed summary data on the plot
  
  return(ggplot(summary, aes(x = age))
         + geom_histogram(binwidth = 1) +
         geom_text(aes(40, 2800, label=paste("Total Screens:", total_screens))) +
           geom_text(aes(40, 2300, label=paste("% Infected:", p_infected))) +
           geom_text(aes(40, 1800, label=paste("Total Screens - % Male:", malepercent))) +
           geom_text(aes(40, 1300, label=paste("Total Screens - % Female:", femalepercent))) +
           xlab("Age") +
           ylab("# Patients") +
           scale_x_continuous(breaks = round(seq(min(summary$age), max(summary$age), by = 5),5)) +
           ggtitle("Summary of combined testing data") +
         theme_classic())
  
}

