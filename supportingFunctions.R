#Code Requirements
#Script: supporting Functions.R
##containing custom functions to accomplish various data handling or summary tasks
##1) function that converts all files in a directory with space or tab delimited data into comma-separated value files
###because country X and country Y use different delimiters in their files
####default for read.table() is sep = " " which is one or more spaces, tabs, newlines, or carriage returns
####need to use paste and variables to create new file name for write.table() to put new file into
####custom function needs to loop through all files in a directory using for loop
####start in directory containing files to convert

#setwd("C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project/countryY")
#"C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project"
#file.remove(list.files(pattern = "*.csv"))
  
#####custom function that converts a given file to a .csv file = WORKING
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

###Use custom function convertdirectorytocsv() to convert all .txt files in countryY directory to .csv files

#convertdirectorytocsv("C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project/countryY")


###Question: does it need to delete the .txt files or leave them before moving on to the next step


##2) function to compile data from all .csv files in a directory into a single .csv file
### final file should have original 12 columns and country and dayofYear columns
###user needs to be able to decide if they want to remove rows with NAs in any columns, include NAs in data but be warned of presence, or include NAs without warning
###Tip: strsplit(): splits a string of characters at defined character and lets you access the sub-strings

many2onecsv<-function(directory){
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
    #column name issue solved by exluding column names from being written if file already exists!
  }
}
#"C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project/all.csv"

##use custom function to compile all files in countryX database
#many2onecsv("C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project/countryX/")

#use custom function to compile all files in country Y database
#many2onecsv("C:/Users/jwmag/OneDrive/Desktop/Intro_to_biocomputing/Final_Project/Biocomputing-Final-Project/countryY/")

















#James Magas
#2023-03-12
#Biocomputing Final Project
#supportingFunctions portion

#if person can come in with c/d/e country data could they use this

#1ST REQUIREMENT
#create a function that converts all files in
#directory with space- or tab-delimited data (.txt)
#into comma-separated value files

#----------------------------------

#2ND REQUIREMENT
#write a function to compile data from all .csv files
#in a directory into a single .csv file

#need to have all twelve columns from daily data sheets
#but also a column for country and a column for dayofYear

#user input will allow them to choose if rows with NA's in any columns
#or include NA's in the compiled data but are warned of their presence
#or include NA's in the the compiled data without a warning
#strsplit() will be useful for this
#it will split a string of characters at a defined character
#and allow you to access the sub-strings of characters
#usage is as follows
#if x<-"I love biocomputing" then z<-strsplit(x," ") would allow you
#to access each word in the sentence from the contents of z

#-----------------------------------

#3RD REQUIREMENT
#write a function to summarize the compiled data set in terms of number
#screens run, percent of patients screened that were infected
#percent of patients that identify as male and femal
#age distribution of patients (as a table or plot)
#may use the provided (allData.csv) so we can perform this task if
#unable to complete the other tasks