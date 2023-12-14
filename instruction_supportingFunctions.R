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

