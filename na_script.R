setwd("~/Desktop/shell-lesson-data/exercises/Biocomputing-Final-Project")

noNA<-function(myData){
  
  # find out what user wants
  findNA<-readline(prompt = "Would you like to remove rows containing N/As? y/n: ")
  
  #in the event that user wants to get rid of NA rows
  if(findNA == "y"){
    r2Del<-0 #will become a vector of the rows containing N/As that should be deleted
    for(rw in 1:nrow(myData)){
      for(cl in 1:ncol(myData)){ #this loop will check every value of allData for N/A
        if(is.na(myData[rw,cl])== TRUE){
          r2Del<-append(r2Del, rw) #adds the row to r2Del if it has an NA
        }
      }
    }
    print("clean data: ") #can probably get rid of this eventually
    rDel0<-unique(r2Del, incomparables = FALSE) #gets unique rows that have NAs
    rDel<-rDel0[2:length(rDel0)] #gets rid of the placeholder 0 we appended to
    cleanData<-myData[-rDel,] #indexes data frame, deletes rows of numbers indicated in rDel
    print(cleanData)
    print(rDel)
    
    #in the event that user doesn't want to remove data, ask if they want to be warned
  }else if(findNA == "n"){
    
    
    #if they would like to be warned...
    warnNA<-readline(prompt = "Would you like to be warned of N/As? y/n: ")
    if(warnNA == "y"){
      r2Warn<-0 #vector with row numbers to be warned about, will append to it
      for(rw in 1:nrow(myData)){
        for(cl in 1:ncol(myData)){
          if(is.na(myData[rw,cl] == TRUE)){
            r2Warn<-append(r2Warn, rw)
          }
        }
      }
      print("Rows with NAs: ") # should probably find a way to make this one line
      rWarn<-unique(r2Warn, incomparables = FALSE) #gets unique row numbers 
      print(rWarn[2:length(rWarn)]) #prints list without placeholder 0
      
      # in the event that the user is satisfied with unclean data
    }else if(warnNA=="n"){
      print("I shall give you the unclean CSV")
    }
    
    # in the event that the user types in something weird instead of y or n
    #default is unclean CSV
  }else{
    print("Input not understood. Creating csv with N/As")
  }
}
