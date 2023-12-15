####Supporting Functions


#1) Convert File format
#This function will take a directory of files with any delimiter and convert it to a csv file
#Usage: csvconvert(directory,HeaderTF,delim)
#Ex: csvconvert("/Users/daniel/Desktop/Biocomputing-Final-Project/countryY",TRUE," ")
csvconvert<-function(directory,HeaderTF,delim){
  for (screen in list.files(directory)) {
    name<-strsplit(screen,"[.]")
    newname<-paste(c(name[[1]][1],"csv"),collapse=".")
    newpath<-paste(c(directory,newname),collapse="/")
    oldpath<-paste(c(directory,screen),collapse="/")
    table<-read.table(oldpath,header=HeaderTF,sep=delim)
    write.table(table,file=newpath,sep=",")
  }
}
#2) Compile data
datacompile<-function(directory,listcountries,NAs){
  CompiledData<-data.frame(matrix(nrow=0,ncol=14))
  setwd(directory)
  for (country in listcountries){
    countrypath<-paste(c(directory,country),collapse="/")
    setwd(countrypath)
    for (screen in list.files(path=countrypath, pattern=".csv")){
      date1<-strsplit(screen,"[.]")
      date2<-strsplit(date1[[1]][1],"_")
      date<-as.integer(date2[[1]][2])
      data<-read.csv(screen,header=TRUE)
      number<-nrow(data)
      data$country<-country
      data$dayofYear<-date
      CompiledData<-rbind(CompiledData,data)
    }
  }
  setwd(directory)
  if(NAs=="3"){} 
  else{ if (NAs=="2"){if (sum(is.na(CompiledData))!=0){cat("Warning:The compiled file contains at least one NA.")}}
    else{CompiledData<-CompiledData[rowSums(is.na(CompiledData))==0,]}
  }
  write.csv(CompiledData,file="CompiledData.csv",row.names=FALSE,col.names=TRUE)
}
#3) Summarize Data
summarize<-function(file){
  ToSummarize<-read.csv(file,header=TRUE)
  screens<-nrow(ToSummarize)
  Message1<-paste(c(paste(c("The","total","number","of","screens","is",as.character(screens)),collapse=" "),"."),collapse="")
  numbermale<-nrow(ToSummarize[ToSummarize$gender=="male",])
  numberfemale<-nrow(ToSummarize[ToSummarize$gender=="female",])
  percentmale<-numbermale*100/screens
  percentfemale<-numberfemale*100/screens
  Message2<-paste(c(paste(c("The","percentage","of","male","patients","is",as.character(percentmale)),collapse=" "),"%."),collapse="")
  Message3<-paste(c(paste(c("The","percentage","of","female","patients","is",as.character(percentfemale)),collapse=" "),"%."),collapse="")
  ToSummarize$NumberMarkers<-ToSummarize$marker01+ToSummarize$marker02+ToSummarize$marker03+ToSummarize$marker04+ToSummarize$marker05+ToSummarize$marker06+
    ToSummarize$marker07+ToSummarize$marker08+ToSummarize$marker09+ToSummarize$marker10
  numberinfected<-nrow(ToSummarize[ToSummarize$NumberMarkers>0,])
  percentinfected<-numberinfected*100/screens
  Message4<-paste(c(paste(c("The","percentage","of","patients","infected","is",as.character(percentinfected)),collapse=" "),"%."),collapse="")
  FinalMessage<-paste(c(Message1,Message2,Message3,Message4),collapse="\n")
  cat(FinalMessage)
  library(ggplot2)
  library(cowplot)
  ggplot(ToSummarize,aes(x=age))+
    geom_histogram(binwidth = 1, fill = "red", color = "black")+
    theme_classic() 
  }




