# Czerwiec, Mae
# Biocomputing Final Project
# "Supporting Functions"

# 1: A function to convert .txt files to .csv files
#INPUT: x must be a filename, head must be a boolean (indicates whether headers should be taken), sep must be a string (tab or space most common)
#OUTPUT: the name of the new .csv file, which has been created in the working directory
filetocsv=function(filename=x, head=FALSE, sep=" ", csvname="default.csv"){
  init=read.table(filename, header=head, sep=sep)
  csv=write.csv(init, csvname, row.names=FALSE)
  return(csvname)
}

# 2: A function to compile data from all .csv files to one single .csv file
#INPUT: the directory where the .csv files are located ("d")-- must be in the parent directory and
## whether you want to remove NA values ("yes"), be warned of their presence ("warn"), or leave them in the dataset ("no")
#OUTPUT: a .csv file created in the parent directory ("finalfile")
filecompile=function(directory=d, finalfile="final.csv", removeNA="no"){
  alldata=data.frame(colnames(c("gender", "age", "marker01", "marker02", "marker03", "marker04", "marker05", "marker06", "marker07", "marker08", "marker09", "marker10", "country", "date")))
  files=list.files(path=directory, pattern=".csv")
  for(i in 1:length(files)){
    filename=files[i]
    setwd(directory)
    data=read.csv(files[i], header=TRUE)
    data$country[1:nrow(data)]=directory
    for(d in 1:nrow(data)){
      z<-unlist(strsplit(filename, split=""))
      data$date[d]=paste(z[8], z[9], z[10])
      data$date[d]=gsub(" ", "", data$date[d])
    }
    alldata=rbind(alldata, data)
    setwd("..")
  }
  final=write.csv(alldata, finalfile, row.names=FALSE)
  for(n in 1:nrow(alldata)){
    if(removeNA=="yes"){
      for(a in 3:12){
        if(alldata[n,a]=="NA"){
          alldata=alldata.drop(alldata.index[n])
        }
      }
    }
    else if(removeNA=="warn"){
      for(a in 3:12){
        if(alldata[n,a]=="NA"){
          print(cat("Warning! This row contains NA value(s): ", n))
        }
      }
    }
  }
  return(alldata)
} 

##ggplot2 must be installed for summarizeData to work

# 3: A function to summarize compiled data. Include:
# number of screens run, % infected patients, gender makeup of pop., age distribution
#INPUT: a data file containing the given columns (disease marker(s), gender, age), and the number of screens you want summary data for (default=10000, but can be a variable like nrow(data))
#OUTPUT: a summary of # screens, % infected, gender makeup AND a plot of age distribution
summarizeData=function(data=x, numscreens=10000){
  infected=0
  for(n in 1:numscreens){
    for(c in 3:12){
      if(data[n,c]==1){
        infected=infected+1
        break #even one disease marker is sufficient to have the disease
      }
    }
  }
  pctinfected=infected/numscreens*100
  males=0
  females=0
  for(n in 1:numscreens){
    if(data$gender[n]=="male"){
      males=males+1
    }
    else if(data$gender[n]=="female"){
      females=females+1
    }
  }
  pctmales=(males/numscreens)*100
  pctfemales=(females/numscreens)*100
  summary=data.frame(numscreens, pctinfected, pctmales, pctfemales)
  agedist<-ggplot(data = data[1:numscreens,], aes(x = age)) + 
    geom_histogram(binwidth = 1, fill = "darkgoldenrod1", color = "darkblue") + 
    theme_classic()
  print(agedist)
  return(summary)
}



