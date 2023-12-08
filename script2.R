### second try



fileConverter<-function(workingDir){
  setwd(workingDir)
  files2convert<-list.files(workingDir, pattern = ".txt")
  for(i in 1:length(files2convert)){
    nameElements<-unlist(strsplit(files2convert[i], split = "\\."))
    dfHolder<-read.table(files2convert[i], header = TRUE, sep = " ")
    write.csv(dfHolder, paste(nameElements[1],".csv"), row.names=FALSE)
  }
}

fileConverter("C:/Users/Judith Lanahan/OneDrive/Desktop/Biocomputing/R/FinalProject/Biocomputing-Final-Project/countryY")


sample_string<-"I love bio computing"
banana<- strsplit(sample_string, " ")
banana[1]
banana[[1]]
banana2<-unlist(banana)
banana2[1]
