### second try



fileConverter<-function(workingDir){
  setwd(workingDir)
  files2convert<-list.files(workingDir, pattern = ".txt")
  for(i in 1:length(files2convert)){
    nameElements<-strsplit(files2convert[i], split = "\\.")
    dfHolder<-read.table(files2convert[i], header = TRUE, sep = " ")
    write.csv(dfHolder, paste(nameElements[1],".csv"), row.names=FALSE, col.names=TRUE)
  }
}

fileConverter("C:/Users/Judith Lanahan/OneDrive/Desktop/Biocomputing/R/FinalProject/Biocomputing-Final-Project/countryY")
