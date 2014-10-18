pollutantmean <- function(directory, pollutant, id = 1:332) {
  data <- NULL
  for(i in id){
    sensorID <- formatC(i, width = 3, format = "d", flag = "0")
    current_file <- read.csv(paste(paste(directory,sensorID,sep = "/"),"csv",sep = "."),header=T,sep=",")
    data <- rbind(data ,current_file)
  }
  return(round(mean(data[[pollutant]],na.rm=TRUE),3))
}