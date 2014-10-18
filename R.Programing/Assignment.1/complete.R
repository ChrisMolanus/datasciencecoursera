complete <- function(directory, id = 1:332) {
  output <- data.frame()
  for(i in id){
    sensorID <- formatC(i, width = 3, format = "d", flag = "0")
    data <- read.csv(paste(paste(directory,sensorID,sep = "/"),"csv",sep = "."), colClasses=c("character","double","double"))
    output <- rbind(output,c(i,nrow(data[complete.cases(data),])))
  }
  colnames(output) <- c("id","nobs")
  return(output)
}