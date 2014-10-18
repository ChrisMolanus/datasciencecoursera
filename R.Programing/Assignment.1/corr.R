corr <- function(directory, threshold = 0) {
  goodmonitors <- complete(directory, 1:332)
  goodmonitors <- subset(goodmonitors, nobs > threshold )
  
  correlations <- vector()

  for(i in goodmonitors$id ) {
    filename <- sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    
    data <- read.csv(filepath)
  
    completeCases <- data[complete.cases(data),]
    correlations <- c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
  }
  
  return(correlations)
}