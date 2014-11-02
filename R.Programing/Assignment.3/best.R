best <- function(state, outcome) {
  ## Read outcome data file
  fileData = read.csv("outcome-of-care-measures.csv", colClasses = "character")
  subSet <- fileData[fileData$State == state,c(2,11,17,23)]
  if (nrow(subSet) > 0){
    ## which outcome
    if(outcome == "heart attack"){
      minRate = min(suppressWarnings(as.numeric(subSet$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),na.rm=TRUE)
      hospital <- subSet[which(subSet$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minRate), "Hospital.Name"]
    }else if(outcome == "heart failure"){
      minRate = min(suppressWarnings(as.numeric(subSet$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),na.rm=TRUE)
      hospital <- subSet[which(subSet$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == minRate), "Hospital.Name"]
    }else if(outcome == "pneumonia"){
      minRate = min(suppressWarnings(as.numeric(subSet$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),na.rm=TRUE)
      hospital <- subSet[which(subSet$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == minRate), "Hospital.Name"]
    } else {
      stop("Invalid outcome")
    }
  }else{
    stop("invalid state")
  }
  
  return(hospital)
}