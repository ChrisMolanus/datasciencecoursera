rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data file
  data <- read.csv(file="outcome-of-care-measures.csv", colClasses = 'character')
  
  if(!any(state == data$State)) {
    stop('invalid state')
  }
  
  col <- if (outcome == "heart attack") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  data_for_state <- data[data$State == state, c("Hospital.Name", col)]
  
  if (nrow(data_for_state) == 0) {
    stop("invalid state")	
  }
  
  data_for_state[,2] <- suppressWarnings(as.numeric(data_for_state[,2]))
  ordered_data_for_state <- order(data_for_state[col], data_for_state$Hospital.Name, na.last=NA)
  
  if (num == "best") {
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[1]])
  } else if (num == "worst") {
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[length(ordered_data_for_state)]])
  } else if (is.numeric(num)) {
    as.character(data_for_state$Hospital.Name[ordered_data_for_state[num]])
  } else {
    stop("invalid num")
  }
}