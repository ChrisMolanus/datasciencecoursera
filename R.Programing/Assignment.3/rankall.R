rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  column <- if (outcome == "heart attack") {
    data[, 11] <- suppressWarnings(as.numeric(data[, 11]))
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    data[, 17] <- suppressWarnings(as.numeric(data[, 17]))
    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    data[, 23] <- suppressWarnings(as.numeric(data[, 23]))
    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  data_by_state <- split(data[, c("Hospital.Name", "State", column)], data$State)  
  
  result <- lapply(data_by_state, function(state_data, num) {
    ordered_state_data <- order(state_data[3], state_data$Hospital.Name, na.last=NA)
    
    if (num == "best") {
      state_data$Hospital.Name[ordered_state_data[1]]
    } else if (num == "worst") {
      state_data$Hospital.Name[ordered_state_data[length(ordered_state_data)]]
    } else if (is.numeric(num)) {
      state_data$Hospital.Name[ordered_state_data[num]]
    } else {
      stop("invalid num")
    }
  }, num)
  
  data.frame(hospital = unlist(result), state = names(result), row.names = names(result))
}