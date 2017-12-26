## 4. Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
      ## Read outcome data
      ## I replace "Not Available" with NA to avoid errors
      data <- read.csv("outcome-of-care-measures.csv",colClasses = 'character' , header =TRUE, na.strings = "Not Available")
      ## As I was getting error when compare, I change data types to numeric:
      ## In tha case of more outcomes, an index vector could be used
      data[, 11] <- as.numeric(data[, 11]) # heart attack
      data[, 17] <- as.numeric(data[, 17]) # heart failure
      data[, 23] <- as.numeric(data[, 23]) # pneumonia
      ## Check that state and outcome are valid
      valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
      state_arr <- sort(unique(data$State))     # Gets states list with unique values
      arr_len <- length(state_arr)              # Number of states
      hospital <- rep("", arr_len)              # Vector with arr_len times "" to hold for hospital ranking
      
      if (!outcome %in% valid_outcomes) {
            stop("invalid outcome")
      } else {
            for(i in 1:arr_len) {
                  # loop for each state
                  state_subset <- data[data[, 7]==state_arr[i], ]
                  if(outcome == "heart attack") {
                        hospital[i] <- aux1(state_subset, 11, num)  # aux func to reduce code
                  } else if (outcome == "heart failure") {
                        hospital[i] <- aux1(state_subset, 17, num) 
                  } else {
                        hospital[i] <- aux1(state_subset, 23, num) 
                  }
            }
      }
      ## For each state, find the hospital of the given rank
      ## Return a data frame with the hospital names and the
      # create the data frame to return
      df <- data.frame(hospital=hospital, state=state_arr)
      result <- df
      return(result)
      ## (abbreviated) state name
}

## This aux function was created to avoid replicating the code
aux1 <- function(state_subset, col_num, num) {
      # get "attack", "failure" and "pneumonia" vector
      outcome_arr <- as.numeric(state_subset[, col_num])
      len <- dim(state_subset[!is.na(outcome_arr), ])[1]
      if (num == "best") {
            rank <- state_subset[, 2][order(outcome_arr, state_subset[, 2])[1]]
      } else if (num == "worst") {
            rank <- state_subset[, 2][order(outcome_arr, state_subset[, 2])[len]]
      } else if (num > len) {
            rank <- NA
      } else {
            rank <- state_subset[, 2][order(outcome_arr, state_subset[, 2])[num]]
      }
      result <- rank
      return(result)
}
