## 3. Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
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
      if (!state %in% data$State) {
            stop("invalid state")
      } else if(!outcome %in% valid_outcomes) {
            stop("invalid outcome")
      } else {                            # Case Valid
            if (num == "best") {          # if num="best" calls previous fn
                  rank <- best(state, outcome)
                  result <- rank
                  return(result)          # Exits function
            } else {                      # Else, assings ncol depending outcome
                  if(outcome == "heart attack") {
                        ncol <- 11
                  } else if(outcome == "heart failure") {
                        ncol <- 17
                  } else {
                        ncol <- 23
                  }
            }
      }
      ## Return hospital name in that state with the given rank
      state_subset <- data[data[, 7]==state, ]
      # get "attack", "failure" and "pneumonia" vector
      outcome_arr <- state_subset[, ncol]
      len <- dim(state_subset[!is.na(outcome_arr), ])[1]    #Get len of subset
      if (num == "worst") {
            rank <- state_subset[, 2][order(outcome_arr, state_subset[, 2])[len]]
      } else if (num > len) {
            rank <- NA
      } else {
            rank <- state_subset[, 2][order(outcome_arr, state_subset[, 2])[num]]
      }
      result <- rank
      return(result)
      
      ## 30-day death rate
}
