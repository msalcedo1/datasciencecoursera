## 2.  Finding the best hospital in a state

best <- function(state, outcome) {
      ## Read outcome data
      #data <- read.csv(file='outcome-of-care-measures.csv', colClasses = 'character')
      data <- read.csv("outcome-of-care-measures.csv",colClasses = 'character' , header =TRUE, na.strings = "Not Available")
      ## As I was getting error when compare, I change data types to numeric:
      data[, 11] <- as.numeric(data[, 11]) # heart attack
      data[, 17] <- as.numeric(data[, 17]) # heart failure
      data[, 23] <- as.numeric(data[, 23]) # pneumonia
      
      ## Check that state and outcome are valid
      valid_outcomes <- c("heart attack", "heart failure", "pneumonia") ##Vector with valid outcomes
      if (!state %in% data$State) {       ## if state doesn't appears on file
            stop("invalid state")
      } else if(!outcome %in% valid_outcomes) {
            stop("invalid outcome")       ## if outcome doesn't match with the ones required
      } else {                            ## Otherise the query is valid, so let's find the result
            if(outcome == "heart attack") {
                  ncol <- 11
            } else if(outcome == "heart failure") {
                  ncol <- 17
            } else {
                  ncol <- 23
            }
      }
      ## Return hospital name in that state with lowest 30-day death
      state_subset <- data[data[, 7]==state, ]  ## Reads all records for that state
      outcome_arr <- state_subset[, ncol]       ##
      min <- min(outcome_arr, na.rm=T)
      min_index <- which(outcome_arr == min)
      hosp_name <- state_subset[min_index, 2]
      result <- hosp_name
      return(result)
      ## rate
}
best("TX", "heart attack")