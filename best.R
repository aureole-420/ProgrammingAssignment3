## assignment 3 question 1

best <- function(state, outcome){
	
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
	states <- levels(factor(data$State))
	
	## Check that state and outcome are valid
	if(!is.element(state,states)){
		stop("invalid state")
	}
	if(outcome == "heart attack"){
		idx <- 11
	}else if(outcome == "heart failure"){
		idx <- 17
	}else if(outcome == "pneumonia"){
		idx <- 23
	}else{
		stop("invalid outcome")
	}
	data[,idx] <- as.numeric(data[,idx])
	
	## Return hospital name in that state with lowest 30-day death rate
	in_state <- data$State == state
	data_state <- data[in_state,]
	
	valid_outcome <- !is.na(data_state[,idx]) 
	data_state_eff <- data_state[valid_outcome,]
	
	state_hospitals_eff <- data_state_eff$Hospital.Name
	if(length(state_hospitals_eff) >=1){
		best_i <- 1 # choose the first one as the best
		best_rate <- data_state_eff[1,idx]
		}else{ # no hospital in state
		message(paste("no hospital in",state,"has valid data for",outcome))
		return(NA)
		} 
		
	for(i in 1:length(state_hospitals_eff)){
		new_i <- i
		new_rate <- data_state_eff[i,idx]
		if(new_rate < best_rate){
			best_i <- new_i
			best_rate <- new_rate
		}
		else if (new_rate == best_rate){
			best_h_name <- data_state_eff[best_i, 2]
			new_h_name <- data_state_eff[new_i, 2]
			if (new_h_name < best_h_name){
				best_i <- new_i
				best_rate <- new_rate
			}
		}		
	}
	
	return(data_state_eff[best_i,2])
	
}


