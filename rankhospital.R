

rankhospital <- function(state, outcome, num = "best"){
	
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
	
	## Return hospital name in that state with the given rank 
	## 30-day death rate 
	
	in_state <- data$State == state
	data_state <- data[in_state,]
	
	valid_outcome <- !is.na(data_state[,idx]) 
	data_state_eff <- data_state[valid_outcome,]
	
	## order 
	ii <- order(data_state_eff[,idx],data_state_eff$Hospital.Name)
	ordered_data <- data_state_eff[ii,]
	
	if(num == "best"){
		return(ordered_data[1,2])
	}else if(num == "worst"){
		tail = nrow(ordered_data)
		return(ordered_data[tail,2])
	}else if(!is.numeric(num)){
		stop("invalid ranking")
	}else if(num > nrow(ordered_data)){
		return(NA)
	}else{
		return(ordered_data[num,2])
	}
		
	
}