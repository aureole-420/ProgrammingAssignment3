# R programming assignment 3, question 3

rankall <- function(outcome, num = "best"){
	
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
	states <- levels(factor(data$State))
	
	## Check that outcome is valid
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
	
	## For each state, find the hospital of the given rank
	rank_st <- c()
	rank_hp <- c()
	for (state in states){
		rank_st <- c(rank_st, state)
		
		in_state <- data$State == state
		data_state <- data[in_state,]
	
		valid_outcome <- !is.na(data_state[,idx]) 
		data_state_eff <- data_state[valid_outcome,]
	
		## order 
		ii <- order(data_state_eff[,idx],data_state_eff$Hospital.Name)
		ordered_data <- data_state_eff[ii,]
		
		if(num == "best"){
			rank_hp <- c(rank_hp, ordered_data[1,2])
		}else if(num == "worst"){
			tail = nrow(ordered_data)
			rank_hp <- c(rank_hp, ordered_data[tail,2])
		}else if(!is.numeric(num)){
			stop("invalid ranking")
		}else if(num > nrow(ordered_data)){
			rank_hp <- c(rank_hp, NA)
		}else{
			rank_hp <- c(rank_hp, ordered_data[num,2])
		}				
	}	
	
	## Return a data frame with the hospital names and the state name
	r <- data.frame(hospital = rank_hp, state= rank_st, row.names = rank_st)
	ii <- order(rank_st,rank_hp)
	r <- r[ii,]
	return(r)	
}