best <- function(state, outcome) {
    
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    
    ## Check that state and outcome are valid
    if(sum(outcome_data$State == state) == 0)
        stop('invalid state')
    
    possible_outcome_names = c('heart attack','heart failure','pneumonia')
    if(sum(possible_outcome_names == outcome) == 0)
        stop('invalid outcome')
    

    if(outcome == 'heart attack'){
        coln = 11
    } else if (outcome == 'heart failure'){
        coln = 17
    } else {
        coln = 23
    }
    name_outcome_data = outcome_data[outcome_data$State == state,c(2,coln)]        
    name_outcome_data[,2] = as.numeric(name_outcome_data[,2])
    
    MIN = min(name_outcome_data[,2], na.rm = TRUE)
    INDEX = which(name_outcome_data[,2] == MIN)
    
    ## rate
    hospital_rate = MIN
    
    ## Return hospital name in that state with lowest 30-day death
    hospital_name = min(name_outcome_data[INDEX,1])
    return(hospital_name)
}