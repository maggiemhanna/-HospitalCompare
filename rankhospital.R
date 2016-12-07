rankhospital <- function(state, outcome, num = "best") {
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
    
    ranked_hospitals = name_outcome_data[ order(name_outcome_data[,2], name_outcome_data[,1]), ]
    complete_cases = complete.cases(ranked_hospitals)
    ranked_hospitals = ranked_hospitals[complete_cases,1]
    
    if(num == "best")
        return(ranked_hospitals[1])
    if(num == "worst")
        return(ranked_hospitals[length(ranked_hospitals)])
    if(num > length(ranked_hospitals))
        return(NA)
    
    return(ranked_hospitals[num])
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
}