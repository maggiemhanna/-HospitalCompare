rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that  outcome are valid
    
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
    
    all_hospitals_ranked = data.frame(hospital = character(),state = character())
    
    ## For each state, find the hospital of the given rank
    splited_data <- split(outcome_data,outcome_data$State)
    
    for(data_frame in splited_data){
        
    name_outcome_data = data_frame[ ,c(2,coln)]
    
    name_outcome_data[,2] = as.numeric(name_outcome_data[,2])
    
    ranked_hospitals = name_outcome_data[ order(name_outcome_data[,2], name_outcome_data[,1]), ]
    complete_cases = complete.cases(ranked_hospitals)
    ranked_hospitals = ranked_hospitals[complete_cases,1]
    
    
    if(num == "best")
        hospital_name = ranked_hospitals[1]
    else if(num == "worst")
        hospital_name = ranked_hospitals[length(ranked_hospitals)]
    else if(num > length(ranked_hospitals))
        hospital_name = '<NA>'
    else 
        hospital_name = ranked_hospitals[num]
    
    state_name = data_frame$State[1]
    
    all_hospitals_ranked <- rbind(all_hospitals_ranked, data.frame(hospital = hospital_name, state=state_name))
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    return(all_hospitals_ranked)
}
