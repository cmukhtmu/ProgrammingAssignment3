best <- function(state, outcome) {
    ## Read outcome data
    outcomeData = read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if(!(state %in% outcomeData$State)) stop("invalid state")

    if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
    
    ##print("Input looks good")
    
    ## Return hospital name in that state with lowest 30-day death
    if(outcome == "heart attack") outcomeCol = 11
    else if(outcome == "heart failure") outcomeCol = 17
    else if(outcome == "pneumonia") outcomeCol = 23
    
    message("column selected: ", outcomeCol)
    
    outcomeData = subset(outcomeData, State==state)
    
    outcomeData = outcomeData[order(as.numeric(outcomeData[,outcomeCol]), outcomeData[,2]),]
    
    outcomeData[1,2]
    
    ## rate
}