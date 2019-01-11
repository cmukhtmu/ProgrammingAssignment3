library(tidyverse)

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    originalData = read.csv("outcome-of-care-measures.csv", colClasses = "character")
    finalOutcome = data.frame()
    outcomeData = data.frame()
    
    if(!(outcome %in% c("heart attack","heart failure","pneumonia"))) stop("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death
    if(outcome == "heart attack") outcomeCol = 11
    else if(outcome == "heart failure") outcomeCol = 17
    else if(outcome == "pneumonia") outcomeCol = 23
    
    uniqueStates = distinct(originalData, originalData$State)

    for (state in uniqueStates[,1])
    {
        outcomeData = subset(originalData, State==state)
        
        outcomeData = outcomeData[order(as.numeric(as.character(outcomeData[,outcomeCol])), outcomeData[,2]),]
        
        outcomeData = outcomeData[,c(2,7,outcomeCol)]
        
        outcomeData = tibble::rowid_to_column(outcomeData, "Rank")
        
        outcomeData = outcomeData[complete.cases(outcomeData),]
        
        names(outcomeData) <- c("Rank", "Hospital", "State", "Rate")
        
        outcomeData[,c(2,3,4,1)]
        
        outcomeData = subset(outcomeData, Rate!="Not Available")
        
        if(num == "best") num = 1
        else if(num == "worst") num = max(outcomeData$Rank)
        
        outcomeData = subset(outcomeData, Rank==num)
        
        finalOutcome = rbind(data.frame("Hospital"=outcomeData[1,2], "State"=state), finalOutcome)
    }
    
    finalOutcome[order(as.character(finalOutcome[,2])),]
    

   
    ## rate
}