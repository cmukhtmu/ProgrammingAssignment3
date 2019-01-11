library(tidyverse)

rankhospital <- function(state, outcome, num = "best") {
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
    
    ##message("column selected: ", outcomeCol)
    
    outcomeData = subset(outcomeData, State==state)
    
    outcomeData = outcomeData[order(as.numeric(as.character(outcomeData[,outcomeCol])), outcomeData[,2]),]
    
    outcomeData = outcomeData[,c(2,outcomeCol)]
    
    outcomeData = tibble::rowid_to_column(outcomeData, "Rank")

    outcomeData = outcomeData[complete.cases(outcomeData),]
    
    names(outcomeData) <- c("Rank", "Hospital.Name", "Rate")

    outcomeData[,c(2,3,1)]
                                 
    outcomeData = subset(outcomeData, Rate!="Not Available")
    
    if(num == "best") num = 1
    else if(num == "worst") num = max(outcomeData$Rank)
    
    outcomeData = subset(outcomeData, Rank==num)
    
    outcomeData[1,2]
    
    ## rate
}