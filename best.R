###Kimberly Ohn
###Coursera R Programming HW3
### best.R
## id_min locates the hospital name corresponding to the lowest mortality rate
## for a given outcome and state.
id_min <- function (state,outcome,data){
  subset <- data[which (data$State==state),]
  if (outcome =="heart attack") 
  {minimum <- min(subset[,11], na.rm = TRUE)
  min_index <- which (subset[,11]==minimum,  arr.ind = TRUE) }
  else if (outcome =="heart failure") 
  {minimum <- min(subset[,17], na.rm = TRUE)
    min_index <- which (subset[,17]==minimum,  arr.ind = TRUE) }
  else if (outcome =="pneumonia")
  {minimum <- min(subset[,23], na.rm = TRUE)
    min_index <- which (subset[,23]==minimum,  arr.ind = TRUE) }
  hospital_name <- subset$Hospital.Name[min_index]  
}


best <- function (state, outcome) {

## Read outcome data
  data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## convert outcome strings into numeric columns 
  data[, 11] <- as.numeric(data[, 11]) 
  data[, 17] <- as.numeric(data[, 17]) 
  data[, 23] <- as.numeric(data[, 23]) 

## Check that state and outcome are valid
  if (!state %in% data$State) 
    {stop ("invalid state")}
  else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) 
  {stop ("invalid outcome")}
## Return hospital name in that state with lowest 30-day death rate
  else {Name <- id_min (state,outcome,data)
  return (Name)}
  print(Name)
}


