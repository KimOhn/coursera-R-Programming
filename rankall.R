###Kimberly Ohn
###Coursera R Programming HW3
### Rankall.R
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name
  
  ## Read outcome data
  rankall <- function (outcome,num="best") { 

    
  data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## convert outcome strings into numeric columns 
  data[, 11] <- as.numeric(data[, 11]) 
  data[, 17] <- as.numeric(data[, 17]) 
  data[, 23] <- as.numeric(data[, 23]) 
  
  ## assign col numbers based on the outcome names and also stop if
  ## invalid outcome names are assigned
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
      stop ("invalid outcome")}
  
  colnum <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))

  ## remove NAs based on the column number corresponding to the outcome name
  data <- data[!is.na(data[,colnum]),]

  ## Order the data by hostpical name and their mortality rates
  data.sorted <- data[order(data[,colnum],data[,2]),]
  data.sorted <- data.sorted[!is.na(data[,colnum]),] 


  ## rankwithinstate Function to obtain the hospital name for the given state of a specified outcome and rank
  # col1: hospital name, col2:State, col3:colnum
  rankwithinstate <- function(data_by_state,num){
  ordered_state_data <- data_by_state[order(data_by_state[,3],data_by_state[,1]),]
  num <- ifelse(num == "best", 1, ifelse(num == "worst", dim(ordered_state_data)[1], as.numeric(num)))
   ranked <- ordered_state_data[num,1]
  return(ranked)
      }
  ## Call sapply, get a subdataset grouped by state using a split function
  data_by_state <- split(data.sorted[, c(2, 7,colnum)], data.sorted$State)
  result <- sapply (data_by_state,rankwithinstate,num)
  data.frame(hospital = unlist(result), state = names(result), row.names = names(result))
  }     
  
  