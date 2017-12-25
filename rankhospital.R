###Kimberly Ohn
###Coursera R Programming HW3
### Rankhospital.R
##Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
##state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
## The num argument can take values ¡°best¡±, ¡°worst¡±, or an integer indicating the ranking
##(smaller numbers are better). 

##parse through the numbers to reassign character ranks to numeric ranks 
##obtain the hospital name for the given state of a specified outcome and rank
 rankwithinoutcome <- function(subset,colnum,num){
  subset <- subset[which(!is.na(subset[,colnum])),]
  if (num == "best")
    {num <- 1}
  else if (num == "worst")
    {num <- length(subset[,colnum])}
  else if (num > length(subset[,colnum])) 
  {num <- NA}
  else 
  {num <- num }
## Apply the order to the subset (hospital name extracted)
## Apply a specified rank 
  ranked <- subset[,2][order(subset[,colnum],subset[,2])][num]
  return(ranked)
  }
 
  
## Read outcome data
 rankhospital <- function (state,outcome,num) {  
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
  else { 
    subset <- data[which (data$State==state),]
    if (outcome =="heart attack") 
    {colnum<- 11}
    else if (outcome =="heart failure") 
    {colnum<- 17}
    else if (outcome =="pneumonia")
    {colnum<- 23}
## Return hospital name in that state with the given rank
## 30-day death rate
    ranked<- rankwithinoutcome(subset,colnum,num) }   
   

 }
  
