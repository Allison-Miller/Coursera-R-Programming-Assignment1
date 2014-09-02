rankall <- function(outcome, num = "best") {
  setwd("d:/r programming")
  options(stringsAsFactors=FALSE)
  data <- read.csv("outcome-of-care-measures.csv",na.strings= "Not Available", colClasses = "character")
  valid_outcome=list("heart attack","heart failure", "pneumonia")
  if (! outcome %in% valid_outcome){
    stop("invalid outcome")
  }
  
  colnames(data)[11] <- "heart attack"
  colnames(data)[17] <- "heart failure"
  colnames(data)[23] <- "pneumonia"
  data[,2]<-as.character(data[, 2])
  data[, 7]<-as.character(data[, 7])
  data[, 11] <- as.numeric(data[, 11])
  data[, 17]<-as.numeric(data[, 17])
  data[, 23]<-as.numeric(data[, 23])
  df=data.frame(data$"Hospital.Name",  data$"State", data[[outcome]])
  colnames(df)[1]<-"Hospital.Name"
  colnames(df)[2]<- "State"
  colnames(df)[3]<- "Rate"
  df[, 3]<-as.numeric(df[, 3])
  clean=df[ ! is.na( df[, 3] ) , ]
  state=unique(clean$State)
  #order(states)
  
  #state_abc=(sort(states))
  
  result=data.frame(NULL)
  
  
  for(states in seq_along(state)){
    
    get_state=subset(clean, clean$State==state[states])
    count=as.numeric(nrow(get_state))
    get_state[,3]<-as.numeric(get_state[,3])
    get_state[,1]<-as.character(get_state[,1])
    sorted=get_state[ order(get_state[,3], get_state[,1]), ]
    sorted[,1]<-as.character(sorted[,1])
    ranked=order(sorted[,3])
    if (num>count){
      hospital=("NA")
    }
    if (num%in%ranked){
      hospital=(sorted[num, 1])
    }
    if (num=="best"){
      best=which.min(ranked)
      hospital=(sorted[best,1])
    }
    if(num=="worst"){
      worst=which.max(ranked)
      hospital=sorted[worst,1]
    }  
    
    event=data.frame(hospital) 
    result=rbind(result, event)
    
  }
  
  y=cbind(result, state)
  
  
  final=y[order(y[,2],y[,1]),]
  
  print(final)
}

