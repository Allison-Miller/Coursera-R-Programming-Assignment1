best<-function(state, outcome){
  options(stringsAsFactors = FALSE)
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  colnames(data)[11] <- "heart attack"
  colnames(data)[17] <- "heart failure"
  colnames(data)[23] <- "pneumonia"
  df=data.frame(data$"Hospital.Name",  data$"State", data$"heart attack", data$"heart failure", data$"pneumonia")  
  colnames(df)[1]<-"hospital_name"
  colnames(df)[2]<- "State"
  colnames(df)[3]<- "heart attack"
  colnames(df)[4]<- "heart failure"
  colnames(df)[5]<- "pneumonia"
  valid_outcome=list("heart attack","heart failure", "pneumonia")
  if (! outcome %in% valid_outcome){
    stop ("invalid outcome")
  }
  if (! state %in%  state.abb) {
    stop ("invalid state")
  }
  get_state=subset(df, State==state)
  
  result=rank(get_state[[outcome]])
  lowest=which.min(result)
  lowest
  get_state[lowest,1]
}
