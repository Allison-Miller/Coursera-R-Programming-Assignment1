rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  
  num=c("best", "worst")
  data <- read.csv("outcome-of-care-measures.csv",na.strings= "Not Available", colClasses = "character")
  data[,2]<-as.character(data[, 2])
  data[, 7]<-as.character(data[, 7])
  data[, 11] <- as.numeric(data[, 11])
  data[, 17]<-as.numeric(data[, 17])
  data[, 23]<-as.numeric(data[, 23])
  colnames(data)[11] <- "heart attack"
  colnames(data)[17] <- "heart failure"
  colnames(data)[23] <- "pneumonia"
  df=data.frame(data$"Hospital.Name",  data$"State", data[[outcome]])   
  
  colnames(df)[1]<-"Hospital.Name"
  colnames(df)[2]<- "State"
  colnames(df)[3]<- "Rate"
  df[, 3]<-as.numeric(df[, 3])
  df_clean=df[ ! is.na( df[, 3] ) , ]
  valid_outcome=list("heart attack","heart failure", "pneumonia")
  if (! outcome %in% valid_outcome){
    stop ("invalid outcome")
  }
  if (! state %in%  state.abb) {
    stop ("invalid state")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  get_state=subset(df, State==state)
  count=as.numeric(nrow(get_state))
  if (num>count){
    stop("NA")
  }
  
  get_state_clean=subset(df_clean, State==state)
  ranked=rank(get_state_clean[, 3])  
  ranked2=order(get_state_clean[,3])
  new_result= as.data.frame (cbind( get_state_clean$Hospital.Name,get_state_clean[,3], ranked2))
  "best"=which.min(ranked)
   "worst"=which.max(ranked)
  if (num="best"){
    hospital=(which.min(ranked))
    best=new_result[hospital,1]
    print(best)
  }
  else (num="worst"){
    hospital=(which.max(ranked))
    worst=new_result[hospital,1]
    print(worst)
  }
  else (num %in% ranked){
    hospital=match(num, ranked)
    output=new_result[hospital,1]
    print(output)
  

  
  
}
  (get_state_clean[,1:3][order(get_state_clean[,3])])
  order(get_state_clean[,3])[get_state_clean[,1:3]]
  all=get_state_clean[,1:3][
  y=order(all[,3])
  
dimnames(new_result)
ranked=rank(get_state_clean[, 3])
dim(new_result)
df=data.frame(data$"Hospital.Name",  data$"State", outcome)  
num=120