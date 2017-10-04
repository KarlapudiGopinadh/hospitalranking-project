best<-function(state,outcome)
{
  a=state
  fileread<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  deathdata<-fileread[,c(2,7,11,17,23)] 
  names(deathdata)<-c("hospital","state","heart attack","heart failure","pneumonia")
  
  states=deathdata[,2]
  outcomes<-c("heart attack","heart failure","pneumonia")
  if((state %in% states)==FALSE)
  {
    stop(print("invalid state"))
  }
  if(outcome %in% outcomes==FALSE)
  {
    stop(print("invalid outcome"))
  }
  
  ##subsetting the required data using state
  subeddata <- subset(deathdata, state == a)
  
  if(outcome=="heart attack"){
    colno=3
  }
  else if(outcome=="heart failure"){
    colno=4
  }
  else {
    colno=5
  }
  reqcol<-as.numeric(subeddata[,colno])
  bad<-is.na(reqcol)
  gdeathdata<-subeddata[!bad,]
  
  
  reqdata<-as.numeric(gdeathdata[,colno])
  mincol<-which(reqdata==min(reqdata))
  reqhosp<-gdeathdata[mincol,1]
  
  if(length(reqhosp)>1)
  {
    reqhosp<-sort(reqhosp)
    reqhosp[1]
    
  }
  else
  {
    reqhosp
  }
  
  
  
  
  
}