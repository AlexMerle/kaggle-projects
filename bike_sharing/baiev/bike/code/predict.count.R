predict.count <- function(models, dataset, options=list(trace=F), features){
  submit <- data.frame(datetime = list(), count = list())
    
  for(i in c(1:24)){
    
    ds <- dataset[dataset$hour == (i-1),]
    
    ds.we <- ds[ds$workingday == 0,]
    ds <- ds[ds$workingday == 1,]
    
    casual <- predict(models$casuals[[i]], newdata=ds[,features])
    registered <- predict(models$registers[[i]], newdata=ds[,features])
    
    if(nrow(ds.we) == 0){
      casual.we <- vector()
      registered.we <- vector()
    }
    else {
      casual.we <- predict(models$casuals.we[[i]], newdata=ds.we[,features])
      registered.we <- predict(models$registers.we[[i]], newdata=ds.we[,features])
    }
    
    df <- data.frame(datetime=ds$datetime, count=(as.vector(casual)+as.vector(registered)))
    df.we <- data.frame(datetime=ds.we$datetime, count=(as.vector(casual.we)+as.vector(registered.we)))
  
    submit <- rbind(submit, df, df.we)
    
    if(options$trace) 
      print(paste('prediction for hour ', i-1, ' done', sep=''))
  }
  
  submit$count[submit$count<0] <- 0
  submit$count[is.na(submit$count)] <- 0
  
  submit <- submit[order(submit$datetime),]
  
  return (submit)
}