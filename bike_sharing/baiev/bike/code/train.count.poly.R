require(splines)

train.count.poly <- function(dataset, options=list(trace=F), features){
  models <- list()
  models$casuals <- list()
  models$registers <- list()
  models$casuals.we <- list()
  models$registers.we <- list()
  
  for(i in c(1:24)){
    if(options$trace) 
      print(paste('models for hour ', i-1, ':', sep=''))
    
    ds <- dataset[dataset$hour == (i-1),]
    
    ds.we <- ds[ds$workingday == 0,]
    ds <- ds[ds$workingday == 1,]
    
    models$casuals[[i]] <- glm(casual ~ bs(sec, df=options$polydegree), data=ds)
    models$registers[[i]] <- glm(registered ~ bs(sec,df=options$polydegree), data=ds)
    models$casuals.we[[i]] <- glm(casual ~ bs(sec,df=options$polydegree), data=ds.we)
    models$registers.we[[i]] <- glm(registered ~ bs(sec,df=options$polydegree), data=ds.we)
    
    if(options$trace) 
      cat(paste('\t\t\t\t\t\tpoly created\n', sep=''))
    
  }
  return (models)
}