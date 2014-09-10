options(java.parameters="-Xmx6g")
require(extraTrees)

train.count.ert <- function(dataset, options=list(trace=F), features){
  models <- list()
  models$casuals <- list()
  models$registers <- list()
  models$casuals.we <- list()
  models$registers.we <- list()
  
  
  regIdx <- grep('registered', names(dataset))
  casIdx <- grep('casual', names(dataset))
  
  for(i in c(1:24)){
    if(options$trace) 
      print(paste('models for hour ', i-1, ':', sep=''))
    
    ds <- dataset[dataset$hour == (i-1),]
    
    ds.we <- ds[ds$workingday == 0,]
    ds <- ds[ds$workingday == 1,]
    

    rf <- extraTrees(x=ds[,features],
                       y=ds[,casIdx],
                        ntree=options$ntree, 
                        mtry=options$mtry.cas,
                        numRandomCuts=options$numRandomCuts.cas)
    models$casuals[[i]] <- rf
    
    if(options$trace) 
      cat(paste('\t\t\t\t\t\tRF for casual\n', sep=''))
    
    rf <- extraTrees(x=ds[,features],
                       y=ds[,regIdx],
                       ntree=options$ntree,
                       mtry=options$mtry.reg,
                       numRandomCuts=options$numRandomCuts.reg)
    models$registers[[i]] <- rf
    
    if(options$trace) 
      cat(paste('\t\t\t\t\t\tRF for registered\n', sep=''))
    
    rf <- extraTrees(x=ds.we[,features],
                     y=ds.we[,casIdx],
                     ntree=options$ntree,
                     mtry=options$mtry.cas,
                     numRandomCuts=options$numRandomCuts.cas)
    models$casuals.we[[i]] <- rf
    
    if(options$trace) 
      cat(paste('\t\t\t\t\t\tRF for casual.we\n', sep=''))
    
    rf <- extraTrees(x=ds.we[,features],
                       y=ds.we[,regIdx],
                       ntree=options$ntree,
                       mtry=options$mtry.reg,
                       numRandomCuts=options$numRandomCuts.reg)
    models$registers.we[[i]] <- rf
    
    if(options$trace) 
      cat(paste('\t\t\t\t\t\tRF for registered.we\n', sep=''))
  }
  return (models)
}