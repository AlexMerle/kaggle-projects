fla.all = as.formula(count ~ .)
fla.all.reg = as.formula(registered ~ .)
fla.all.cas = as.formula(casual ~ .)
fla.signif = as.formula(count ~ season+workingday+weather+temp+humidity+hour+dayofweek)

#bike.train.sub = subset(bike.train, select=-c(date,datetime,datetime.orig,registered,casual))

train.sub = subset(train, select=-c(datetime,datetime.orig,registered,casual,date))
train.reg = subset(train, select=-c(datetime,datetime.orig,casual,count,workingday))
train.cas = subset(train, select=-c(datetime,datetime.orig,registered,count,workingday))

lm = lm(fla.all, data=bike.train.sub)

lm.signif = lm(fla.signif, data=train)
summary(lm.signif)

lm.signif.pred = predict(lm, valid)


control <- trainControl(method="cv", number=2, verboseIter=T)
hyperparams <- expand.grid(mtry=c(12,14)) # 12, tuneGrid=hyperparams, 
rf.all <- train(fla.all, data=train.sub, method="rf", tuneGrid=hyperparams, trControl=control)
rf.all.pred <- predict(rf.all, valid)
rmse(rf.all.pred, valid$count)
rmsle(rf.all.pred, valid$count)

# [1] 1882.837, 45.50517, 0.3197 for 12
# [1] 45.34903, 0.3199 for 12
# w/o workingday field:
# [1] 45.43652, 0.3307 for 14
# w/o date field:
# [1] 2044.651, [1] 0.3280025 for 14
# w/o season & holiday fields:
# [1] 0.3202916 for 14
# w/o season & holiday & temp fields:
# [1] 0.3187633 for 11
# monthyear added
# [1] 0.3157666 for 14
# monthyear+daynumber added
# [1] 0.3161533 for 14
# monthyear+daynumber-date
# [1] 0.3154003


hyperparams <- expand.grid(mtry=c(9,11)) # 12, tuneGrid=hyperparams, 
# 11,12 - RMSE=32
rf.all.reg = train(fla.all.reg, data=train.reg, method="rf", tuneGrid=hyperparams, trControl=control)
# 10-12 - RMSE=13.6
rf.all.cas = train(fla.all.cas, data=train.cas, method="rf", tuneGrid=hyperparams, trControl=control)
rf.all.reg.pred = predict(rf.all.reg, valid)
rf.all.cas.pred = predict(rf.all.cas, valid)
rf.all.cnt.pred = rf.all.reg.pred + rf.all.cas.pred
rmse(rf.all.cnt.pred, valid$count)
rmsle(rf.all.cnt.pred, valid$count)

# [1] 1936.758, [1] 0.3177762 - for 14-14
# [1] 1923.204, [1] 0.3278304 - for 12-12


rmse <- function(pred, test) {
  dif = pred - test
  dif = dif*dif
  n = length(test)
  return(sqrt(sum(dif)/n))
}

rmsle <- function(pred, test) {
  pred.ln = log1p(pred)
  test.ln = log1p(test)
  dif = pred.ln - test.ln
  dif = dif*dif
  sdif = sum(dif)
  n = length(test)
  return(sqrt(sdif/n))
}

## Extra Trees
library(extraTrees)

set.seed(100)
control.et <- trainControl(method="cv", number=5, verboseIter=T)
hyperparams.et <- expand.grid(mtry=c(12,14,16),numRandomCuts=c(4,6,8)) # tuneGrid=hyperparams, 

et.all <- train(fla.all, data=train.sub, method="extraTrees", tuneGrid=hyperparams.et, trControl=control.et, linout=FALSE, trace=TRUE)
et.all.pred <- predict(et.all, valid)
rmse(et.all.pred, valid$count)
rmsle(et.all.pred, valid$count)

# [1] 1998.648, [1] 0.3188956 for 16-4


#! 1. predict weather=4 as weather=3
# 2. change all negative predictions to 1
# 3. date should be removed???
#! 4. some bad atemp values are present: plot(bike.train$temp, bike.train$atemp) (atemp == 12.12)
#! bike.train[bike.train$temp > 25 & bike.train$atemp == 12.12,]
#! 5. some bad humidity values are present: boxplot(bike.train$humidity)
# 

plot(rf.all.pred-valid$count)
cbind(valid,rf.all.pred,(rf.all.pred-valid$count))[diff>200,]
valid[valid$date == '2012-08-19',]

cbind(valid,rf.all.cnt.pred,(rf.all.cnt.pred-valid$count))[diff>200,]
plot(rf.all.pred)


# by year

train.2011 = train.sub[train.sub$year == 2011,]
valid.2011 = valid[valid$year == 2011,]
train.2012 = train.sub[train.sub$year == 2012,]
valid.2012 = valid[valid$year == 2012,]

rf.2011 <- train(fla.all, data=train.2011, method="rf", tuneGrid=hyperparams, trControl=control)
rf.2011.pred <- predict(rf.2011, valid.2011)
rf.2012 <- train(fla.all, data=train.2012, method="rf", tuneGrid=hyperparams, trControl=control)
rf.2012.pred <- predict(rf.2012, valid.2012)

rf.2011.pred2 = data.frame(valid.2011$datetime.orig, rf.2011.pred)
names(rf.2011.pred2) = c("Date", "Pred")
rf.2011.pred2 = rf.2011.pred2[order(rf.2011.pred2$Date),]
rf.2012.pred2 = data.frame(valid.2012$datetime.orig, rf.2012.pred)
names(rf.2012.pred2) = c("Date", "Pred")
rf.2012.pred2 = rf.2012.pred2[order(rf.2012.pred2$Date),]
rf.both.pred = rbind(rf.2011.pred2, rf.2012.pred2)

valid2 = valid[order(valid$datetime.orig),]

rmse(rf.both.pred$Pred, valid2$count)
rmsle(rf.both.pred$Pred, valid2$count)

# [1] 47.50565, 0.3220209

pred <- predict(rf.all, bike.test)

result = data.frame(bike.test$datetime.orig, pred)
colnames(result) = c("datetime","count")
write.csv(result,"result.csv",row.names=F,quote=F)


trainByHour <- function(dataset, features) {
  models = list()
  models$count <- list()
  #models$registers <- list()
  models$count.we <- list()
  #models$registers.we <- list()
  
  cntIdx <- grep('count', names(dataset))
  regIdx <- grep('registered', names(dataset))
  casIdx <- grep('casual', names(dataset))
    
  for(i in c(1:24)){
    print(paste0('Calculating models for hour ', i-1))
    
    ds <- dataset[dataset$hour == (i-1),]
    
    ds.we <- ds[ds$workingday == 0,]
    ds <- ds[ds$workingday == 1,]
    
    rf <- randomForest(x=ds[,features],
                     y=ds[,cntIdx],
                     ntree=500, 
                     mtry=12)
    models$count[[i]] <- rf
    
    rf <- randomForest(x=ds[,features],
                     y=ds[,cntIdx],
                     ntree=500, 
                     mtry=12)
    models$count.we[[i]] <- rf
  }
  
  return(models)
}

predictByHour <- function(models, dataset) {
  result <- data.frame(datetime = list(), count = list())
  
  for(i in c(1:24)){
    print(paste0('Calculating predictions for hour ', i-1))
    
    ds <- dataset[dataset$hour == (i-1),]
    
    ds.we <- ds[ds$workingday == 0,]
    ds <- ds[ds$workingday == 1,]
    
    pred <- predict(models$count[[i]], ds)
    pred.we <- predict(models$count.we[[i]], ds.we)
    
    df <- data.frame(datetime=ds$datetime, count=as.vector(pred))
    df.we <- data.frame(datetime=ds.we$datetime, count=as.vector(pred.we))
    
    result <- rbind(result, df, df.we)
  }
  
  result$count[result$count<0] <- 0
  result$count[is.na(result$count)] <- 0
  
  result <- result[order(result$datetime),]
  
  return (result)
}
