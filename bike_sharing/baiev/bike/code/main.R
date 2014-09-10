
rm(list=ls())
gc()
setwd('bike/code')

source('preprocess.R', echo=F)
source('rmsle.R', echo=F)
source('predict.count.R', echo=F)
source('train.count.poly.R', echo=F)
source('train.count.ert.R', echo=F)

##
## read data
##

# train
data.train <- preprocess('../data/src/train.csv')
# clear train's anomalies
c <- which(data.train$temp > 25  & data.train$atemp < 15)
data.train$atemp[c] <- data.train$temp[c] + 2
data.train$humidity[data.train$humidity==0] <- 
  mean(data.train$humidity[data.train$weather == 3 & data.train$humidity!=0])

# test
data.test <- preprocess('../data/src/test.csv')
# fix test datetime mistakes (summer time?)
data.test$sec[630] <- data.test$sec[629] + 1800
data.test$sec[3872] <- data.test$sec[3871] + 1800

##
## override data.test to validation set if needed
##
# set.seed(1)
# train.ratio <- 0.8
# train.rows <- sample.int(nrow(data.train), floor(nrow(data.train)*0.7))
# data.test <- data.train[-train.rows,]
# data.train <- data.train[train.rows,]
# data.train <- data.train[order(data.train$datetime),]
# data.test <- data.test[order(data.test$datetime),]


##
## divide sets by weather
##
data.train.good <- data.train[data.train$weather == 1,]
data.test.good <- data.test[data.test$weather == 1,]

data.train.bad <- data.train[data.train$weather == 2,]
data.test.bad <- data.test[data.test$weather == 2,]

data.train.wbad <- data.train[data.train$weather == 3,]
data.test.wbad <- data.test[data.test$weather == 3,]


##
## set options for all methods
##
options <- list(ntree = 1000,
                rf.trace = F,
                mtry = 8,
                polydegree = 10,
                trace = T,
                nn.size = 1000, 
                nn.rang = 0.1,
                nn.decay = 5e-4, 
                nn.maxit = 20,
                numRandomCuts=1,
                mtry.reg = 3,
                numRandomCuts.reg=1,
                mtry.cas = 6,
                numRandomCuts.cas=3
)

##
## determine features in model's input
##
features <- c("sec", "humidity", "temp", "atemp", "windspeed", "wday", "weather","season")

##
## models without weather dividing
##
# models <- train.count.ert(data.train, options, features)
# submit <- predict.count(models, data.test, options, features)
# rmsle(data.test$count, submit$count)


##
## call functions for training and predicting
##
models.good <- train.count.poly(data.train.good,options, features)
models.bad <- train.count.poly(data.train.bad,options, features)
models.wbad <- train.count.poly(data.train.wbad,options, features)

submit.good <- predict.count(models.good, data.test.good,options, features)
submit.bad <- predict.count(models.bad, data.test.bad,options, features)
submit.wbad <- predict.count(models.wbad, data.test.wbad,options, features)

##
## calculate error if validation set was used
##
# rmsle(data.test.good$count, submit.good$count)
# rmsle(data.test.bad$count, submit.bad$count)
# rmsle(data.test.wbad$count, submit.wbad$count)

##
## compose results
##
submit <- rbind(submit.good, submit.bad, submit.wbad)
submit <- submit[order(submit$datetime),]
# rmsle(data.test$count, submit$count)

##
## save results
##
write.csv(submit, '../data/result/submit.csv', row.names=F, quote=F)