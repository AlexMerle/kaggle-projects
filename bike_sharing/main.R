rm(list=ls())
gc()
setwd("C:\\MyDocs\\kaggle\\bike_sharing")
load(".RData")


## load files
bike.train = load_file("data\\train.csv")
bike.test = load_file("data\\test.csv")

## preprocess
bike.train = preprocess(bike.train)
bike.train = fix.train(bike.train)

bike.test = preprocess(bike.test)
bike.test = fix.test(bike.test)

train = get.cv_train(bike.train)
valid = get.cv_valid(bike.train)

## train
features = c("date", "humidity", "temp", "atemp", "windspeed", "workingday", "weather", "season", "hour", "year", "month", "dayofweek", "monthyear", "daynumber")
models = trainByHour(train, features)
result = predictByHour(models, valid)
rmsle(result$count, valid[order(valid$datetime),]$count)
