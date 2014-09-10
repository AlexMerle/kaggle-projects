setwd("C:\\MyDocs\\kaggle\\forest")
forest.train = read.csv("train.csv")
forest.test = read.csv("test.csv")
forest.train$Cover = as.factor(forest.train$Cover)