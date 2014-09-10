setwd("C:\\MyDocs\\kaggle\\forest")
load(".RData")

## train & validation sets

set.seed(100)
index.valid = sample(1:nrow(forest.train), nrow(forest.train)/5)
index.train = -index.valid
train = forest.train[index.train,]
valid = forest.train[index.valid,]
train.orig = forest.train.orig[index.train,]
valid.orig = forest.train.orig[index.valid,]

## prediction & pruning

library("rpart")
dtree = rpart(Cover~.-Id, data=train, method="class")
printcp(dtree)
plot(dtree)
text(dtree, pretty=0)

plotcp(dtree)

dtree.pred = predict(dtree, valid, type="class")
mean(dtree.pred != valid$Cover) #37%

dtree.p1 = prune(dtree, cp=0.005)
dtree.p1.pred = predict(dtree.p1, valid, type="class")
mean(dtree.p1.pred != valid$Cover) #37%


## prediction

dtree2 = rpart(Cover~., data=forest.train, method="class")
plotcp(dtree2)
dtree2.pred = predict(dtree2, forest.test, type="class")

result = data.frame(forest.test$Id, dtree2.pred)
colnames(result) = c("Id","Cover_Type")
write.csv(result,"result.csv",row.names=F,quote=F)




#### Random Forest

library(randomForest)

rf = randomForest(Cover~.-Id, data=train, ntree=1000)
print(rf)
importance(rf)
plot(rf)
plot(importance(rf), lty=2, pch=16)
lines(importance(rf))
varImpPlot(rf)

library(ROCR)
bestmtry <- tuneRF(subset(train, select=-c(Id,Cover)),train$Cover, ntreeTry=100, 
                   stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE) # mtry=22

# !!!
rf = randomForest(Cover~.-Id-Aspect, data=train, mtry=22, ntree=1000)


rf = randomForest(Cover~Elevation+Slope+HydrologyHorDist+HydrologyVerDist+
                    RoadwaysHorDist+FirePointsHorDist+Hillshade9am+HillshadeNoon+
                    Hillshade3pm+Wilderness1+Wilderness2+Wilderness3+Wilderness4+
                    AspectN+AspectNE+AspectE+AspectSE+AspectS+AspectSW+AspectW+AspectNW+
                    SoilStony+SoilVeryStony+SoilExtremStony+SoilRubbly+SoilBouldery+
                    SoilCryaquolis+SoilCryaquolls+Soil7+Soil8+Soil14+Soil15+Soil16+Soil35,
                  data=train, mtry=22, ntree=1000)


rf.pred <- predict(rf, valid)
mean(rf.pred != valid$Cover)
# ~19% for ntree=500, 18.8% for ntree=1000, 15.5% for ntree=1000 and mtry=22, 15% for Aspect features
table(valid$Cover, rf.pred)

# predict

rf2 = randomForest(formula.soiloptim2, data=forest.train, mtry=14, ntree=1000)
rf2.pred <- predict(rf2, forest.test)

result2 = data.frame(forest.test$Id, rf2.pred)
colnames(result2) = c("Id","Cover_Type")
write.csv(result2,"result3.csv",row.names=F,quote=F)



# caret

formula.simple = Cover~Elevation+Aspect+Slope+HydrologyHorDist+HydrologyVerDist+
  RoadwaysHorDist+FirePointsHorDist+Hillshade9am+HillshadeNoon+Hillshade3pm+
  Wilderness1+Wilderness2+Wilderness3+Wilderness4+
  Soil1+Soil2+Soil3+Soil4+Soil5+Soil6+Soil7+Soil8+Soil9+Soil10+Soil11+Soil12+Soil13+
  Soil14+Soil15+Soil16+Soil17+Soil18+Soil19+Soil20+Soil21+Soil22+Soil23+Soil24+Soil25+
  Soil26+Soil27+Soil28+Soil29+Soil30+Soil31+Soil32+Soil33+Soil34+Soil35+Soil36+Soil37+
  Soil38+Soil39+Soil40

formula.soiloptim = Cover~Elevation+Aspect+Slope+HydrologyHorDist+HydrologyVerDist+
  RoadwaysHorDist+FirePointsHorDist+Hillshade9am+HillshadeNoon+Hillshade3pm+
  Wilderness1+Wilderness2+Wilderness3+Wilderness4+
  Soil1+Soil2+Soil3+Soil4+Soil8+Soil9+Soil10+Soil13+Soil18+Soil19+Soil20+
  Soil21+Soil22+Soil23+Soil25+Soil29+Soil32+Soil34+Soil36+Soil37

formula.soiloptim2 = Cover~Elevation+Aspect+Slope+HydrologyHorDist+HydrologyVerDist+
  RoadwaysHorDist+FirePointsHorDist+Hillshade9am+HillshadeNoon+Hillshade3pm+
  Wilderness1+Wilderness2+Wilderness3+Wilderness4+
  Soil1+Soil2+Soil3+Soil4+Soil9+Soil10+Soil13+Soil18+Soil19+Soil20+
  Soil21+Soil22+Soil23+Soil29+Soil32+Soil34+Soil36+Soil37


formula.soilaggr = Cover~Elevation+Aspect+Slope+HydrologyHorDist+HydrologyVerDist+
  RoadwaysHorDist+FirePointsHorDist+Hillshade9am+HillshadeNoon+Hillshade3pm+
  Wilderness1+Wilderness2+Wilderness3+Wilderness4+
  SoilType

formula.nosoil = Cover~Elevation+Aspect+Slope+HydrologyHorDist+HydrologyVerDist+
  RoadwaysHorDist+FirePointsHorDist+Hillshade9am+HillshadeNoon+Hillshade3pm+
  Wilderness1+Wilderness2+Wilderness3+Wilderness4

formula.best = Cover~Elevation+Aspect+Slope+HydrologyHorDist+HydrologyVerDist+
  RoadwaysHorDist+FirePointsHorDist+Hillshade9am+HillshadeNoon+Hillshade3pm+
  Wilderness1+Wilderness3+Wilderness4+
  Soil3+Soil4+Soil10+Soil38+Soil39

  
formula.noaspect = Cover~Elevation+Slope+HydrologyHorDist+HydrologyVerDist+
  RoadwaysHorDist+FirePointsHorDist+Hillshade9am+HillshadeNoon+Hillshade3pm+
  Wilderness1+Wilderness2+Wilderness3+Wilderness4+
  AspectN+AspectNW+AspectW+AspectSW+AspectS+AspectSE+AspectE+AspectNE+
  Soil1+Soil2+Soil3+Soil4+Soil5+Soil6+Soil7+Soil8+Soil9+Soil10+Soil11+Soil12+Soil13+
  Soil14+Soil15+Soil16+Soil17+Soil18+Soil19+Soil20+Soil21+Soil22+Soil23+Soil24+Soil25+
  Soil26+Soil27+Soil28+Soil29+Soil30+Soil31+Soil32+Soil33+Soil34+Soil35+Soil36+Soil37+
  Soil38+Soil39+Soil40

formula.sv = Cover~Elevation+Slope+HydrologyHorDist+HydrologyVerDist+
  RoadwaysHorDist+FirePointsHorDist+Hillshade9am+HillshadeNoon+Hillshade3pm+
  Wilderness+AspectFact+SoilType

formula.sv.hill = Cover~Elevation+Slope+HydrologyHorDist+HydrologyVerDist+
  RoadwaysHorDist+FirePointsHorDist+Hillshade+Wilderness+AspectFact+SoilType

  
formula.noWA = Cover~Elevation+Aspect+Slope+HydrologyHorDist+HydrologyVerDist+
  RoadwaysHorDist+FirePointsHorDist+Hillshade9am+HillshadeNoon+Hillshade3pm+
  Soil1+Soil2+Soil3+Soil4+Soil5+Soil6+Soil7+Soil8+Soil9+Soil10+Soil11+Soil12+Soil13+
  Soil14+Soil15+Soil16+Soil17+Soil18+Soil19+Soil20+Soil21+Soil22+Soil23+Soil24+Soil25+
  Soil26+Soil27+Soil28+Soil29+Soil30+Soil31+Soil32+Soil33+Soil34+Soil35+Soil36+Soil37+
  Soil38+Soil39+Soil40

library(caret)

set.seed(100)
control <- trainControl(method="cv", number=5)
hyperparams <- expand.grid(mtry=c(22))

rf <- train(formula.simple, data=train.orig, method="rf", tuneGrid=hyperparams, trControl=control, linout=FALSE, trace=TRUE)
rf$results
plot(rf)
rf.pred <- predict(rf, valid.orig)
mean(rf.pred != valid.orig$Cover)
table(valid.orig$Cover, rf.pred)
varImpPlot(rf$finalModel)

rf.asp <- train(formula.noaspect, data=train.orig, method="rf", trControl=control, linout=FALSE, trace=TRUE)
rf.asp$results
rf.asp.pred <- predict(rf.asp, valid.orig)
mean(rf.asp.pred != valid.orig$Cover)
table(valid.orig$Cover, rf.asp.pred)

rf.soil <- train(formula.nosoil, data=train, method="rf", trControl=control, linout=FALSE, trace=TRUE)
rf.soil$results
rf.soil.pred <- predict(rf.soil, valid)
mean(rf.soil.pred != valid$Cover)
table(valid$Cover, rf.soil.pred)

rf.best <- train(formula.best, data=train.orig, method="rf", trControl=control, linout=FALSE, trace=TRUE)
rf.best$results
rf.best.pred <- predict(rf.best, valid.orig)
mean(rf.best.pred != valid.orig$Cover)
table(valid.orig$Cover, rf.best.pred)

rf.soilaggr <- train(formula.soilaggr, data=train, method="rf", trControl=control, linout=FALSE, trace=TRUE)
rf.soilaggr$results
rf.soilaggr.pred <- predict(rf.soilaggr, valid)
mean(rf.soilaggr.pred != valid$Cover)

hyperparams <- expand.grid(mtry=c(12)) # 12
rf.soiloptim <- train(formula.soiloptim2, data=train, method="rf", tuneGrid=hyperparams, trControl=control, linout=FALSE, trace=TRUE)
rf.soiloptim$results
rf.soiloptim.pred <- predict(rf.soiloptim, valid)
mean(rf.soiloptim.pred != valid$Cover)
table(valid$Cover, rf.soiloptim.pred)




# split by Wilderness area
forest.train.WA1 = forest.train[forest.train$Wilderness1 == 1,]
forest.train.WA1$Cover = factor(forest.train.WA1$Cover)
forest.train.WA2 = forest.train[forest.train$Wilderness2 == 1,]
forest.train.WA2$Cover = factor(forest.train.WA2$Cover)
forest.train.WA3 = forest.train[forest.train$Wilderness3 == 1,]
forest.train.WA3$Cover = factor(forest.train.WA3$Cover)
forest.train.WA4 = forest.train[forest.train$Wilderness4 == 1,]
forest.train.WA4$Cover = factor(forest.train.WA4$Cover)

rf.WA1 <- train(formula.noWA, data=forest.train.WA1, method="rf", tuneGrid=hyperparams, trControl=control, linout=FALSE, trace=TRUE)
rf.WA2 <- train(formula.noWA, data=forest.train.WA2, method="rf", tuneGrid=hyperparams, trControl=control, linout=FALSE, trace=TRUE)
rf.WA3 <- train(formula.noWA, data=forest.train.WA3, method="rf", tuneGrid=hyperparams, trControl=control, linout=FALSE, trace=TRUE)
rf.WA4 <- train(formula.noWA, data=forest.train.WA4, method="rf", tuneGrid=hyperparams, trControl=control, linout=FALSE, trace=TRUE)

rf.WA1$results
rf.WA2$results
rf.WA3$results
rf.WA4$results

predictByWA = function(dataTrain, dataTest, formula, mtry=24, ntree=500) {
  dataTrain = forest.train
  dataTest = forest.test
  formula = formula.noWA
  mtry = 24
  ntree = 1000
  
  dataTrain.WA1 = dataTrain[dataTrain$Wilderness1 == 1,]
  dataTrain.WA1$Cover = factor(dataTrain.WA1$Cover)
  dataTrain.WA2 = dataTrain[dataTrain$Wilderness2 == 1,]
  dataTrain.WA2$Cover = factor(dataTrain.WA2$Cover)
  dataTrain.WA3 = dataTrain[dataTrain$Wilderness3 == 1,]
  dataTrain.WA3$Cover = factor(dataTrain.WA3$Cover)
  dataTrain.WA4 = dataTrain[dataTrain$Wilderness4 == 1,]
  dataTrain.WA4$Cover = factor(dataTrain.WA4$Cover)
  
  set.seed(100)
  rf.WA1 <- randomForest(formula, data=dataTrain.WA1, mtry=mtry, ntree=ntree)
  rf.WA2 <- randomForest(formula, data=dataTrain.WA2, mtry=mtry, ntree=ntree)
  rf.WA3 <- randomForest(formula, data=dataTrain.WA3, mtry=mtry, ntree=ntree)
  rf.WA4 <- randomForest(formula, data=dataTrain.WA4, mtry=mtry, ntree=ntree)
  
  dataTest.WA1 = dataTest[dataTest$Wilderness1 == 1,]
  dataTest.WA2 = dataTest[dataTest$Wilderness2 == 1,]
  dataTest.WA3 = dataTest[dataTest$Wilderness3 == 1,]
  dataTest.WA4 = dataTest[dataTest$Wilderness4 == 1,]
  
  rf.WA1.pred <- predict(rf.WA1, dataTest.WA1)
  rf.WA2.pred <- predict(rf.WA2, dataTest.WA2)
  rf.WA3.pred <- predict(rf.WA3, dataTest.WA3)
  rf.WA4.pred <- predict(rf.WA4, dataTest.WA4)
  
  res.WA1 = data.frame(dataTest.WA1$Id, rf.WA1.pred)
  colnames(res.WA1) = c("Id","Cover_Type")
  res.WA2 = data.frame(dataTest.WA2$Id, rf.WA2.pred)
  colnames(res.WA2) = c("Id","Cover_Type")
  res.WA3 = data.frame(dataTest.WA3$Id, rf.WA3.pred)
  colnames(res.WA3) = c("Id","Cover_Type")
  res.WA4 = data.frame(dataTest.WA4$Id, rf.WA4.pred)
  colnames(res.WA4) = c("Id","Cover_Type")
  
  result = rbind(res.WA1, res.WA2, res.WA3, res.WA4)
  result = result[order(result$Id),]
  
  #write.csv(result,"result.byWA.csv",row.names=F,quote=F)
  
  return(result)
}

## Extra Trees
library(extraTrees)

set.seed(100)
control <- trainControl(method="cv", number=5)
hyperparams <- expand.grid(mtry=c(28),numRandomCuts=c(6)) # tuneGrid=hyperparams, 
# 31,3   22,3    28,6 - best

et <- train(formula.noaspect, data=train.orig, method="extraTrees", tuneGrid=hyperparams, trControl=control, linout=FALSE, trace=TRUE)
et.pred <- predict(et, valid.orig)
mean(et.pred != valid.orig$Cover)
table(valid.orig$Cover, et.pred)

hyperparams.sv <- expand.grid(mtry=c(5),numRandomCuts=c(4)) # tuneGrid=hyperparams, 
# 8-5,9-7 need to try 6|7|8-6|7
# 5-6 for integer factors, 5-4 for hill

et.sv <- train(formula.sv, data=train.orig, method="extraTrees", tuneGrid=hyperparams.sv, trControl=control, linout=FALSE, trace=TRUE)
et.sv.pred <- predict(et.sv, valid.orig)
mean(et.sv.pred != valid.orig$Cover)
table(valid.orig$Cover, et.sv.pred)

et.sv.hill <- train(formula.sv.hill, data=train.orig, method="extraTrees", tuneGrid=hyperparams.sv, trControl=control, linout=FALSE, trace=TRUE)
et.sv.hill.pred <- predict(et.sv.hill, valid.orig)
mean(et.sv.hill.pred != valid.orig$Cover)
table(valid.orig$Cover, et.sv.hill.pred)

# 11.4? for 5-4
# 11.7 for 5-6

forest.train.mtx = subset(forest.train.orig, select=c("Elevation","Slope","HydrologyHorDist","HydrologyVerDist","RoadwaysHorDist","FirePointsHorDist","AspectFact","Hillshade","SoilType","Wilderness"))
forest.train.mtx = as.matrix(forest.train.mtx)
forest.test.mtx = subset(forest.test.orig, select=c("Elevation","Slope","HydrologyHorDist","HydrologyVerDist","RoadwaysHorDist","FirePointsHorDist","AspectFact","Hillshade","SoilType","Wilderness"))
forest.test.mtx = as.matrix(forest.test.mtx)

train.mtx = as.matrix(subset(train.orig, select=c("Elevation","Slope","HydrologyHorDist","HydrologyVerDist","RoadwaysHorDist","FirePointsHorDist","AspectFact","Hillshade","SoilType","Wilderness")))
valid.mtx = as.matrix(subset(valid.orig, select=c("Elevation","Slope","HydrologyHorDist","HydrologyVerDist","RoadwaysHorDist","FirePointsHorDist","AspectFact","Hillshade","SoilType","Wilderness")))

et.sv.hill.2 = extraTrees(train.mtx, train.orig$Cover, ntree=2000, mtry=5, numRandomCuts=4)
et.sv.hill.pred.2 <- predict(et.sv.hill.2, valid.mtx)
mean(et.sv.hill.pred != valid.orig$Cover)


# predict test
options( java.parameters = "-Xmx4g" )
library(extraTrees)

predictExtraTrees <- function(model, test, step) {
  count = floor(dim(test)[1] / step) + 1
  for(i in 1:count)
  {
    start = (i-1)*step+1
    if (i==count)
      end = dim(test)[1]
    else
      end = i*step
    test.piece = test[start:end,]
    print(paste(start,"-",end))
    
    pred <- predict(model, test.piece)
    
    result.piece = data.frame(test.piece$Id, pred)
    colnames(result.piece) = c("Id","Cover_Type")
    
    if (i==1)
      result = result.piece
    else
      result = rbind(result, result.piece)
    print(paste(i,"iteration calculated"))
  }
  return (result)
}

result = predictExtraTrees(et.sv.hill.2, forest.test.mtx, 50000)
write.csv(result,"result_et_hill.csv",row.names=F,quote=F)

