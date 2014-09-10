library(nnet)

normalize = function(data) {
  data.nn = data[,-1]
  data.nn$Elevation = data.nn$Elevation/max(data.nn$Elevation)
  data.nn$Aspect = data.nn$Aspect/max(data.nn$Aspect)
  data.nn$Slope = data.nn$Slope/max(data.nn$Slope)
  data.nn$HydrologyHorDist = data.nn$HydrologyHorDist/max(data.nn$HydrologyHorDist)
  data.nn$HydrologyVerDist = data.nn$HydrologyVerDist/max(data.nn$HydrologyVerDist)
  data.nn$RoadwaysHorDist = data.nn$RoadwaysHorDist/max(data.nn$RoadwaysHorDist)
  data.nn$FirePointsHorDist = data.nn$FirePointsHorDist/max(data.nn$FirePointsHorDist)
  data.nn$Hillshade9am = data.nn$Hillshade9am/max(data.nn$Hillshade9am)
  data.nn$HillshadeNoon = data.nn$HillshadeNoon/max(data.nn$HillshadeNoon)
  data.nn$Hillshade3pm = data.nn$Hillshade3pm/max(data.nn$Hillshade3pm)
  return(data.nn)
}

train.nn = normalize(train)
valid.nn = normalize(valid)
valid.nn = valid.nn[,-55]
forest.train.nn = normalize(forest.train)
forest.test.nn = normalize(forest.test)

nn <- nnet(Cover ~ ., data=forest.train.nn, size=60, decay=0.0005, maxit=100, MaxNWts=16000)
nn.pred <- predict(nn, forest.test.nn, type="class")
mean(nn.pred != valid$Cover) # 24.8% for 252, 23.6% for 50
table(valid$Cover,nn.pred)
plot(nn)

# caret

library(caret)
control <- trainControl(method="cv", number=5)
hyperparams_nnet <- expand.grid(.decay=c(0.0001,0.0005,0.001,0.005), .size=c(40,50,60))

nn <- train(Cover ~ ., data=forest.train.nn, method="nnet", tuneGrid=hyperparams_nnet, trControl=control, linout=FALSE, trace=TRUE, MaxNWts=16000)
plot(nn)
