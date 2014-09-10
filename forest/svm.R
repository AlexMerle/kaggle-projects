library(caret)
library(kernlab)

control <- trainControl(method="cv", number=5)
# sigma-c: 0.52-64, 0.1-64
hyperparams.svm <- expand.grid(.sigma=c(0.013,0.026,0.052,0.1,0.5,1),.C=c(2,3,5,8,16,32,64))

svm <- train(formula.soiloptim2, data=train, method="svmRadial", tuneGrid=hyperparams.svm, trControl=control, tuneLength=5)
svm$results
plot(svm)
svm.pred <- predict(svm, valid)
mean(svm.pred != valid$Cover)
table(valid$Cover, svm.pred)
varImpPlot(svm$finalModel)

library(e1071)

svm.model <- svm(formula.soiloptim2, data = train, cost = 5, gamma = 1)


