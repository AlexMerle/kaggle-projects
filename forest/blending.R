
set.seed(100)
index.blend.valid = sample(1:nrow(train), nrow(train)/4)
index.blend.train = -index.blend.valid
blend.train = train[index.blend.train,]
blend.valid = train[index.blend.valid,]
blend.train.orig = train.orig[index.blend.train,]
blend.valid.orig = train.orig[index.blend.valid,]



rf <- train(formula.simple, data=blend.train.orig, method="rf", trControl=control, linout=FALSE, trace=TRUE)
rf.pred <- predict(rf, blend.valid.orig)
mean(rf.pred != blend.valid.orig$Cover)
table(blend.valid.orig$Cover, rf.pred)

rf.asp <- train(formula.noaspect, data=blend.train.orig, method="rf", trControl=control, linout=FALSE, trace=TRUE)
rf.asp.pred <- predict(rf.asp, blend.valid.orig)
mean(rf.asp.pred != blend.valid.orig$Cover)
table(blend.valid.orig$Cover, rf.asp.pred)

rf.soil <- train(formula.nosoil, data=blend.train, method="rf", trControl=control, linout=FALSE, trace=TRUE)
rf.soil.pred <- predict(rf.soil, blend.valid)
mean(rf.soil.pred != blend.valid$Cover)
table(blend.valid$Cover, rf.soil.pred)

rf.best <- train(formula.best, data=blend.train.orig, method="rf", trControl=control, linout=FALSE, trace=TRUE)
rf.best.pred <- predict(rf.best, blend.valid.orig)
mean(rf.best.pred != blend.valid.orig$Cover)
table(blend.valid.orig$Cover, rf.best.pred)

#hyperparams <- expand.grid(mtry=c(12)) # 12
rf.soiloptim <- train(formula.soiloptim2, data=blend.train, method="rf", trControl=control, linout=FALSE, trace=TRUE)
rf.soiloptim.pred <- predict(rf.soiloptim, blend.valid)
mean(rf.soiloptim.pred != blend.valid$Cover)
table(blend.valid$Cover, rf.soiloptim.pred)

hyperparams.svm <- expand.grid(.sigma=c(0.013,0.026,0.052,0.1,0.5,1),.C=c(2,3,5,8,16,32,64))
svm <- train(formula.soiloptim2, data=blend.train, method="svmRadial", tuneGrid=hyperparams.svm, trControl=control, tuneLength=5)
svm.pred <- predict(svm, blend.valid)
mean(svm.pred != blend.valid$Cover)
table(blend.valid$Cover, svm.pred)




bindedResults = cbind(rf.pred, rf.asp.pred, rf.soil.pred, rf.best.pred, rf.soiloptim.pred, svm.pred, blend.valid$Cover)
colnames(bindedResults)[7] = "Cover"

rf.blend <- train(Cover ~ ., data=blend.valid, method="rf", trControl=control, linout=FALSE, trace=TRUE)
rf.blend.pred <- predict(rf.blend, valid)
mean(rf.blend.pred != valid$Cover)
table(valid$Cover, rf.blend.pred)



