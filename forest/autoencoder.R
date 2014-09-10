library(randomForest)
library(autoencoder)

## Set up the autoencoder architecture:
nl=3 ## number of layers (default is 3: input, hidden, output)
unit.type = "logistic" ## specify the network unit type, i.e., the unit
N.input = 44 ## number of units (neurons) in the input layer (one unit per pixel)
N.hidden = 4 ## number of units in the hidden layer

lambda = 0.0005 ## weight decay parameter
beta = 6 ## weight of sparsity penalty term
rho = 0.01 ## desired sparsity parameter
epsilon <- 0.001 ## a small parameter for initialization of weights
## as small gaussian random numbers sampled from N(0,epsilon^2)
max.iterations = 100000 ## number of iterations 

cols <- names(train.orig)[12:55]

trainmatrix = data.matrix(train.orig[,cols], rownames.force = NA)
testmatrix = data.matrix(valid.orig[,cols], rownames.force = NA)

#cols2 <- names(train.set)[2:55]

autoencoder.object <- autoencode(X.train=trainmatrix,X.test=testmatrix, nl=nl,N.hidden=N.hidden,
                                 unit.type=unit.type,lambda=lambda,beta=beta,rho=rho,epsilon=epsilon,
                                 optim.method="BFGS",max.iterations=max.iterations)
#                                 ,rescale.flag=TRUE,rescaling.offset=0.001)

#output <- predict(autoencoder.object, X.input=trainmatrix, hidden.output=FALSE)$X.output
output_train <- predict(autoencoder.object, X.input=trainmatrix, hidden.output=TRUE)$X.output
output_test <- predict(autoencoder.object, X.input=testmatrix, hidden.output=TRUE)$X.output

cols1 <- names(train.orig)[2:11]
train1<-train.orig[,cols1]
train1$p1<-output_train[,1]
train1$p2<-output_train[,2]
train1$p3<-output_train[,3]
train1$p4<-output_train[,4]
train1$Cover<-train.orig$Cover

test1<-valid.orig[,cols1]
test1$p1<-output_test[,1]
test1$p2<-output_test[,2]
test1$p3<-output_test[,3]
test1$p4<-output_test[,4]

set.seed(100)
control <- trainControl(method="cv", number=5)

hyperparams.auto <- expand.grid(mtry=c(4))
clf <- train(Cover~., data=train1, method="rf", trControl=control, tuneGrid=hyperparams.auto, linout=FALSE, trace=TRUE)

#tuneRF(train1, train.orig$Cover, 1, stepFactor=2, improve=0.05, trace=TRUE)
system.time(clf <- randomForest(train1, train.orig$Cover, ntree=1000, mtry=4))
clf.pred <- predict(clf, test1)
mean(clf.pred != valid$Cover)
table(valid$Cover, clf.pred)


##real testing

realmatrix = data.matrix(forest.test.orig[,cols], rownames.force = NA)
output_real <- predict(autoencoder.object, X.input=realmatrix, hidden.output=TRUE)$X.output

cols3 <- names(forest.test.orig)[2:11]
forest.test.auto<-forest.test.orig[,cols1]
forest.test.auto$p1<-output_real[,1]
forest.test.auto$p2<-output_real[,2]
forest.test.auto$p3<-output_real[,3]
forest.test.auto$p4<-output_real[,4]

clf.real <- randomForest(Cover~., data=train1, ntree=1000, mtry=4)
clf.real.pred <- predict(clf.real, forest.test.auto)

result.auto = data.frame(forest.test$Id, clf.real.pred)
colnames(result.auto) = c("Id","Cover_Type")
write.csv(result.auto,"result4.csv",row.names=F,quote=F)








test_tree <- read.table("/home/andrew/Projects/kaggle/tree/test.csv", sep= ',', header=TRUE)
testmatrix2 = data.matrix(test_tree[,cols], rownames.force = NA)
output_test2 <- predict(autoencoder.object, X.input=testmatrix2, hidden.output=TRUE)$X.output


test2<-test_tree[,cols1]
test2$p1<-output_test2[,1]
test2$p2<-output_test2[,2]
test2$p3<-output_test2[,3]
test2$p4<-output_test2[,4]

result=predict(clf, test2)

res<-data.frame(test_tree$Id)
names(res)[1] <- "Id"
res$Cover_Type<-as.numeric(result)
write.csv(res, file = "/home/andrew/Projects/kaggle/tree/rf_encoder_result.csv",row.names=FALSE)