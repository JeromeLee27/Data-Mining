
library(e1071)
head(heartdata)
train = sort(sample(1:918, 642))
test = setdiff(1:918, train)
tune.svm(HeartDisease ~., data= heartdata, gamma = 2^(-5:5), cost = 2^(0:5))
#가장 좋은 gamma = 0.125, cost = 1임을 확인할 수 있어.
heart.train = svm(HeartDisease~., data = heartdata[train,], 
                  type = 'C-classification', kernal = 'linear',gamma = 0.125, cost = 1)
summary(heart.train)
rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf,...)
  auc = performance(predob, measure="auc")
  auc = auc@y.values[[1]]
  auc
}

svmfit.opt=svm(HeartDisease~., data=heartdata[train,], kernel="radial",gamma=0.125, cost=1,decision.values=T) # decision.values=T 로 설정하면 f(x) 값 계산
svmfit.opt
fitted=attributes(predict(ridge.pred,heartdata[train,],decision.values=TRUE))$decision.values
fitted
par(mfrow=c(1,2))
auc1 = rocplot(fitted,heartdata[train,"HeartDisease"],main="Training Data")




svmfit.flex=svm(HeartDisease~., data=heartdata[train,], kernel="radial",gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,heartdata[train,],decision.values=T))$decision.values
auc2 = rocplot(fitted,heartdata[train,"HeartDisease"],add=T,col="red")

fitted=attributes(predict(svmfit.opt,heartdata[-train,],decision.values=T))$decision.values
auc3 = rocplot(fitted,heartdata[-train,"HeartDisease"],main="Test Data")
fitted=attributes(predict(svmfit.flex,heartdata[-train,],decision.values=T))$decision.values
auc4 = rocplot(fitted,heartdata[-train,"HeartDisease"],add=T,col="red")

cat('train data : gamma = 0.125, cost = 1=>',auc1,'\n')
cat('train data : gamma = 50, cost = 1=>',auc2,'\n')
cat('test data : gamma = 0.125, cost = 1=>',auc3,'\n')
cat('test data : gamma = 50, cost = 1=>',auc4,'\n')


