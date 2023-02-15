
#ridge regression
library(glmnet)
x = model.matrix(HeartDisease~., heartdata)[,-1]
y = heartdata$HeartDisease ; length(y)
grid = 10^seq(10,-2, length = 100)
ridge.mod = glmnet(x,y, alpha = 0, lambda = grid)
head(coef(ridge.mod))

#best lambda using k-fold cv
cv.out = cv.glmnet(x[train,], y[train], alpha =0)
plot(cv.out)
(best.lambda = cv.out$lambda.min)
ridge.pred = predict(ridge.mod, s = best.lambda, newx = x[test,])
mean((ridge.pred - y.test)^2)

pred = predict(cv.out, newx = x[test,], type = 'response')
perf = performance(prediction(pred, y), 'tpr','fpr')

fitted=attributes(predict(ridge.mod,heartdata[train,],decision.values=TRUE))$decision.values
par(mfrow=c(1,2))
auc1 = rocplot(fitted,heartdata[train,"HeartDisease"],main="Training Data")

#전체 데이터 fitting
out = glmnet(x,y, alpha = 0)
ridge.pred = predict(out, newx = x[test,],type = 'response', s=best.lambda)
head(ridge.pred)
library(pROC)
auc(heartdata[test,]$HeartDisease, ridge.pred)

ridge.pred.mat = matrix(ridge.pred)
t(ridge.pred.mat)

x.mat = matrix(x, ncol = 15) ; dim(x.mat)
x.mat
ridge.pred %*% x
length(ridge.pred)
dim(x)
head(x)


#lasso
lasso.mod = glmnet(x[train, ], y[train], alpha = 1, lambda = grid )
lasso.pred = predict(lasso.mod, s =4,  newx = x[test,]) 

#best lambda using k-fold cv
cv.out1 = cv.glmnet(x[train,], y[train], alpha =1)
plot(cv.out1)
(best.lambda1 = cv.out1$lambda.min)
lasso.pred = predict(lasso.mod, s = best.lambda1, newx = x[test,]) ; lasso.pred
perf <- performance(prediction(lasso.pred, y.test), 'tpr', 'fpr')

#전체 데이터 fit
out1 = glmnet(x,y, alpha =1)
lasso.pred = predict(out1, newx = x[test,],type = 'response', s=best.lambda1)
auc(heartdata[test,]$HeartDisease, lasso.pred)

lasso.pred = predict(out1, type = 'coefficients', s=best.lambda1) ; lasso.pred
lasso.pred[lasso.pred!= 0]
coef.lasso = predict(lasso.pred, x[test,], type = 'class', s = best.lambda1)
