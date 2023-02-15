head(heartdata)

library(leaps)
regfit.full = regsubsets(HeartDisease~., heartdata)
reg.summary = summary(regfit.full)
regfit.full = regsubsets(HeartDisease~., heartdata, nvmax = 15)
names(reg.summary)
reg.summary$rsq
which.min(reg.summary$bic)

plot(reg.summary$adjr2)
str(heartdata)
regfit.fwd = regsubsets(HeartDisease~., heartdata, nvmax=15, method = 'forward')
summary(regfit.fwd)
regfit.bwd = regsubsets(HeartDisease~., heartdata, nvmax=15, method = 'backward')
summary(regfit.bwd)
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)
length(train) ; length(test)
regfit.best = regsubsets(HeartDisease~., heartdata[train,], nvmax = 15)
test.mat = model.matrix(HeartDisease~., heartdata[test,])
dim(test.mat)
head(test.mat)
val.errors = rep(NA, 15)
for(i in 1:15){
  coefi=coef(regfit.best,id=i) # coefficients from i-variable model
  pred=test.mat[,names(coefi)]%*%coefi # Xbeta
  val.errors[i]=mean((heartdata$HeartDisease[test] - pred)^2) # Test MSE
}
val.errors
which.min(val.errors)
regfit.best=regsubsets(HeartDisease~.,data=heartdata,nvmax=15) # perform on the full data
coef(regfit.best, 12)

#k-fold cv
k=10
folds=sample(1:k,nrow(heartdata),replace=TRUE)
cv.errors=matrix(NA,k,15, dimnames=list(NULL, paste(1:15)))
cv.errors

for (j in 1:k){
  best.fit = regsubsets(HeartDisease~., data = heartdata[folds!= j,], nvmax = 15)
  test.mat = model.matrix(HeartDisease~., heartdata[folds ==j,])
  for (i in 1:15){
    coefi=coef(best.fit, id=i) # coefficients from i-variable model
    pred=test.mat[,names(coefi)]%*%coefi # Xbeta
    cv.errors[j,i]=mean((heartdata$HeartDisease[folds ==j] - pred)^2)
  }
}
cv.errors
mean.cv.errors=apply(cv.errors,2,mean) ; mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
which.min(mean.cv.errors)
reg.best=regsubsets(HeartDisease~.,data=heartdata, nvmax=15)
coef(reg.best,12)
