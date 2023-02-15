library(pls)

pls.fit = plsr(HeartDisease~., data = heartdata, subset = train, scale = T, validation = 'CV')
summary(pls.fit)

validationplot(pls.fit, val.type = 'MSEP')
pls.pred = predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2)

pls.fitall = plsr(HeartDisease~., data = heartdata,scale = T, ncomp =3)
summary(pls.fitall)

auc(heartdata$HeartDisease, pls.fitall)
