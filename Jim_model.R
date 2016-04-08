library(quantreg)
library (ISLR)
library(plsdof)
library(pls)
dust <- read.csv('/Users/jimbai/Desktop/urapg/sensor/dust.csv')
set.seed(1)

d <- dust[,4:11]
pairs(dust[,4:11])

X = dust[,4:10]
Y = dust$ppd60_3
n = nrow(X)
train = 1:round(n*.7)
valid = round(n*.7 + 1):n

#PCR
set.seed(1)
pcr.fit=pcr(ppd60_3~., data=d, subset=train, scale=TRUE, validation="CV")
res = MSEP(pcr.fit)
pcr.best=which.min(res$val[1,,])-1
cv = res$val[,,pcr.best+1]
pcr.pred=predict(pcr.fit, X[valid,], ncomp=pcr.best)
MSE <- (sum((Y[valid] - pcr.pred)^2))/length(pcr.pred)
print(MSE)

#L1 Regression
model.rq <- rq(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, data = dust[train,], tau = .5)
print(model.rq)
pre = predict(model.rq, newdata=X[valid,])
MSE2 <- (sum((test$ppd60_3 - pre)^2))/length(pre)
MSE2