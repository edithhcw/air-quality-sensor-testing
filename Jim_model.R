library(lars)
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
x <- as.matrix(X[train,])
y <- as.matrix(Y[train])
tx <- as.matrix(X[valid,])
ty <- as.matrix(Y[valid])
fit <- lars(x, y, type="lasso")
best_step <- fit$df[which.min(fit$RSS)]
predictions <- predict(fit, tx, s=best_step, type="fit")$fit
rmse <- mean((ty - predictions)^2)
print(rmse)