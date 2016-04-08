# libraries
library(ggplot2)
library(glmnet)
library(tree)
library(randomForest)
library(boot)
library(quantreg)
library (ISLR)
library(plsdof)
library(pls)
set.seed(1)

# loading in data
dust <- read.csv('/Users/edithho/Google Drive/cal/
                 2016 spring/air-quality-sensor_testing/Dust Sensor Comparison.csv')
# pairs(dust[,4:11])
head(dust)
X = dust[,4:10]
Y = dust$ppd60_3
n = nrow(X)
train = sample(1:nrow(dust), (nrow(dust) * .7))
valid = -train

## linear regression
lm.mod = lm(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust)
summary(lm.mod)

lm.mod2 = lm(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust[train,])
summary(lm.mod2)
#plot(lm.mod2)

yhat_lm = predict(lm.mod2, newdata = X[valid,])
plot(yhat_lm, type='l', main="Linear Regression",
     col='cornflowerblue',ylab="Values")
par(new=T)
plot(Y[valid], col='firebrick1',type='l', ylab='')
par(new=F)

# accuracy
MSE.lm <- (sum((Y[valid] - yhat_lm)^2))/length(yhat_lm)
MSE.lm 
yhat_interval = predict(lm.mod, newdata = X[valid,], interval ='prediction')
yhat_confit = predict(lm.mod, newdata = X[valid,], interview='confidence')

lm.mod3 = lm(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3+ppd60_1+ppd60_2, dust[train,])
summary(lm.mod3)
MSE.lm3 <- (sum((Y[valid] - predict(lm.mod3, newdata = X[valid,]))^2))/length(Y[valid])
MSE.lm3


#glm.fit <- glm(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust[train,])
#cv.err <- cv.glm(dust[train,], glm.fit)

## random forest
# all predictors

dust.rf <- randomForest(ppd60_3~temperature+humidity+ppd42_1+ppd42_2
                        +ppd42_3+ppd60_1+ppd60_2, data=dust, subset=train,
                        mtry=7, ntree=25)
dust.rf
yhat.rf <- predict(dust.rf, newdata=X[valid,])
plot(yhat.rf, type='l', main="Random Forest",
     col='cornflowerblue',ylab="Values")
par(new=T)
plot(Y[valid], col='firebrick1',type='l', ylab='')
par(new=F)
importance(dust.rf)
varImpPlot(dust.rf)

MSE.rf1 <- mean((Y[valid] - yhat.rf)^2)
plot(yhat.rf, Y[valid])
abline(0,1)

# without the two pp60
dust.rf2 <- randomForest(ppd60_3~temperature+humidity+ppd42_1+ppd42_2
                         +ppd42_3, data=dust, subset=train,
                         mtry=5, ntree=25)

dust.rf3 <- randomForest(dust[train,c(4:8)], dust$ppd60_3[train],
                         mtry=5, ntree=25)

dust.rf2
yhat.rf2 <- predict(dust.rf2, newdata=X[valid,])
plot(yhat.rf2, type='l', main="Random Forest",
     col='cornflowerblue',ylab="Values")
par(new=T)
plot(Y[valid], col='firebrick1',type='l', ylab='')
par(new=F)
MSE.rf2 <- mean((Y[valid] - yhat.rf2)^2) #higher MSE but expected
plot(yhat.rf2, Y[valid])
abline(0,1)
importance(dust.rf2)
varImpPlot(dust.rf2)


## regression tree/random forest
tree.dust <- tree(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust)
summary(tree.dust)
plot(tree.dust)
text(tree.dust)
tree.dust

tree.dust.train <- tree(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust, subset = train)
tree.pred <- predict(tree.dust.train, dust[valid,])


## jim's

#PCR
d = dust[,4:11]
pcr.fit=pcr(ppd60_3~., data=d, subset=train, scale=TRUE, validation="CV")
res = MSEP(pcr.fit)
pcr.best=which.min(res$val[1,,])-1
cv = res$val[,,pcr.best+1]
pcr.pred=predict(pcr.fit, X[valid,], ncomp=pcr.best)
MSE.pcr <- (sum((Y[valid] - pcr.pred)^2))/length(pcr.pred)
print(MSE.pcr)

#L1 Regression
model.rq <- rq(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, data = dust[train,], tau = .5)
print(model.rq)
pre = predict(model.rq, newdata=X[valid,])
MSE.l1reg <- (sum((Y[valid] - pre)^2))/length(pre)
MSE.l1reg

##yc
###ridge
grid=10^seq(-1,-3,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))  #6*100

#cv
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam1=cv.out$lambda.min #bestlam:lambda that results in the smallest cv error

ridge.pred=predict(ridge.mod,s=bestlam1,newx=x[test,])
MSE.ridge <- mean((ridge.pred-y.test)^2) # MSE associated with the best lambda
mean(abs(ridge.pred-y.test)) # MAE associated with the best lambda
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam1)[1:6,]


###lasso
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
dim(coef(lasso.mod))
plot(lasso.mod)

cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam2=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam2,newx=x[test,])
MSE.lasso <- mean((lasso.pred-y.test)^2) #MSE
mean(abs(lasso.pred-y.test)) #MAE
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam2)[1:6,]
lasso.coef
lasso.coef[lasso.coef!=0]

min(c(MSE.lm, MSE.lm3, MSE.rf1, MSE.rf2, MSE.pcr, MSE.l1reg, MSE.ridge, MSE.lasso))
