library(ggplot2)
library(glmnet)
library(tree)
library(randomForest)
library(boot)

## linear regression
dust <- read.csv('/Users/edithho/Google Drive/cal/2016 spring/air-quality-sensor_testing/Dust Sensor Comparison.csv')
# pairs(dust[,4:11])
head(dust)
X = dust[,4:10]
Y = dust$ppd60_3
n = nrow(X)
train = sample(1:nrow(dust), (nrow(dust) * .7))
valid = -train

lm.mod = lm(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust)
summary(lm.mod)

lm.mod2 = lm(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust[train,])
summary(lm.mod2)
plot(lm.mod2)

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


glm.fit <- glm(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust[train,])
cv.err <- cv.glm(dust[train,], glm.fit)

## random forest
# all predictors
set.seed(1)
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

mean((Y[valid] - yhat.rf)^2)
plot(yhat.rf, Y[valid])
abline(0,1)

# without the two pp60
dust.rf2 <- randomForest(ppd60_3~temperature+humidity+ppd42_1+ppd42_2
                        +ppd42_3, data=dust, subset=train,
                        mtry=5, ntree=25)
dust.rf2
yhat.rf2 <- predict(dust.rf2, newdata=X[valid,])
plot(yhat.rf2, type='l', main="Random Forest",
     col='cornflowerblue',ylab="Values")
par(new=T)
plot(Y[valid], col='firebrick1',type='l', ylab='')
par(new=F)
mean((Y[valid] - yhat.rf2)^2) #higher MSE but expected
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


