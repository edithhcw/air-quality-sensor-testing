library(ggplot2)
library(glmnet)
library(tree)
library(randomForest)

## linear regression
dust <- read.csv('/Users/edithho/Downloads/Dust Sensor Comparison.csv')
pairs(dust[,4:11])
head(dust)
X = dust[,4:10]
Y = dust$ppd60_3
n = nrow(X)
train = 1:round(n*.7)
valid = round(n*.7 + 1):n

lm.mod = lm(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust)
summary(lm.mod)

lm.mod2 = lm(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust[train,])
summary(lm.mod2)
plot(lm.mod2)

yhat_lm = predict(lm.mod, newdata = X[valid,])
plot(yhat_lm, type='l', main="Linear Regression",
     col='cornflowerblue',ylab="Values")
par(new=T)
plot(Y[valid], col='firebrick1',type='l', ylab='')
par(new=F)

yhat_interval = predict(lm.mod, newdata = X[valid,], interval ='prediction')
yhat_confit = predict(lm.mod, newdata = X[valid,], interview='confidence')

## regression tree/random forest
tree.dust <- tree(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust)
summary(tree.dust)
plot(tree.dust)
text(tree.dust)
tree.dust

tree.dust.train <- tree(ppd60_3~temperature+humidity+ppd42_1+ppd42_2+ppd42_3, dust, subset = train)
tree.pred <- predict(tree.dust.train, dust[valid,])
