＃＃＃#simulating data
m <- 100
n <- 5
x <- matrix(rnorm(m*n, mean=0, sd=1),nrow = m, ncol= n)  #simulating the matrix, with n=5 for tem,hum,ppd42_1,pd42_2,ppd42_3, m=100:observations.
y <- rnorm(m, mean=0.06, sd=0.02)  #simulating ppd60_1 

###ridge
library(glmnet)
grid=10^seq(-1,-3,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))  #6*100

#cv
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam1=cv.out$lambda.min #bestlam:lambda that results in the smallest cv error

ridge.pred=predict(ridge.mod,s=bestlam1,newx=x[test,])
mean((ridge.pred-y.test)^2) # MSE associated with the best lambda
mean(abs(ridge.pred-y.test)) # MAE associated with the best lambda
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam1)[1:6,]


###lasso
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
dim(coef(lasso.mod))
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam2=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam2,newx=x[test,])
mean((lasso.pred-y.test)^2) #MSE
mean(abs(lasso.pred-y.test)) #MAE
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam2)[1:6,]
lasso.coef
lasso.coef[lasso.coef!=0]
