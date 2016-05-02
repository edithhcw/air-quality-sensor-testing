# This code is written by Edith Ho, Jim Bai, and Yachuan Liu  in April 2016,
# under the supervision of Dilek Uz, for a research project in Berkeley, CA.
# This script provides some basic machine learning algorithms, and selects 
# the best one (lowest MSE) for predicting air quality with a censor.

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

dust <- read.csv('/Users/edithho/Google Drive/cal/2016 spring/air-quality-sensor-testing/Dust Sensor Comparison.csv')
X = dust[,4:6]
Y = dust$ppd60_3
n = nrow(X)
train = sample(1:nrow(dust), (nrow(dust) * .7))
valid = -train


# Machine Learning Models
# linear regression
LinearReg <- function(dust, X, Y, train, valid, relation) {
	lm.mod2 = lm(relation, dust[train,])
	yhat_lm = predict(lm.mod2, newdata = X[valid,])
	plot(yhat_lm, type = 'l', main = "Linear Regression",
     	col = 'cornflowerblue',ylab = "Values")
	par(new = T)
	plot(Y[valid], col = 'orange', type = 'l', ylab = '', xaxt='n', yaxt='n')
	par(new = T)
	plot(X[,3], col = 'firebrick1', type = 'l', ylab = '', xaxt='n', yaxt='n')
	legend("topleft",c('Predicted', 'Expensive', 'Cheap'), bty = "n",
	       lty=c(1,1),lwd=c(2.5,2.5),col=c('cornflowerblue', 'orange', 'firebrick1')) 
	par(new = F)

	MSE.lm = (sum((Y[valid] - yhat_lm)^2)) / length(yhat_lm)
	
	ret = list("model" = lm.mod2, "error" = MSE.lm, "name" = "Linear Regression")
	return(ret)
}

# Random forest
RanForest <- function(dust, X, Y, train, valid, relation) {
	dust.rf = randomForest(relation, data = dust, subset = train,
	                        ntree = 100)
	yhat.rf = predict(dust.rf, newdata = X[valid,])
	plot(yhat.rf, type = 'l', main = "Random Forest",
	     col = 'cornflowerblue',ylab = "Values")
	par(new = T)
	plot(Y[valid], col = 'orange', type = 'l', ylab = '', xaxt='n', yaxt='n')
	par(new = T)
	plot(X[,3], col = 'firebrick1', type = 'l', ylab = '', xaxt='n', yaxt='n')
	legend("topleft",c('Predicted', 'Expensive', 'Cheap'), bty = "n",
	       lty=c(1,1),lwd=c(2.5,2.5),col=c('cornflowerblue', 'orange', 'firebrick1')) 
	par(new = F)
	importance(dust.rf)
	varImpPlot(dust.rf)
	MSE.rf1 = mean((Y[valid] - yhat.rf)^2)

	ret = list("model" = dust.rf, "error" = MSE.rf1, 'importance' = importance(dust.rf),
	            "name" = "Random Forest")
	return(ret)
}

# The Principal Components Regression
PCR <- function(dust, X, Y, train, valid, relation) {
	pcr.fit = pcr(relation, data = dust, subset = train, scale = TRUE, validation = "CV")
	res = MSEP(pcr.fit)
	pcr.best = which.min(res$val[1,,]) - 1
	cv = res$val[,,pcr.best+1]
	pcr.pred = predict(pcr.fit, X[valid,], ncomp = pcr.best)
	MSE.pcr = (sum((Y[valid] - pcr.pred)^2)) / length(pcr.pred)
	plot(pcr.pred, type = 'l', main = "Principal Components Regression",
	     col = 'cornflowerblue',ylab = "Values")
	par(new = T)
	plot(Y[valid], col = 'orange', type = 'l', ylab = '', xaxt='n', yaxt='n')
	par(new = T)
	plot(X[,3], col = 'firebrick1', type = 'l', ylab = '', xaxt='n', yaxt='n')
	legend("topleft",c('Predicted', 'Expensive', 'Cheap'), bty = "n",
	       lty=c(1,1),lwd=c(2.5,2.5),col=c('cornflowerblue', 'orange', 'firebrick1')) 
	par(new = F)
	
	ret = list("model" = pcr.fit, "error" = MSE.pcr, "ncomp" = pcr.best,  "name" = "Principal Components Regression")
	return(ret)
}

# L1 Linear Regression
l1LinearReg <- function(dust, X, Y, train, valid, relation) {
	model.rq = rq(relation, data = dust[train,], tau = .5)
	pre = predict(model.rq, newdata = X[valid,])
	MSE.l1reg = (sum((Y[valid] - pre)^2)) / length(pre)
	plot(pre, type = 'l', main = "L1 Linear Regression",
	     col = 'cornflowerblue',ylab = "Values")
	par(new = T)
	plot(Y[valid], col = 'orange', type = 'l', ylab = '', xaxt='n', yaxt='n')
	par(new = T)
	plot(X[,3], col = 'firebrick1', type = 'l', ylab = '', xaxt='n', yaxt='n')
	legend("topleft",c('Predicted', 'Expensive', 'Cheap'), bty = "n",
	       lty=c(1,1),lwd=c(2.5,2.5),col=c('cornflowerblue', 'orange', 'firebrick1')) 
	par(new = F)
	
	ret = list("model" = model.rq, "error" = MSE.l1reg, "name" = "L1 Linear Regression")
	return(ret)
}

# Ridge Regression
Ridge <- function(dust, X, Y, train, valid) {
  	grid = 10^seq(-1,-3,length = 100)
  	X = as.matrix(X)
  	ridge.mod = glmnet(X, Y, alpha = 0, lambda = grid)
  	cv.out = cv.glmnet(X[train,], Y[train], alpha = 0)
  	bestlam1 = cv.out$lambda.min
  	ridge.pred = predict(ridge.mod,s = bestlam1, newx = X[valid,])
  	MSE.ridge = mean((ridge.pred-Y[valid])^2)
  	plot(ridge.pred, type = 'l', main = "Ridge Regression",
  	     col = 'cornflowerblue',ylab = "Values")
  	par(new = T)
  	plot(Y[valid], col = 'orange', type = 'l', ylab = '', xaxt='n', yaxt='n')
  	par(new = T)
  	plot(X[,3], col = 'firebrick1', type = 'l', ylab = '', xaxt='n', yaxt='n')
  	legend("topleft",c('Predicted', 'Expensive', 'Cheap'), bty = "n",
  	       lty=c(1,1),lwd=c(2.5,2.5),col=c('cornflowerblue', 'orange', 'firebrick1')) 
  	par(new = F)
  	
  	ret = list("model" = ridge.mod, "error" = MSE.ridge, "bestlam" = bestlam1,  "name" = "Ridge Regression")
  	return(ret)
}

# Lasso Regression
Lasso <- function(dust, X, Y, train, valid) {
  	X = as.matrix(X)
  	lasso.mod = glmnet(X[train,], Y[train], alpha=1, lambda=grid)
  	cv.out = cv.glmnet(X[train,], Y[train], alpha=1)
  	bestlam2 = cv.out$lambda.min
  	lasso.pred = predict(lasso.mod, s = bestlam2, newx = X[valid,])
  	MSE.lasso = mean((lasso.pred-Y[valid])^2)
  	plot(lasso.pred, type = 'l', main = "LASSO Regression",
  	     col = 'cornflowerblue',ylab = "Values")
  	par(new = T)
  	plot(Y[valid], col = 'orange', type = 'l', ylab = '', xaxt='n', yaxt='n')
  	par(new = T)
  	plot(X[,3], col = 'firebrick1', type = 'l', ylab = '', xaxt='n', yaxt='n')
  	legend("topleft",c('Predicted', 'Expensive', 'Cheap'), bty = "n",
  	       lty=c(1,1),lwd=c(2.5,2.5),col=c('cornflowerblue', 'orange', 'firebrick1')) 
  	par(new = F)

	ret <- list("model" = lasso.mod, "error" = MSE.lasso, "bestlam" = bestlam2,  "name" = "Lasso Regression")
  	return(ret)
}


relation = formula(ppd60_3 ~ temperature + humidity + ppd42_1)
result.lm = LinearReg(dust, X, Y, train, valid, relation)
result.rf = RanForest(dust, X, Y, train, valid, relation)
result.pcr = PCR(dust, X, Y, train, valid, relation)
result.l1 = l1LinearReg(dust, X, Y, train, valid, relation)
result.ridge = Ridge(dust, X, Y, train, valid)
result.lasso = Lasso(dust, X, Y, train, valid)
result = list(result.lm,　result.rf, result.pcr, result.l1, result.ridge, result.lasso)
min = result[[1]]$error
index = 1
for (i in 1:6){
	if (min > result[[i]]$error) {
		min = result[[i]]$error
		index = i
	}
}

cat("The model with the least error is: ", result[[index]]$name)
cat("The error rate is: ", min)

