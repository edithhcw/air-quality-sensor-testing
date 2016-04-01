data.credit=read.csv("data.csv", header =T,na.strings ="?")
names(data.credit)
library (ISLR)
library(plsdof)
data.credit = na.omit(data.credit)
library(pls)

x = model.matrix(expensive~., data.credit)[,-1]
y = data.credit$expensive

set.seed(1)
train=sample(c(TRUE, FALSE), nrow(data.credit), rep=TRUE)
test=(!train)

set.seed(1)
pcr.fit=pcr(expensive~., data=data.credit, subset=train, scale=TRUE, validation="CV")
#validationplot(pcr.fit, val.type="MSEP")
res = MSEP(pcr.fit)
pcr.best=which.min(res$val[1,,])-1
cv = res$val[,,pcr.best+1]
print(pcr.best)
print(cv)

pcr.pred=predict(pcr.fit, x[test,], ncomp=pcr.best)
