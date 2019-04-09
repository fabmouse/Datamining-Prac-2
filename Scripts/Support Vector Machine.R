#### support vector machines for multiple classes 

library(e1071)
data.vote <- read.csv("data/Clean Vote Data.csv", header=T)
head(data.vote)

set.seed(123)

svmfit <- svm(factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + 
                Vote.5 + Vote.6 + Vote.7 + Vote.8, data = data.vote,
                kernel = "radial", cost = 10, gamma = 1)
summary(svmfit)
plot(svmfit, data.vote)

?predict.svm()


######

set.seed(1)
x=rbind(50, matrix(rnorm(50*2), ncol=2))
y=c(50, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))
svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)
