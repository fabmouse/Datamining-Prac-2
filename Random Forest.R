## Random Forest
voteData <- read.csv("data/Clean Vote Data.csv", header = T)
head(voteData)
table(voteData$Party)
library(plyr)
library(randomForest)
source("RFFunction.R")

## Create the forest
set.seed(100)
# Trying without validation ----------------------------------------------------
#ON ALL DATA
#Fit the Random Forest
vote.rf = randomForest(Party~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8,data = voteData)
##View the forest results
print(vote.rf)
round(importance(vote.rf),2)

## set train and test dataset
train.rows <- sample(1:nrow(voteData), 0.7*nrow(voteData))
train <- voteData[train.rows,]
holdout <- voteData[-train.rows,]
fit.rf <- randomForest(factor(Party)~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8,data = train)
print(fit.rf)


## try to find a good mtry number
n <- length(names(train))
set.seed(100)
for (i in 1:8) {
  mtry_fit <- randomForest(factor(Party)~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8,data = train,mtry = i)
  err <- mean(mtry_fit$err.rate)
  print(err)
}
## when mtry = 4, the error is minimum

## try to find a good tree number
set.seed(100)
fit.rf <- randomForest(factor(Party)~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8,data = train,mtry = 4,ntree = 1000)
plot(fit.rf)
## when number of trees is around 300 to 400, the model is steable

## when ntree = 350, mtry = 4
set.seed(100)
select.rf <- randomForest(factor(Party)~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8,data = train,mtry = 4,ntree = 350)
print(select.rf)
## when ntree = 350, the OOB err is 3.09%

## check the accuracy and misclass rate
randomForestTest(select.rf, holdout)
## misclass.rate = 0.0441989, accuracy = 0.9558011

importance(select.rf)
varImpPlot(select.rf)

## Try to find the better tree number in the ranger [300:600] by calculate accuracy
j <- seq(100, 600, by = 50)
for (i in 1:7) {
  numTree = (250 + 50*i)
  
  findnumber.rf <- randomForest(factor(Party)~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8,data = train,mtry = 5,ntree = numTree)
  
  randomForestAccuracy(findnumber.rf, holdout)
  print(numTree)
}

# try to find the good number of tree, when mtry = 5
findgoodnumber(200,600,50,4,voteData)
# when mtry = 4, ntrr = 450, the accuracy is 0.9723757, the misclass.rate is 0.2762431


    
k=5
cvlist <- CVgroup(k, voteData,seed = 100)
pred <- data.frame()

j <- seq(400, 600, by = 50)  # J the number of tree  
i <- 1:k                     # K-fold  
i <- rep(i, times = length(j))  
j <- rep(j, each = k)      #多少折，each多少  
x <- cbind(i, j)  



cvtest <- function(i, j) {  
  train <- voteData[-cvlist[[i]],]  
  test <- voteData[cvlist[[i]],]  
  
  model <- randomForest(Party~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8, data = train, ntree = j)  
  prediction <- predict(model, subset(test, select = - Party))  
  
  temp <- data.frame(cbind(subset(test, select = Party), prediction))  
}  
system.time(pred <- mdply(x, cvtest))

accuracy <- vector()
for (i in 1:4) {
  if(i == 1){
    conf_mat <- table(pred[1:602, 3:4])
    accuracy[i] <- sum(diag(conf_mat))/(sum(conf_mat))
    print(i)
    print(accuracy[i])
  }
  if(i != 1){
    row.s = ((i-1)*602+1)
    row.e = i*602
    conf_mat <- table(pred[row.s:row.e, 3:4])
    accuracy[i] <- sum(diag(conf_mat))/(sum(conf_mat))
    print(i)
    print(accuracy[i])
  }
}
print(accuracy)











