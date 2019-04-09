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
vote.rf = randomForest(Party~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6+Vote.7+Vote.8,data = voteData)
##View the forest results
print(vote.rf)
round(importance(vote.rf),2)

## set train and test dataset
train.rows <- sample(1:nrow(voteData), 0.7*nrow(voteData))
train <- voteData[train.rows,]
holdout <- voteData[-train.rows,]
fit.rf <- randomForest(factor(Party)~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6+Vote.7+Vote.8,data = train)
print(fit.rf)



## try to find a good mtry number
n <- length(names(train[,4:11]))
findgoodmtry(voteData[,4:11], voteData[,2],n,100,holdout)
## when mtry = 4, the error is minimum

## try to find a good tree number
set.seed(100)
fit.rf <- randomForest(factor(Party)~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8,data = train,mtry = 4,ntree = 1000)
plot(fit.rf)
## when number of trees is around 300 to 400, the model is steable

## set ntree = 350  mtry = 4, to get the basic information about these paramter
set.seed(100)
select.rf <- randomForest(factor(Party)~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8,data = train,mtry = 4,ntree = 350)
print(select.rf)
## when ntree = 350, the OOB err is 3.33%

## check the accuracy and misclass rate, when ntree = 350  mtry = 4
randomForestTest(select.rf, holdout)
## misclass.rate = 0.03314917, accuracy = 0.9668508

# check the importance and draw the error lines
importance(select.rf)
varImpPlot(select.rf)
plot(select.rf)


# try to find the good number of tree, when mtry = 4
Y <- voteData$Party
X = voteData$Vote.1+voteData$Vote.2+voteData$Vote.3+voteData$Vote.4+voteData$Vote.5+voteData$Vote.6+voteData$Vote.7+voteData$Vote.8
findgoodntree(voteData[,4:11], voteData[,2],300,550,20,4,voteData)
goodmtry(voteData[,4:11], voteData[,2],ntreeTry=500)
# when mtry = 4, ntrr = 450, the accuracy is 0.9723757, the misclass.rate is 0.2762431


# get the prediction and actual combined table, by 5-fold    
k=5
# get the random 5 lists by split the voteData
cvlist <- CVgroup(k, voteData,seed = 100)

j <- seq(400, 600, by = 50)  # J the number of tree  
i <- 1:k                     # K-fold  
i <- rep(i, times = length(j))  
j <- rep(j, each = k)      #多少折，each多少  
x <- cbind(i, j) 
pred <- data.frame()

cvtest <- function(i, j) {  
  train <- voteData[-cvlist[[i]],]  
  test <- voteData[cvlist[[i]],]  
  
  model <- randomForest(Party~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8, data = train, ntree = j)  
  prediction <- predict(model, subset(test, select = - Party))  
  
  temp <- data.frame(cbind(subset(test, select = Party), prediction))  
}  
# get the table of prediction and actual
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




############# test functions #############

# create the frame to record data
predandactualtable <- data.frame()

calculateTable <- function(k, start,end,by,mtryNumber, dataset, list) {  
  
  ntreeNumber <- seq(start, end, by)  # J the number of tree
  print(ntreeNumber)
  kFold <- 1:k   # K-fold
  print(kFold)
  kFold <- rep(kFold, times = length(ntreeNumber))  
  train <- dataset[-list[[kFold]],]  
  test <- dataset[list[[kFold]],] 
  print(kFold)
  ntreeNumber <- rep(ntreeNumber, each = k)
  print(ntreeNumber)
  print(dataset)
  print(list)
  
  
  
  model <- randomForest(Party~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8, data = train,mtry = mtryNumber, ntree = ntreeNumber)  
  prediction <- predict(model, subset(test, select = - Party))  
  
  temp <- data.frame(cbind(subset(test, select = Party), prediction)) 
} 
calculateTable(5,300,500,50,4,voteData,cvlist)

tablelist <- function(k, start,end,by) {
  ntreeNumber <- seq(start, end, by = by)  # J the number of tree  
  kFold <- 1:k                     # K-fold  
  kFold <- rep(kFold, times = length(ntreeNumber))  
  ntreeNumber <- rep(ntreeNumber, each = k)
  x <- cbind(kFold, ntreeNumber) 
}
tablelist(5,300,500,50)
calculateTable(5,300,500,50,4,voteData)
system.time(predandactualtable <- mdply(tablelist(5,300,500,50), calculateTable(5,300,500,50,4,voteData)))



