## Random Forest for aim(i)

# Load dataset
voteData <- read.csv("data/Clean Vote Data.csv", header = T)
head(voteData)
table(voteData$Party)
# Load libraries
library(plyr)
library(randomForest)
source("Scripts/RFFunction.R")


## Create the forest
set.seed(100)
# Trying without validation ----------------------------------------------------
#ON ALL DATA
#Fit the Random Forest
vote.rf = randomForest(Party~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8, data = voteData)
##View the forest results
print(vote.rf)
round(importance(vote.rf),2)

## set train and test dataset
train.rows <- sample(1:nrow(voteData), 0.7*nrow(voteData))
train <- voteData[train.rows,]
holdout <- voteData[-train.rows,]
fit.rf <- randomForest(factor(Party)~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8,data = train)
print(fit.rf)


## try to find a good mtry number
n <- length(names(train[,4:11]))
findgoodmtry(train[,4:11], train[,1],n,100,holdout)
## when mtry = 4, the error is minimum

## try to find a good tree number
set.seed(100)
fit.rf <- randomForest(factor(Party)~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8,data = train,mtry = 4,ntree = 1000)
plot(fit.rf)
## when number of trees is around 300 to 400, the model is steable

## set ntree = 350  mtry = 4, to get the basic information about these paramter
set.seed(100)
select.rf <- randomForest(factor(Party)~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8,data = train,mtry = 4,ntree = 350)
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
findgoodntree(voteData[,4:11], voteData[,1],300,550,20,4,voteData)
findgoodntree(voteData[,4:11], voteData[,1],400,550,10,4,voteData)
# when mtry = 4, ntrr = 470, the accuracy is 0.9723757, the misclass.rate is 0.2762431

# when mtry = 4, ntrr = 470, the model is:
modelone.rf <- randomForest(factor(Party)~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8,data = train,mtry = 4,ntree = 470)
importance(modelone.rf)
varImpPlot(modelone.rf)
plot(modelone.rf)



