## Random Forest
# load dataset which include remain or leave factor, this dataset will be used for classcifacation random forest
brexitData <- read.csv("data/remainORleave.csv", header = T)
head(brexitData)
table(brexitData$LeaveRemain)

# load dataset which use numbers to expresee remain or leave meaning, and this data will be used for regression random forest
beData <- read.csv("data/Final Downloaded Data.csv", header = T)
head(beData)
table(beData)



library(plyr)
library(randomForest)
source("RFFunction.R")

## Create the forest
set.seed(100)
# Trying without validation ----------------------------------------------------
#ON ALL DATA
#Fit the Random Forest
original.rf = randomForest(factor(LeaveRemain)~ Voting.1+Voting.2+Voting.3+Voting.4+Voting.5+Voting.6+Voting.7+Voting.8,data = brexitData, na.action = na.omit)
##View the forest results
print(original.rf)
round(importance(original.rf),2)
plot(original.rf)

## set train and test dataset
train.rows <- sample(1:nrow(brexitData), 0.7*nrow(brexitData))
train <- brexitData[train.rows,]
test <- brexitData[-train.rows,]








