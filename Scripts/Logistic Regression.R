######### Logistic Regression ##########

# Part one, predict for the parties
# Load dataset
voteData <- read.csv("data/Clean Vote Data.csv", header = T)
head(voteData)
table(voteData$Party)

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

source("RFFunction.R")

# clean data
sapply(voteData, function(x) sum(is.na(x)))


## set train and test dataset
train.rows <- sample(1:nrow(voteData), 0.7*nrow(voteData))
train <- voteData[train.rows,]
test <- voteData[-train.rows,]

## create model of logistic regression, dataset = train
model.lr <- glm(Party~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6++Vote.7+Vote.8,data = train)
## get some error that the 'glm' cannot compute the '-' data








