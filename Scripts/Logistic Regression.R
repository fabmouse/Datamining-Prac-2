######### Logistic Regression ##########

######## Part One: Using Logistic Regression for the aim(i) ########
# Part one, predict for the aim(i) (belonging parties)
# Load dataset
voteData <- read.csv("data/Clean Vote Data.csv", header = T)
head(voteData)
table(voteData$Party)

library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)


# clean data
sapply(voteData, function(x) sum(is.na(x)))
votedata <- na.omit(voteData)

## set train and test dataset
train.rows <- sample(1:nrow(votedata), 0.7*nrow(votedata))
train <- votedata[train.rows,]
test <- votedata[-train.rows,]

## create model, X = Voting data Y = Party, dataset = train
original.lr <- multinom(Party~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8, data = train)
summary(original.lr)
## when the X only include the voting data, the residual deviance = 178.55, AIC = 232.55

## Build the confusion matrix for original.lr by test dataset
prediction <- predict(original.lr, test)
actuals <- test$Party
CMtable <- table(actuals, prediction)
print(CMtable)
# Accuracy and Misclass rate
accuracy.lr <- sum(diag(CMtable))/sum(CMtable)
misclass.lr <- 1-sum(diag(CMtable))/sum(CMtable)
print(accuracy.lr) # Accuracy = 94.48%
print(misclass.lr) # Misclass rate = 5.52%

## if the X includes Voting data remain/leave and percentage data, 
## does the accuracy of logical regression will be changed?

brexitData <- read.csv("data/remainORleave.csv", header = T)
head(brexitData)
table(brexitData$Party)
breixtdata <- na.omit(brexitData)
table(breixtdata$Party)

## set train and test dataset
trainlr.rows <- sample(1:nrow(breixtdata), 0.7*nrow(breixtdata))
trainlr <- breixtdata[train.rows,]
testlr <- breixtdata[-train.rows,]

## create model,X = Voting + Percent(Constituency) Y = Party, dataset = train
model1.lr <- multinom(Party~ Constituency + Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8, data = trainlr)
summary(model1.lr)
## when the X = voting + percentage data (Constituency), the residual deviance = 158.09, AIC = 218.0912

## Build the confusion matrix for model1.lr by testlr dataset
prediction1 <- predict(model1.lr, testlr)
actuals1 <- testlr$Party
CMtable1 <- table(actuals1, prediction1)
print(CMtable1)

# Accuracy and Misclass rate
accuracy.lr <- sum(diag(CMtable1))/sum(CMtable1)
misclass.lr <- 1-sum(diag(CMtable1))/sum(CMtable1)
print(accuracy.lr) # Accuracy = 93.92%
print(misclass.lr) # Misclass rate = 6.07%

### Conlcusion for the aim(i) ###
# when the X only includes the voting data, the performance and general error of model is better.
# the best model of logistic regression for aim(i) is:
aimOne.lr <- multinom(Party~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8, data = train)
print(aimOne.lr)

###########################################################################################

######## Part Two: Using Logistic Regression for the aim(ii) ########

## Check the 'breixtdata' dataset
head(breixtdata)
table(breixtdata$LeaveRemain)

######### Create model2 for aim(ii), X = voting data, Y = remain/leave ########
model2.lr <- multinom(LeaveRemain~ Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8, data = trainlr)
print(model2.lr)
## the Residual Deviance = 411.9407, AIC = 429.9407

## Build the confusion matrix for model2.lr by testlr dataset
prediction2 <- predict(model2.lr, testlr)
actuals2 <- testlr$LeaveRemain
CMtable2 <- table(actuals2, prediction2)
print(CMtable2)

# Accuracy and Misclass rate
accuracy.lr <- sum(diag(CMtable2))/sum(CMtable2)
misclass.lr <- 1-sum(diag(CMtable2))/sum(CMtable2)
print(accuracy.lr) # Accuracy = 77.90%
print(misclass.lr) # Misclass rate = 22.10%
######## when X = voting data, the Accuracy = 77.90%, Misclass rate = 22.10% #######

######### Create model 3 for aim(ii), X = voting + Party data, Y = remain/leave ########
model4.lr <- multinom(LeaveRemain~ Party + Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8, data = trainlr)
print(model4.lr)
## the Residual Deviance = 399.3978, AIC = 423.3978

## Build the confusion matrix for model3.lr by testlr dataset
prediction3 <- predict(model3.lr, testlr)
actuals3 <- testlr$LeaveRemain
CMtable3 <- table(actuals3, prediction3)
print(CMtable3)

# Accuracy and Misclass rate
accuracy.lr <- sum(diag(CMtable3))/sum(CMtable3)
misclass.lr <- 1-sum(diag(CMtable3))/sum(CMtable3)
print(accuracy.lr) # Accuracy = 78.82%
print(misclass.lr) # Misclass rate = 21.17%
######## when X = voting + Party, the Accuracy = 71.82%, Misclass rate = 28.17% #######


######### Create model 4, X = voting + Party data + Percent (Constituency), Y = remain/leave ########
model4.lr <- multinom(LeaveRemain~ Party + Constituency + Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8, data = trainlr)
print(model4.lr)
## the Residual Deviance = 12.08895, AIC = 38.08895

## Build the confusion matrix for model4.lr by testlr dataset
prediction4 <- predict(model4.lr, testlr)
actuals4 <- testlr$LeaveRemain
CMtable4 <- table(actuals4, prediction4)
print(CMtable4)

# Accuracy and Misclass rate
accuracy.lr <- sum(diag(CMtable4))/sum(CMtable4)
misclass.lr <- 1-sum(diag(CMtable4))/sum(CMtable4)
print(accuracy.lr) # Accuracy = 97.34%
print(misclass.lr) # Misclass rate = 2.66%
######## when X = voting + Party + Percent(Constituency), the Accuracy = 98.34%, Misclass rate = 1.66% #######

### Conlcusion for the aim(ii) ###
# when the X = voting + Party + Percent(Constituency), the performance and general error of model is better.
# the best model of logistic regression for aim(ii) is:
aimtwo.lr <- multinom(LeaveRemain~ Party + Constituency + Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8, data = trainlr)
print(aimtwo.lr)
########X = voting + Party + Percent(Constituency), the Accuracy = 98.34%, Misclass rate = 1.66% #######

###########################################################################################

reg.lr <- multinom(Constituency~ Party + LeaveRemain + Voting.1 + Voting.2 + Voting.3 + Voting.4 + Voting.5 + Voting.6 + Voting.7 + Voting.8, data = trainlr)
print(reg.lr)










