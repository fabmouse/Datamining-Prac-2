## Random Forest
brexitData <- read.csv("data/remainORleave.csv", header = T)
head(brexitData)
table(brexitData$LeaveRemain)
library(plyr)
library(randomForest)
source("RFFunction.R")

## Create the forest
set.seed(100)
# Trying without validation ----------------------------------------------------
#ON ALL DATA
#Fit the Random Forest
original.rf = randomForest(factor(LeaveRemain)~ Voting.1+Voting.2+Voting.3+Voting.4+Voting.5+Voting.6+Voting.7+Voting.8,data = brexitData)
##View the forest results
print(original.rf)
round(importance(original.rf),2)
