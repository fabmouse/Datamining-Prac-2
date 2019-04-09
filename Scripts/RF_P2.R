## Random Forest
brexitData <- read.csv("data/Final Downloaded Data.csv", header = T)
head(brexitData)
table(brexitData$Constituency)
library(plyr)
library(randomForest)
source("RFFunction.R")

## Create the forest
set.seed(100)
# Trying without validation ----------------------------------------------------
#ON ALL DATA
#Fit the Random Forest
original.rf = randomForest(factor(Constituency)~ Voting.1+Voting.2+Voting.3+Voting.4+Voting.5+Voting.6+Voting.7+Voting.8,data = brexitData)
##View the forest results
print(original.rf)
round(importance(original.rf),2)
