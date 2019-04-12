#Team Members
# Lei
# Jose
#Brooke :) 


voteData <- read.csv("Data/Clean Vote Data.csv")
View(voteData)
attach(voteData)
attach(voting_data)
votedata2 <- read.csv("Data/voting_data.csv")

install.packages("tree")
library(tree)
votetree <- tree(Party ~ Vote.1+Vote.2+Vote.3+Vote.4+Vote.5+Vote.6+Vote.7+Vote.8, data = voteData)
tree(Party ~ Vote.1, Vote.2, Vote.3 + Vote.4 + Vote.5 +
       Vote.6 + Vote.7 + Vote.8, data = voteData)
install.packages("rpart")
library(rpart)
library(rpart.plot)

decisiontree1 <- rpart(as.factor(Party) ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 + Vote.5 + Vote.6 + Vote.7 + Vote.8, data = voteData)
  
decisiontree1
summary(decisiontree1)
plot(decisiontree1)
               
rpart.plot(decisiontree1)
library(rpart.plot)

print(decisiontree1)
summary(decisiontree1)
predict(decisiontree1)


decisiontree2 <- rpart(as.factor(Party) ~ `Voting 1` + `Voting 2` + `Voting 3` + `Voting 4` + `Voting 5` + `Voting 6` + `Voting 7` + `Voting 8`, data = voting_data)
rpart.plot(decisiontree2)

decisiontree2 <- rpart(Party ~ `Voting 1` + `Voting 2` + `Voting 3` + `Voting 4` + `Voting 5` + `Voting 6` + `Voting 7` + `Voting 8`, data = voting_data)
rpart.plot(decisiontree2)
print(decisiontree2)

prediction <- predict(, newdata = voting_data)
actuals <- holdout[,2]
T <- table(actuals, prediction)
accuracy <- sum(diag(T)/Sum(T))


attach(voteData)
tree.voting <- tree(Party ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 +
                      Vote.5 + Vote.6 + Vote.7 + Vote.8, data = voteData)
library(tree)
plot(tree.voting)
text(tree.voting, pretty = 0)

set.seed(100)
train=sample(1:nrow(voteData), 100)
tree.voting1 <- tree(Party ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 +
                       Vote.5 + Vote.6 + Vote.7 + Vote.8, data = voteData, subset = train)
plot(tree.voting1)
text(tree.voting1, pretty = 0)

tree.predict <- predict(tree.voting1, voteData[-train,], type = "class")
with(voteData[-train,], table(tree.predict, Party))
