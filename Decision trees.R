library(tree)
library(ISLR)

#import data, might change data set
voteData <- read.csv("data/Clean Vote Data.csv", header=T)
attach(voteData)

#decision tree fit
tree.voting <- tree(Party ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                      Voting.5 + Voting.6 + Voting.7 + Voting.8, data = voteData)

#plot of decision tree
plot(tree.voting)
text(tree.voting, pretty = 0)

#training the decision tree, using 500 rows of the data set, might change the training size
set.seed(22)
train <- sample(1:nrow(voteData), 500)
voteData.test <- voteData[train,]
Party.test <- Party[train]
tree.voting <- tree(Party ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                      Voting.5 + Voting.6 + Voting.7 + Voting.8, data = voteData, subset = train)
tree.pred <- predict(tree.voting, voteData.test, type = "class")

#classification tree. unsure about the results
table(tree.pred, Party.test)
(40+43+5+8)
  
set.seed(23)
train <- sample(1:nrow(voteData), 100)
voteData.test <- voteData[-train,]
Party.test <- Party[-train]
tree.voting <- tree(Party ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                      Voting.5 + Voting.6 + Voting.7 + Voting.8, data = voteData, subset = train)
tree.pred <- predict(tree.voting, voteData.test, type = "class")

#classification tree. unsure about the results
table(tree.pred, Party.test)

set.seed(21)

cv.voting <- cv.tree(tree.voting, FUN = prune.misclass)
names(cv.voting)
cv.voting
plot(cv.voting$size, cv.voting$dev, type = "b")
plot(cv.voting$k, cv.voting$dev, type = "b")
prune.voting <- prune.misclass(tree.voting, best = 7)
prune.voting

plot(prune.voting)
text(prune.voting, pretty = 0)

tree.pred2 <- predict(prune.voting, voteData.test, type = "class")
table(tree.pred2, Party.test)


tree.pred2
with(voteData[train,], table(tree.pred2, Party.test))

