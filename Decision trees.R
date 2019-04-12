library(tree)
library(ISLR)
voteData <- read.csv("data/Clean Vote Data.csv", header=T)
attach(voteData)


tree.voting <- tree(Party ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                      Voting.5 + Voting.6 + Voting.7 + Voting.8, data = voteData)

plot(tree.voting)
text(tree.voting, pretty = 0)

set.seed(20)
train <- sample(1:nrow(voteData), 450)
voteData.test <- voteData[-train,]
Party.test <- Party[-train]
tree.voting <- tree(Party ~ Voting.1 + Voting.2 + Voting.3 + Voting.4 +
                      Voting.5 + Voting.6 + Voting.7 + Voting.8, data = voteData, subset = train)
tree.pred <- predict(tree.voting, voteData.test, type = "class")
table(tree.pred, Party.test)
(169+153+12+25)/(169+1+16+3+1+153+1+6+0+3+12+12+0+0+0+25)#0.893

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

