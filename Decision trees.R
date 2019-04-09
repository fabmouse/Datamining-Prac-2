library(tree)
library(ISLR)
voteData <- read.csv("Data/Clean Vote Data", header = True)
attach(voteData)

tree.voting <- tree(Party ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 +
                      Vote.5 + Vote.6 + Vote.7 + Vote.8, data = voteData)

plot(tree.voting)
text(tree.voting, pretty = 0)

set.seed(20)
train <- sample(1:nrow(voteData), 200)
voteData.test <- voteData[-train,]
Party.test <- Party[-train]
tree.voting <- tree(Party ~ Vote.1 + Vote.2 + Vote.3 + Vote.4 +
                      Vote.5 + Vote.6 + Vote.7 + Vote.8, data = voteData, subset = train)
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

