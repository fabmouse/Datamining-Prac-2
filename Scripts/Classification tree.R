#CLASSIFICATION TREE

data.vote <- read.csv("data/remainORleave.csv", header = T)
head(data.vote)


# Set up the datasets -----------------------------------------------------
#Split dataset into trainging and testing
set.seed(123)
ind <- sample(1:nrow(data.vote), 0.75 * nrow(data.vote), replace = FALSE) 
data.train <- data.vote[ind, -c(2:5)] # training dataset to choose best model
data.validation <- data.vote[-ind, -c(2:5)]

library(tree)
library(ISLR)


#decision tree fit
tree.voting <- tree(Party ~ ., data = data.vote)

#plot of decision tree
plot(tree.voting)
text(tree.voting, pretty = 0)

#training the decision tree, using 500 rows of the data set, might change the training size
set.seed(22)
train <- sample(1:nrow(data.vote), 500)
data.vote.test <- data.vote[train,]
Party.test <- Party[train]
tree.voting <- tree(Party ~ ., data = data.vote, subset = train)
tree.pred <- predict(tree.voting, data.vote.test, type = "class")

#classification tree. unsure about the results
table(tree.pred, Party.test)
(40+43+5+8)
  
set.seed(23)
train <- sample(1:nrow(data.vote), 100)
data.vote.test <- data.vote[-train,]
Party.test <- Party[-train]
tree.voting <- tree(Party ~ ., data = data.vote, subset = train)
tree.pred <- predict(tree.voting, data.vote.test, type = "class")

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

tree.pred2 <- predict(prune.voting, data.vote.test, type = "class")
table(tree.pred2, Party.test)


tree.pred2
with(data.vote[train,], table(tree.pred2, Party.test))

